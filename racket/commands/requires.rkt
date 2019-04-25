#lang at-exp racket/base

(require (only-in macro-debugger/analysis/check-requires show-requires)
         racket/contract
         racket/format
         racket/function
         (only-in racket/list append* append-map add-between filter-map)
         racket/match
         racket/set
         racket/string)

(provide requires/tidy
         requires/trim
         requires/base)

(module+ test
  (require rackunit))

(define require-subform? (or/c module-path? list?))
(define require-form? (cons/c 'require (listof require-subform?)))

(define level? (or/c #f number? 'racket/require))
(define denormalized? (hash/c level? (set/c require-subform?)))


(define/contract (requires/tidy reqs)
  (-> (listof require-form?) string?)
  (require-pretty-format
   (normalize
    (denormalize reqs))))

(module+ test
  (check-equal?
   (requires/tidy '((require z)
                    (require (prefix-in a: a))
                    (require c d e)))
   "(require (prefix-in a: a)\n         c\n         d\n         e\n         z)\n"))

;; Note: Why pass in a list of the existing require forms -- why not
;; just use the "keep" list from show-requires? Because the keep list
;; only states the module name, not the original form. Therefore if
;; the original require has a subform like `(only-in mod f)` (or
;; rename-in, except-in, &c), we won't know how to preserve that
;; unless we're given it. That's why our strategy must be to look for
;; things to drop, as opposed to things to keep.
(define/contract (requires/trim path-str reqs)
  (-> path-string? (listof require-form?) string?)
  (require-pretty-format
   (normalize
    (denormalize reqs
                 #:drops (requires-to-drop (analyze path-str))))))

(define/contract (requires/base path-str reqs)
  (-> path-string? (listof require-form?) string?)
  (define a (analyze path-str))
  (require-pretty-format
   (normalize
    (denormalize reqs
                 #:adds  (requires-to-add a)
                 #:drops (requires-to-drop a)))))

;;; analyze

(define requires-analysis? (listof (or/c (list/c 'keep   module-path? number?)
                                         (list/c 'bypass module-path? number? list?)
                                         (list/c 'drop   module-path? number?))))
(define mod+level? (list/c module-path? number?))

(define/contract (analyze path-str)
  (-> path-string? requires-analysis?)
  (define-values (base name _) (split-path (string->path path-str)))
  (parameterize ([current-load-relative-directory base]
                 [current-directory base])
    (show-requires name)))

;; Use `bypass` convert from `#lang racket` to `#lang racket/base`
;; plus explicit requires. Hardcoded to `#lang racket`, only.
(define/contract (requires-to-drop a)
  (-> requires-analysis? (listof mod+level?))
  (filter-map (λ (x)
                (match x
                  [(list 'drop mod lvl) (list mod lvl)]
                  [_ #f]))
              a))

(define/contract (requires-to-add a)
  (-> requires-analysis? (listof mod+level?))
  (append*
   (filter-map (λ (x)
                 (match x
                   [(list 'bypass 'racket 0 (list (list mod lvl _) ...))
                    (filter (λ (x)
                              (match x
                                [(list 'racket/base 0) #f]
                                [_ #t]))
                            (map list mod lvl))]
                   [_ #f]))
               a)))

;;; denormalize / normalize

(define/contract (denormalize reqs
                              #:drops [drops '()]
                              #:adds  [adds  '()])
  (->* ((listof require-form?))
       (#:adds  (listof mod+level?)
        #:drops (listof mod+level?))
       denormalized?)
  (define ht (make-hasheq))
  (define (add* level v)
    (unless (and (not (eq? v 'racket/require)) ;always keep
                 (member (list (form-mod v) level) drops))
      (hash-update! ht
                    (if (eq? v 'racket/require) 'racket/require level)
                    (λ (s) (set-add s v))
                    set)))
  (define (add level v)
    (match v
      [(list* 'multi-in vs) (for-each (curry add* level) (multi vs))]
      [v                    (add* level v)]))
  (for ([add (in-list adds)])
    (match-define (list mod level) add)
    (add* level mod))
  (for ([req (in-list reqs)])
    (match-define (cons 'require vs) req)
    (for ([v (in-list vs)])
      (match v
        [(list* 'for-meta level vs) (for-each (curry add level) vs)]
        [(list* 'for-syntax vs)     (for-each (curry add 1    ) vs)]
        [(list* 'for-template vs)   (for-each (curry add -1   ) vs)]
        [(list* 'for-label vs)      (for-each (curry add #f   ) vs)]
        [v                          (add 0 v)])))
  ht)

;; `multi` from racket/require adapted for plain sexprs not stxs
(define (multi xs)
  (define (loop xs)
    (if (null? xs)
      '(())
      (let ([first (car xs)]
            [rest (loop (cdr xs))])
        (if (list? first)
          (let ([bads (filter list? first)])
            (if (null? bads)
              (append-map (λ (x) (map (λ (y) (cons x y)) rest)) first)
              (error 'multi-in "not a simple element" (car bads))))
          (map (λ (x) (cons first x)) rest)))))
  (define options (loop xs))
  (define (try pred? ->str str->)
    (and (andmap (λ (x) (andmap pred? x)) options)
         (map (λ (x)
                (let* ([d x]
                       [r (apply string-append
                                 (add-between (if ->str (map ->str d) d)
                                              "/"))])
                  (if str-> (str-> r) r)))
              options)))
  (or (try string? #f #f)
      (try symbol? symbol->string string->symbol)
      (error 'multi-in "only accepts all strings or all symbols")))

(module+ test
  (let ([ht (denormalize '((require a b c)
                           (require d e)
                           (require a f)
                           (require
                            (for-syntax s t u)
                            (for-label l0 l1 l2))
                           (require
                            (for-meta 1 m1a m1b)
                            (for-meta 2 m2a m2b))
                           (require
                            (multi-in foo (bar baz))
                            (multi-in "foo" ("bar.rkt" "baz.rkt")))))])
    (check-equal? (hash-ref ht 0)
                  (set 'a 'e 'd 'foo/bar "foo/baz.rkt" "foo/bar.rkt" 'c 'f 'b 'foo/baz))
    (check-equal? (hash-ref ht 1)
                  (set 'm1a 'm1b 't 'u 's))
    (check-equal? (hash-ref ht 2)
                  (set 'm2b 'm2a))
    (check-equal? (hash-ref ht #f)
                  (set 'l1 'l2 'l0))))

;; Sort the subforms by phase level: for-syntax, for-template,
;; for-label, for-meta, and plain (0). Within each such group, sort
;; them first by module paths then relative requires. Within each such
;; group, sort alphabetically. If racket/require is present, sort it
;; first and use multi-in.
(define/contract (normalize ht)
  (-> denormalized? require-form?)
  (define (mod-set->mod-list mod-set)
    (sort (set->list mod-set) mod<?))
  (define (for-level level k)
    (match (hash-ref ht level #f)
      [#f '()]
      [mods
       #:when (and (not (eq? level 'racket/require))
                   (hash-ref ht 'racket/require #f))
       (k (add-multi-in (mod-set->mod-list mods)))]
      [mods
       (k (mod-set->mod-list mods))]))
  (define (preface . pres)
    (λ (mods) `((,@pres ,@mods))))
  (define (meta-levels)
    (sort (for/list ([x (hash-keys ht)]
                     #:when (not (member x '(racket/require -1 0 1 #f))))
            x)
          <))
  `(require
    ,@(for-level 'racket/require values)
    ,@(for-level  1 (preface 'for-syntax))
    ,@(for-level -1 (preface 'for-template))
    ,@(for-level #f (preface 'for-label))
    ,@(append* (for/list ([level (in-list (meta-levels))])
                 (for-level level (preface 'for-meta level))))
    ,@(for-level 0 values)))

(module+ test
  ;; with racket/require
  (check-equal? (normalize
                 (denormalize
                  '((require z c b a)
                    (require racket/require) ; <====
                    (require (multi-in mi-z (mi-z0 mi-z1)))
                    (require mi-z/mi-z2)
                    (require (multi-in mi-a (mi-a1 mi-a0)))
                    (require mi-a/mi-a2)
                    (require (for-meta 4 m41 m40))
                    (require (for-meta -4 m-41 m-40))
                    (require (for-label l1 l0))
                    (require (for-template t1 t0))
                    (require (for-syntax s1 s0))
                    (require
                     "a.rkt" "b.rkt" "c.rkt" "z.rkt"
                     (only-in "mod.rkt" oi)
                     (only-in mod oi)))))
                '(require
                  racket/require
                  (for-syntax s0 s1)
                  (for-template t0 t1)
                  (for-label l0 l1)
                  (for-meta -4 m-40 m-41)
                  (for-meta 4 m40 m41)
                  a b c
                  (multi-in mi-a (mi-a0 mi-a1 mi-a2)) ;b/c racket/require
                  (multi-in mi-z (mi-z0 mi-z1 mi-z2)) ;b/c racket/require
                  (only-in mod oi) z
                  "a.rkt" "b.rkt" "c.rkt" (only-in "mod.rkt" oi) "z.rkt"))
  ;; without racket/require
  (check-equal? (normalize
                 (denormalize
                  '((require z c b a)
                    (require mi-a/mi-a0)
                    (require mi-a/mi-a1)
                    (require mi-z/mi-z0)
                    (require mi-z/mi-z1)
                    (require (for-meta 4 m41 m40))
                    (require (for-meta -4 m-41 m-40))
                    (require (for-label l1 l0))
                    (require (for-template t1 t0))
                    (require (for-syntax s1 s0))
                    (require
                     "a.rkt" "b.rkt" "c.rkt" "z.rkt"
                     (only-in "mod.rkt" oi)
                     (only-in mod oi)))))
                '(require
                  (for-syntax s0 s1)
                  (for-template t0 t1)
                  (for-label l0 l1)
                  (for-meta -4 m-40 m-41)
                  (for-meta 4 m40 m41)
                  a b c
                  mi-a/mi-a0
                  mi-a/mi-a1
                  mi-z/mi-z0
                  mi-z/mi-z1
                  (only-in mod oi) z
                  "a.rkt" "b.rkt" "c.rkt" (only-in "mod.rkt" oi) "z.rkt")))

(define (add-multi-in xs)
  ;; (-> (listof require-subform?) (listof require-subform?))
  ;; 1. Assumes xs are sorted. 2. Only tries to discover/add multi-in
  ;; forms where the first element is a single item -- e.g. (multi-in
  ;; a (b c)) but not (multi-in (a b) (c d)).
  (define (split v)
    (cond [(string? v) (string-split v #px"/")]
          [(symbol? v) (map string->symbol (string-split (symbol->string v) #px"/"))]
          [else (list)]))
  (define (join vs)
    (cond [(andmap string? vs) (string-join vs "/")]
          [(andmap symbol? vs) (string->symbol (string-join (map symbol->string vs) "/"))]
          [else (error 'add-multi-in "not strings or symbols")]))
  (let loop ([xs xs])
    (match xs
      [(list) (list)]
      [(list x) (list x)]
      [(list* (and `(multi-in ,pre ,vs) this) next more)
       (define-values (pres this-rest next-rest) (split-common-prefix (split pre) (split next)))
       (cond [(equal? (split pre) pres)
              (loop (list* `(multi-in ,pre ,(append vs (list (join next-rest))))
                           more))]
             [else
              (cons this (loop (list* next more)))])]
      [(list* this next more)
       (define-values (pres this-rest next-rest) (split-common-prefix (split this) (split next)))
       (cond [(null? pres)
              (cons this (loop (list* next more)))]
             [else
              (loop (list* `(multi-in ,(join pres) (,(join this-rest) ,(join next-rest)))
                           more))])])))

(module+ test
  (check-equal? (add-multi-in '(a b c))
                '(a b c))
  (check-equal? (add-multi-in '(a (prefix-in b: b) c))
                '(a (prefix-in b: b) c))
  (check-equal? (add-multi-in '(racket/string b c))
                '(racket/string b c))
  (check-equal? (add-multi-in '(racket/format racket/string s t))
                '((multi-in racket (format string)) s t))
  (check-equal? (add-multi-in '(racket/contract racket/format racket/string s t))
                '((multi-in racket (contract format string)) s t))
  (check-equal? (add-multi-in '(a/b/x a/b/y))
                '((multi-in a/b (x y))))
  (check-equal? (add-multi-in '("a.rkt" "b.rkt" "c.rkt"))
                '("a.rkt" "b.rkt" "c.rkt"))
  (check-equal? (add-multi-in '("a.rkt" (prefix-in b: "b.rkt") "c.rkt"))
                '("a.rkt" (prefix-in b: "b.rkt") "c.rkt"))
  (check-equal? (add-multi-in '("a/x.rkt" "b.rkt" "c.rkt"))
                '("a/x.rkt" "b.rkt" "c.rkt"))
  (check-equal? (add-multi-in '("a/x.rkt" "a/y.rkt" "b.rkt" "c.rkt"))
                '((multi-in "a" ("x.rkt" "y.rkt")) "b.rkt" "c.rkt"))
  (check-equal? (add-multi-in '("a/x.rkt" "a/y.rkt" "a/z.rkt" "b.rkt" "c.rkt"))
                '((multi-in "a" ("x.rkt" "y.rkt" "z.rkt")) "b.rkt" "c.rkt"))
  (check-equal? (add-multi-in '("a/b/x.rkt" "a/b/y.rkt"))
                '((multi-in "a/b" ("x.rkt" "y.rkt")))))

;; Defined here b/c not in Racket < 6.3 and we support 6.2
(define (split-common-prefix as bs)
  (let loop ([as as] [bs bs])
    (if (and (pair? as) (pair? bs) (equal? (car as) (car bs)))
        (let-values ([(prefix atail btail) (loop (cdr as) (cdr bs))])
          (values (cons (car as) prefix) atail btail))
        (values null as bs))))

(define (form-mod x)
    (match x
      [(list 'only-in   m _ ...)     (form-mod m)]
      [(list 'except-in m _ ...)     (form-mod m)]
      [(list 'prefix-in _ m)         (form-mod m)]
      [(list 'relative-in _ m _ ...) (form-mod m)]
      [(list 'multi-in m _)          (form-mod m)]
      [m                             m]))

(define (mod<? a b)
  (let ([a (form-mod a)]
        [b (form-mod b)])
    (or (and (symbol? a) (not (symbol? b)))
        (and (list? a) (not (list? b)))
        (and (not (string? a)) (string? a))
        (and (string? a) (string? b)
             (string<? a b))
        (and (symbol? a) (symbol? b)
             (string<? (symbol->string a) (symbol->string b))))))

(module+ test
  (check-true (mod<? 'a 'b))
  (check-false (mod<? 'b 'a))
  (check-true (mod<? 'a '(only-in b)))
  (check-true (mod<? '(only-in a) 'b))
  (check-true (mod<? 'a '(except-in b)))
  (check-true (mod<? '(except-in a) 'b))
  (check-true (mod<? 'a '(prefix-in p 'b)))
  (check-true (mod<? '(prefix-in p 'a) 'b))
  (check-true (mod<? 'a '(relative-in p 'b)))
  (check-true (mod<? '(relative-in p 'a) 'b))
  (check-true (mod<? 'a '(prefix-in p (only-in b))))
  (check-true (mod<? '(prefix-in p (only-in a)) 'b)))

;;; pretty

(define/contract (require-pretty-format x)
  (-> list? string?)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (require-pretty-print x))
  (get-output-string out))

(module+ test
  (check-equal? (require-pretty-format
                 '(require a))
                @~a{(require a)

                    })
  (check-equal? (require-pretty-format
                 '(require a b))
                @~a{(require a
                             b)

                    })
  (check-equal? (require-pretty-format
                 '(require (for-syntax a b) (for-meta 2 c d) e f))
                @~a{(require (for-syntax a
                                         b)
                             (for-meta 2 c
                                         d)
                             e
                             f)

                    })
  (check-equal? (require-pretty-format
                 `(require (only-in m a b) (except-in m a b)))
                @~a{(require (only-in m
                                      a
                                      b)
                             (except-in m
                                        a
                                        b))

                    }))

;; Pretty print a require form with one module per line and with
;; indentation for the `for-X` subforms. Example:
;;
;; (require (for-syntax racket/base
;;                      syntax/parse)
;;          (for-meta 3 racket/a
;;                      racket/b)
;;          racket/format
;;          racket/string
;;          "a.rkt"
;;          "b.rkt")
(define/contract (require-pretty-print x)
  (-> list? any)
  (define (prn x first? indent)
    (define (indent-string)
      (if first? "" (make-string indent #\space)))
    (define (prn-form pre this more)
      (define new-indent (+ indent (+ 2 (string-length pre))))
      (printf "~a(~a " (indent-string) pre)
      (prn this #t new-indent)
      (for ([x more])
        (newline)
        (prn x #f new-indent))
      (display ")"))
    (match x
      [(list 'require)
       (void)]
      [(list* (and pre (or 'require 'for-syntax 'for-template 'for-label
                           'only-in 'except-in))
              this more)
       (prn-form (format "~s" pre) this more)
       (when (eq? pre 'require)
         (newline))]
      [(list* 'for-meta level this more)
       (prn-form (format "for-meta ~a" level) this more)]
      [this
       (printf "~a~s" (indent-string) this)]))
  (prn x #t 0))

#lang racket/base

(require macro-debugger/analysis/check-requires
         racket/contract
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/port
         racket/pretty
         racket/set
         racket/string
         syntax/modresolve
         (only-in xml xexpr->string)
         "defn.rkt"
         "logger.rkt"
         "scribble.rkt"
         "try-catch.rkt"
         "util.rkt")

(provide make-prompt-read)

(module+ test
  (require rackunit))

(define (make-prompt-read path put/stop rerun)
  (define-values (base name _) (cond [path (split-path path)]
                                     [else (values (current-directory) "" #f)]))
  (λ ()
    (let ([;; Elisp prints '() as 'nil. Reverse that. (Assumption:
           ;; Although Elisp code puns nil/() also to mean "false",
           ;; _our_ Elisp code _won't_ do that.)
           read (λ () (match (read) ['nil '()] [x x]))])
      (flush-output (current-error-port))
      (display #\u227a) (display name) (display #\u227b) (display #\space)
      (define in ((current-get-interaction-input-port)))
      (define stx ((current-read-interaction) (object-name in) in))
      (syntax-case stx ()
        [(uq cmd)
         (eq? 'unquote (syntax-e #'uq))
         (case (syntax-e #'cmd)
           [(run) (run put/stop rerun)]
           [(top) (top put/stop rerun)]
           [(def) (def (read))]
           [(doc) (doc (read-syntax))]
           [(describe) (describe (read-syntax))]
           [(mod) (mod (read) path)]
           [(type) (type (read))]
           [(exp) (exp1 (read))]
           [(exp+) (exp+)]
           [(exp!) (exp! (read))]
           [(log) (log-display (map string->symbol (string-split (read-line))))]
           [(pwd) (display-commented (~v (current-directory)))]
           [(cd) (cd (~a (read)))]
           [(requires/tidy) (requires/tidy (read))]
           [(requires/trim) (requires/trim (read) (read))]
           [(requires/base) (requires/base (read) (read))]
           [(find-collection) (find-collection (read))]
           [else (usage)])]
        [_ stx]))))

(define (usage)
  (displayln
   "Commands:
,run </path/to/file.rkt> [<memory-limit-MB> [<pretty-print?>]]
,top [<memory-limit-MB>]
,def <identifier>
,type <identifier>
,doc <identifier>|<string>
,exp <stx>
,exp+
,exp! <stx>
,pwd
,cd <path>
,log <opts> ...")
  (void))

;;; run and top

;; Parameter-like interface, but we don't care about thread-local
;; stuff. We do care about calling collect-garbage IFF the new limit
;; is less than the old one or less than the current actual usage.
(define current-mem
  (let ([old #f])
    (case-lambda
      [() old]
      [(new)
       (and old new
            (or (< new old)
                (< (* new 1024 1024) (current-memory-use)))
            (collect-garbage))
       (set! old new)])))

(define current-pp? (make-parameter #t))

(define (run put/stop rerun)
  ;; Note: Use ~a on path to allow both `,run "/path/file.rkt"` and
  ;; `run /path/file.rkt`.
  (define (go path)
    (put/stop (rerun (~a path) (current-mem) (current-pp?))))
  (match (read-line->reads)
    [(list path mem pp?) (cond [(and (number? mem) (boolean? pp?))
                                (current-mem mem)
                                (current-pp? pp?)
                                (go path)]
                               [else (usage)])]
    [(list path mem)     (cond [(number? mem)
                                (current-mem mem)
                                (go path)]
                               [else (usage)])]
    [(list path)         (go path)]
    [_                   (usage)]))

(define (top put/stop rerun)
  (define (go)
    (put/stop (rerun #f (current-mem) (current-pp?))))
  (match (read-line->reads)
    [(list mem pp?) (cond [(and (number? mem) (boolean? pp?))
                           (current-mem mem)
                           (current-pp? pp?)
                           (go)]
                          [else (usage)])]
    [(list mem)     (cond [(number? mem)
                           (current-mem mem)
                           (go)]
                          [else (usage)])]
    [(list)         (go)]
    [_              (usage)]))

(define (read-line->reads)
  (reads-from-string (read-line)))

(define (reads-from-string s)
  (with-input-from-string s reads))

(define (reads)
  (match (read)
    [(? eof-object?) (list)]
    ['t              (cons #t (reads))] ;in case from elisp
    ['nil            (cons #f (reads))] ;in case from elisp
    [x               (cons x (reads))]))

;;; misc other commands

(define (def sym)
  (elisp-println (find-definition (symbol->string sym))))

(define (mod v rel)
  (define (mod* mod rel)
    (define path (with-handlers ([exn:fail? (λ _ #f)])
                   (resolve-module-path mod rel)))
    (and path
         (file-exists? path)
         (list (path->string path) 1 0)))
  (elisp-println (cond [(module-path? v) (mod* v rel)]
                       [(symbol? v)      (mod* (symbol->string v) rel)]
                       [else             #f])))

(define (type v) ;; the ,type command.  rename this??
  (elisp-println (type-or-sig v)))

(define (type-or-sig v)
  (or (type-or-contract v)
      (sig v)
      ""))

(define (sig v) ;symbol? -> (or/c #f string?)
  (define x (find-signature (symbol->string v)))
  (cond [x (~a x)]
        [else #f]))

(define (type-or-contract v) ;any/c -> (or/c #f string?)
  ;; 1. Try using Typed Racket's REPL simplified type.
  (try (match (with-output-to-string
                  (λ ()
                    ((current-eval)
                     (cons '#%top-interaction v))))
         [(pregexp "^- : (.*) \\.\\.\\..*\n" (list _ t)) t]
         [(pregexp "^- : (.*)\n$"            (list _ t)) t])
       #:catch exn:fail? _
       ;; 2. Try to find a contract.
       (try (parameterize ([error-display-handler (λ _ (void))])
              ((current-eval)
               (cons '#%top-interaction
                     `(if (has-contract? ,v)
                       (~a (contract-name (value-contract ,v)))
                       (error)))))
            #:catch exn:fail? _
            #f)))

(define (sig-and/or-type stx)
  (define dat (syntax->datum stx))
  (define s (or (sig dat) (~a dat)))
  (define t (type-or-contract stx))
  (xexpr->string
   `(div ()
     (p () ,s)
     ,@(if t `((pre () ,t)) `())
     (br ()))))

;;; describe

;; If a symbol has installed documentation, display it.
;;
;; Otherwise, walk the source to find the signature of its definition
;; (because the argument names have explanatory value), and also look
;; for Typed Racket type or a contract, if any.

;; Keep reusing one temporary file.
(define describe-html-path (make-temporary-file "racket-mode-describe-~a"))

(define (describe stx)
  (with-output-to-file describe-html-path #:mode 'text #:exists 'replace
    (λ () (display (describe* stx))))
  (elisp-println (path->string describe-html-path))
  (void)) ;(void) prevents Typed Racket type annotation line

(define (describe* _stx)
  (define stx (namespace-syntax-introduce _stx))
  (or (scribble-doc/html stx)
      (sig-and/or-type stx)))

;;; print elisp values

(define (elisp-println v)
  (elisp-print v)
  (newline))

(define (elisp-print v)
  (print (->elisp v)))

(define (->elisp v)
  (match v
    [(or #f (list)) 'nil]
    [#t             't]
    [(? list? xs)   (map ->elisp xs)]
    [(cons x y)     (cons (->elisp x) (->elisp y))]
    [v              v]))

(module+ test
  (check-equal? (with-output-to-string (λ () (elisp-print '(1 t nil 3))))
                "'(1 t nil 3)"))

;;; misc

(define (doc stx)
  (eval
   (namespace-syntax-introduce
    (datum->syntax #f
                   `(begin
                     (local-require racket/help)
                     (help ,stx))))))

(define (cd s)
  (let ([old-wd (current-directory)])
    (current-directory s)
    (unless (directory-exists? (current-directory))
      (display-commented (format "~v doesn't exist." (current-directory)))
      (current-directory old-wd))
    (display-commented (format "In ~v" (current-directory)))))

;;; syntax expansion

(define last-stx #f)

(define (exp1 stx)
  (set! last-stx (expand-once stx))
  (pp-stx last-stx))

(define (exp+)
  (when last-stx
    (define this-stx (expand-once last-stx))
    (cond [(equal? (syntax->datum last-stx) (syntax->datum this-stx))
           (display-commented "Already fully expanded.")
           (set! last-stx #f)]
          [else
           (pp-stx this-stx)
           (set! last-stx this-stx)])))

(define (exp! stx)
  (set! last-stx #f)
  (pp-stx (expand stx)))

(define (pp-stx stx)
  (newline)
  (pretty-print (syntax->datum stx)
                (current-output-port)
                1))

;;; requires

;; requires/tidy : (listof require-sexpr) -> require-sexpr
(define (requires/tidy reqs)
  (let* ([reqs (combine-requires reqs)]
         [reqs (group-requires reqs)])
    (require-pretty-format reqs)))

;; requires/trim : path-string? (listof require-sexpr) -> require-sexpr
;;
;; Note: Why pass in a list of the existing require forms -- why not
;; just use the "keep" list from show-requires? Because the keep list
;; only states the module name, not the original form. Therefore if
;; the original require has a subform like `(only-in mod f)` (or
;; rename-in, except-in, &c), we won't know how to preserve that
;; unless we're given it. That's why our strategy must be to look for
;; things to drop, as opposed to things to keep.
(define (requires/trim path-str reqs)
  (let* ([reqs (combine-requires reqs)]
         [sr (show-requires* path-str)]
         [drops (filter-map (λ (x)
                              (match x
                                [(list 'drop mod lvl) (list mod lvl)]
                                [_ #f]))
                            sr)]
         [reqs (filter-map (λ (req)
                             (cond [(member req drops) #f]
                                   [else req]))
                           reqs)]
         [reqs (group-requires reqs)])
    (require-pretty-format reqs)))

;; Use `bypass` to help convert from `#lang racket` to `#lang
;; racket/base` plus explicit requires.
;;
;; Note: Currently this is hardcoded to `#lang racket`, only.
(define (requires/base path-str reqs)
  (let* ([reqs (combine-requires reqs)]
         [sr (show-requires* path-str)]
         [drops (filter-map (λ (x)
                              (match x
                                [(list 'drop mod lvl) (list mod lvl)]
                                [_ #f]))
                            sr)]
         [adds (append*
                (filter-map (λ (x)
                              (match x
                                [(list 'bypass 'racket 0
                                       (list (list mod lvl _) ...))
                                 (filter (λ (x)
                                           (match x
                                             [(list 'racket/base 0) #f]
                                             [_ #t]))
                                         (map list mod lvl))]
                                [_ #f]))
                            sr))]
         [reqs (filter-map (λ (req)
                             (cond [(member req drops) #f]
                                   [else req]))
                           reqs)]
         [reqs (append reqs adds)]
         [reqs (group-requires reqs)])
    (require-pretty-format reqs)))

;; show-requires* : Like show-requires but accepts a path-string? that
;; need not already be a module path.
(define (show-requires* path-str)
  (define-values (base name _) (split-path (string->path path-str)))
  (parameterize ([current-load-relative-directory base]
                 [current-directory base])
    (show-requires name)))

(define (combine-requires reqs)
  (remove-duplicates
   (append* (for/list ([req reqs])
              (match req
                [(list* 'require vs)
                 (append*
                  (for/list ([v vs])
                    ;; Use (list mod level), like `show-requires` uses.
                    (match v
                      [(list* 'for-meta level vs) (map (curryr list level) vs)]
                      [(list* 'for-syntax vs)     (map (curryr list 1) vs)]
                      [(list* 'for-template vs)   (map (curryr list -1) vs)]
                      [(list* 'for-label vs)      (map (curryr list #f) vs)]
                      [v                          (list (list v 0))])))])))))

(module+ test
  (require rackunit)
  (check-equal?
   (combine-requires '((require a b c)
                       (require d e)
                       (require a f)
                       (require (for-syntax s t u) (for-label l0 l1 l2))
                       (require (for-meta 1 m1a m1b)
                                (for-meta 2 m2a m2b))))
   '((a 0) (b 0) (c 0) (d 0) (e 0) (f 0)
     (s 1) (t 1) (u 1)
     (l0 #f) (l1 #f) (l2 #f)
     (m1a 1) (m1b 1) (m2a 2) (m2b 2))))

;; Given a list of requires -- each in the (list module level) form
;; used by `show-requires` -- group them by level and convert them to
;; a Racket `require` form. Also, sort the subforms by phase level:
;; for-syntax, for-template, for-label, for-meta, and plain (0).
;; Within each such group, sort them first by module paths then
;; relative requires. Within each such group, sort alphabetically.
;;
;; Note: Does *not* attempt to sort things *within* require subforms
;; such as except-in, only-in and so on. Such forms are only sorted in
;; between module paths and relative requires.
(define (group-requires reqs)
  ;; Put the requires into a hash of sets.
  (define ht (make-hasheq)) ;(hash/c <level> (set <mod>))
  (for ([req reqs]) (match req
                      [(list mod lvl) (hash-update! ht lvl
                                                    (lambda (s) (set-add s mod))
                                                    (set mod))]))
  (define (mod-set->mod-list mod-set)
    (sort (set->list mod-set) mod<?))
  (define (for-level level k)
    (define mods (hash-ref ht level #f))
    (cond [mods (k (mod-set->mod-list mods))]
          [else '()]))
  (define (preface . pres)
    (λ (mods) `((,@pres ,@mods))))
  (define (meta-levels)
    (sort (for/list ([x (hash-keys ht)] #:when (not (member x '(-1 0 1 #f)))) x)
          <))
  `(require
    ,@(for-level  1 (preface 'for-syntax))
    ,@(for-level -1 (preface 'for-template))
    ,@(for-level #f (preface 'for-label))
    ,@(append* (for/list ([level (in-list (meta-levels))])
                 (for-level level (preface 'for-meta level))))
    ,@(for-level 0 values)))

(module+ test
  (check-equal? (group-requires
                 (combine-requires
                  '((require z c b a)
                    (require (for-meta 4 m41 m40))
                    (require (for-meta -4 m-41 m-40))
                    (require (for-label l1 l0))
                    (require (for-template t1 t0))
                    (require (for-syntax s1 s0))
                    (require "a.rkt" "b.rkt" "c.rkt" "z.rkt"
                             (only-in "mod.rkt" oi)
                             (only-in mod oi)))))
                '(require
                  (for-syntax s0 s1)
                  (for-template t0 t1)
                  (for-label l0 l1)
                  (for-meta -4 m-40 m-41)
                  (for-meta 4 m40 m41)
                  a b c (only-in mod oi) z
                  "a.rkt" "b.rkt" "c.rkt" (only-in "mod.rkt" oi) "z.rkt")))

(define (mod<? a b)
  (define (key x)
    (match x
      [(list 'only-in   m _ ...)     (key m)]
      [(list 'except-in m _ ...)     (key m)]
      [(list 'prefix-in _ m)         (key m)]
      [(list 'relative-in _ m _ ...) (key m)]
      [m                             m]))
  (let ([a (key a)]
        [b (key b)])
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

;; require-pretty-format : list? -> string?
(define (require-pretty-format x)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (require-pretty-print x))
  (get-output-string out))

(module+ test
  (check-equal? (require-pretty-format
                 '(require a))
                "(require a)\n")
  (check-equal? (require-pretty-format
                 '(require a b))
                "(require a\n         b)\n")
  (check-equal? (require-pretty-format
                 '(require (for-syntax a b) (for-meta 2 c d) e f))
                "(require (for-syntax a\n                     b)\n         (for-meta 2 c\n                     d)\n         e\n         f)\n"))

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
;;
;; Note: Does *not* attempt to format things *within* require subforms
;; such as except-in, only-in and so on. Each such form is put on its
;; own line, but might run >80 chars.
(define (require-pretty-print x)
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
      [(list* (and pre (or 'require 'for-syntax 'for-template 'for-label))
              this more)
       (prn-form (format "~s" pre) this more)
       (when (eq? pre 'require)
         (newline))]
      [(list* 'for-meta level this more)
       (prn-form (format "for-meta ~a" level) this more)]
      [this
       (printf "~a~s" (indent-string) this)]))
  (prn x #t 0))

;;; find-collection

(define (do-find-collection str)
  (match (with-handlers ([exn:fail? (λ _ #f)])
           (and ;;#f ;<-- un-comment to exercise fallback path
            (dynamic-require 'find-collection/find-collection
                             'find-collection-dir)))
    [#f 'find-collection-not-installed]
    [f  (map path->string (f str))]))

(define find-collection (compose elisp-println do-find-collection))

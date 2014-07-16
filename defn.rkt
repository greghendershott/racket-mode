#lang racket/base

(require syntax/modread
         racket/match
         racket/function
         racket/pretty
         racket/contract)

(provide
 (contract-out
  [find-definition
   (-> string?
       (or/c #f 'kernel (list/c path-string? natural-number/c natural-number/c)))]
  [find-provision
   (-> string?
       (or/c #f 'kernel (list/c path-string? natural-number/c natural-number/c)))]
  [display-definition (-> string? void?)]
  [display-provision (-> string? void?)]))

;; Try to find the definition of `str`
(define (find-definition str)
  (find-x str definition-in-stx #:expand? #t))

;; Try to find the provide of `str`.
(define (find-provision str)
  (find-x str provision-in-stx #:expand? #f))

(define/contract (find-x str f #:expand? expand?)
  (-> string? (-> symbol? syntax? syntax?) #:expand? boolean?
      (or/c #f 'kernel (list/c path-string? natural-number/c natural-number/c)))
  (define-values (id where) (source str))
  (and id
       (match where
         ['kernel 'kernel]
         [path? (define stx (f id (file->syntax where #:expand? expand?)))
                (list (path->string (or (syntax-source stx) where))
                      (or (syntax-line stx) 1)
                      (or (syntax-column stx) 0))]
         [_ #f])))

;; Display definition location, and if possible, some of the original
;; unexpanded definition syntax (to show e.g. function arguments names).
(define (display-definition str) ;; string? -> void?
  (define stx-expanded (find-definition str #:expand? #t))
  (define datum-unexpanded (and stx-expanded
                                (let ([stx (find-definition str #:expand? #f)])
                                  (and stx (syntax? stx)
                                       (syntax->datum stx)))))
  (match* (stx-expanded datum-unexpanded)
    [(#f _)                 (printf "; No definition found for ~a\n" str)]
    [('kernel _)            (printf "; The Racket #%kernel defines ~a\n" str)]
    [(stx (list x y _ ...)) (printf "\n; ~a: (~a ~a ...\n"
                                    (source-loc-str stx) x y)]
    [(stx _)                (printf "\n; ~a defines ~a\n"
                                    (source-loc-str stx) str)]))

(define (display-provision str) ;; string? -> void?
  (match (find-provision str)
    [#f (printf "; No provide found for ~a\n" str)]
    ['kernel (printf "; The Racket #%kernel provides ~a\n" str)]
    [stx (printf "\n; ~a provides ~a\n"
                 (source-loc-str stx)
                 (commented (pretty-format (syntax->datum stx))))]))
  
(define (commented str)
  (regexp-replace* "\n" str "\n;"))

(define (source-loc-str stx)
  (format "~a:~a:~a"
          (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)))

;; Wrapper for source* which also accepts string? or symbol?
(define/contract (source v)
  (-> (or/c string? symbol? identifier?)
      (values (or/c symbol? #f) (or/c path? #f 'kernel)))
  (cond [(string? v)     (source (string->symbol v))]
        [(symbol? v)     (source (namespace-symbol->identifier v))]
        [(identifier? v) (source* v)]))

;; Return the source where an identifier binding is defined, as well
;; as the id used in the source (which is not necessarily the same,
;; e.g. `(provide (rename-out ...`).
(define/contract (source* id)
  (-> identifier?
      (values (or/c symbol? #f) (or/c path? #f 'kernel)))
  (match (identifier-binding id)
    [(list source-mpi source-id
           nominal-source-mpi nominal-source-id
           source-phase import-phase nominal-export-phase)
     (match (resolved-module-path-name (module-path-index-resolve source-mpi))
       [(? path-string? path)        (values nominal-source-id path)]
       ['#%kernel                    (values nominal-source-id 'kernel)]
       [(? symbol? sym)              (values nominal-source-id (sym->path sym))]
       [(list (? symbol? sym) _ ...) (values nominal-source-id (sym->path sym))]
       [_ (values #f #f)])]
    [_ (values #f #f)]))

;; When module source is 'sym or '(sym sym1 ...) treat it as "sym.rkt"
;; in the current-load-relative-directory.
(define (sym->path sym)
  (build-path (current-load-relative-directory) (format "~a.rkt" sym)))

;; Return a syntax object (or #f) for the contents of `file`.
(define (file->syntax file #:expand? expand?)
  (define-values (base _ __) (split-path file))
  (parameterize ([current-load-relative-directory base]
                 [current-namespace (make-base-namespace)])
    (define stx (with-handlers ([exn:fail? (const #f)])
                  (with-module-reading-parameterization
                   (thunk
                    (with-input-from-file file read-syntax/count-lines)))))
    (if expand?
        (expand stx) ;; do this while current-load-relative-directory is set
        stx)))

(define (read-syntax/count-lines)
  (port-count-lines! (current-input-port))
  (read-syntax))

;; Given a symbol? and syntax?, return syntax? corresponding to the
;; definition.
;;
;; If `stx` is run through expand we can find things defined via
;; definer macros.
;;
;; If `stx` is not run through expand, we will miss some things,
;; however the syntax will be closer to what a human expects --
;; e.g. `(define (f x) x)` instead of `(define-values (f) (lambda (x) x))`.
(define (definition-in-stx sym stx) ;;symbol? syntax? -> syntax?
  (define (eq-sym? stx)
    (if (eq? sym (syntax-e stx))
        stx
        #f))
  (syntax-case* stx
                (module #%module-begin define-values define-syntaxes
                        define define/contract
                        define-syntax struct define-struct)
                syntax-e=?
    [(module _ _ (#%module-begin . stxs)) (for/or ([stx (syntax->list #'stxs)])
                                            (definition-in-stx sym stx))]
    [(define          (s . _) . _)  (eq-sym? #'s) stx]
    [(define/contract (s . _) . _)  (eq-sym? #'s) stx]
    [(define s . _)                 (eq-sym? #'s) stx]
    [(define-values (ss ...) . _)   (ormap eq-sym? (syntax->list #'(ss ...)))
                                    (ormap eq-sym? (syntax->list #'(ss ...)))]
    [(define-syntax (s .  _) . _)   (eq-sym? #'s) stx]
    [(define-syntax s . _)          (eq-sym? #'s) stx]
    [(define-syntaxes (ss ...) . _) (ormap eq-sym? (syntax->list #'(ss ...)))
                                    (ormap eq-sym? (syntax->list #'(ss ...)))]
    [(define-struct s . _)          (eq-sym? #'s) stx]
    [(define-struct (s _) . _)      (eq-sym? #'s) stx]
    [(struct s . _)                 (eq-sym? #'s) stx]
    [(struct (s _) . _)             (eq-sym? #'s) stx]
    [_ #f]))

;; Given a symbol? and syntax?, return syntax? corresponding to the
;; provision. Note that we do NOT want stx to be run through `expand`
;; because we want the original contract definitions (if any).
(define (provision-in-stx sym stx) ;;symbol? syntax? -> syntax?
  (define (eq-sym? stx)
    (if (eq? sym (syntax-e stx))
        stx
        #f))
  (syntax-case* stx
                (module #%module-begin #%provde provide provide/contract)
                syntax-e=?
    [(module _ _ (#%module-begin . ss)) (ormap (curry provision-in-stx sym)
                                               (syntax->list #'ss))]
    [(provide/contract . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case stx ()
         [(s _) (eq-sym? #'s) stx]
         [_ #f]))]
    [(provide . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case* stx (contract-out) syntax-e=?
         [(contract-out . stxs)
          (for/or ([stx (syntax->list #'stxs)])
            (syntax-case* stx (rename struct) syntax-e=?
              [(struct s _ ...)     (eq-sym? #'s) stx]
              [(struct (s _) _ ...) (eq-sym? #'s) stx]
              [(rename _ s _)       (eq-sym? #'s) stx]
              [(s _)                (eq-sym? #'s) stx]
              [_ #f]))]
         ;; Only care about contracts
         ;; [s (eq-sym? #'s) stx]
         [_ #f]))]
    [(#%provide . ss)
     (ormap eq-sym? (syntax->list #'ss))
     (ormap eq-sym? (syntax->list #'ss))]
    [_ #f]))

;; For use with syntax-case*. When we use syntax-case for syntax-e equality.
(define (syntax-e=? a b)
  (equal? (syntax-e a) (syntax-e b)))

(module+ test
  (require rackunit)
  (check-equal?
   (syntax->datum
    (provision-in-stx
     'foo
     #'(module a b
         (#%module-begin
          (define x 1)
          (provide baz)
          (provide/contract [foo x] [bar y])))))
    '(foo x))
  (require net/url racket/port)
  (check-true
   (match (with-output-to-string (thunk (display-definition "get-pure-port")))
     ;; full path depends on OS and Racket version, so check tail:
     [(pregexp "^.+?/collects/net/url\\.rkt:\\d+:\\d+ defines `get-pure-port`\n$") #t]
     [x x])))

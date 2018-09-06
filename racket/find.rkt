#lang racket/base

(require racket/contract
         (only-in racket/format ~a)
         racket/list
         racket/match
         "syntax.rkt")

(provide find-definition
         find-signature)

(define location/c (list/c path-string? natural-number/c natural-number/c))

;; Try to find the definition of `str`, returning a list with the file
;; name, line and column, 'kernel, or #f if not found.
(define/contract (find-definition str)
  (-> string? (or/c #f 'kernel location/c))
  (match (find-definition/stx str)
    [(list* stx file submods)
     (list (path->string (or (syntax-source stx) file))
           (or (syntax-line stx) 1)
           (or (syntax-column stx) 0))]
    [v v]))

;; Try to find the definition of `str`, returning its signature or #f.
;; When defined in 'kernel, returns a form saying so, not #f.
(define/contract (find-signature str)
  (-> string? (or/c #f pair?))
  (match (find-definition/stx str)
    ['kernel '("defined in #%kernel, signature unavailable")]
    [(list* id-stx file submods)
     (define file-stx (file->syntax file))
     (define sub-stx (submodule file submods file-stx))
     (match ($signature (syntax-e id-stx) sub-stx)
       [(? syntax? stx) (syntax->datum stx)]
       [_ #f])]
    [v v]))

(define/contract (find-definition/stx str)
  (-> string?
      (or/c #f 'kernel (cons/c syntax? (cons/c path? (listof symbol?)))))
  (match (identifier-binding* str)
    [(? list? xs)
     (define ht (make-hash)) ;cache in case source repeated
     (for/or ([x (in-list (remove-duplicates xs))])
       (match x
         [(cons id 'kernel) 'kernel]
         [(list* id file submods)
          (define (sub-stx file->stx)
            (hash-ref! ht (cons file file->stx)
                       (λ () (submodule file submods (file->stx file)))))
          (match (or ($definition id (sub-stx file->expanded-syntax))
                     (match ($renaming-provide id (sub-stx file->syntax))
                       [(? syntax? s)
                        ($definition (syntax-e s) (sub-stx file->expanded-syntax))]
                       [_ #f]))
            [#f  #f]
            [stx (list* stx file submods)])]))]
    [_ #f]))

;; Distill identifier-binding to what we need. Unfortunately it can't
;; report the definition id in the case of a contract-out and a
;; rename-out, both. For `(provide (contract-out [rename orig new
;; contract]))` it reports (1) the contract-wrapper as the id, and (2)
;; `new` as the nominal-id -- but NOT (3) `orig`. Instead the caller
;; will need try using `renaming-provide`.
(define/contract (identifier-binding* v)
  (-> (or/c string? symbol? identifier?)
      (or/c #f
            (listof (cons/c symbol?
                            (or/c 'kernel
                                  (cons/c path-string? (listof symbol?)))))))
  (define sym->id namespace-symbol->identifier)
  (define id (cond [(string? v)     (sym->id (string->symbol v))]
                   [(symbol? v)     (sym->id v)]
                   [(identifier? v) v]))
  (match (identifier-binding id)
    [(list source-mpi         source-id
           nominal-source-mpi nominal-source-id
           source-phase import-phase nominal-export-phase)
     (list (cons source-id         (mpi->path source-mpi))
           (cons nominal-source-id (mpi->path nominal-source-mpi)))]
    [_ #f]))

(define/contract (mpi->path mpi)
  (-> module-path-index?
      (or/c 'kernel
            (cons/c path-string? (listof symbol?))))
  (define (hash-bang-symbol? v)
    (and (symbol? v)
         (regexp-match? #px"^#%" (symbol->string v))))
  (match (resolved-module-path-name (module-path-index-resolve mpi))
    [(? hash-bang-symbol?) 'kernel]
    [(? path-string? path) (list path)]
    [(? symbol? sym) (list (build-path (current-load-relative-directory)
                                       (~a sym ".rkt")))]
    [(list (? path-string? path) (? symbol? subs) ...)
     (list* path subs)]))

;; For use with syntax-case*. When we use syntax-case for syntax-e equality.
(define (syntax-e-eq? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define ((make-eq-sym? sym) stx)
  (and (eq? sym (syntax-e stx)) stx))

(define (file-module file)
  (match (path->string (last (explode-path file)))
    [(pregexp "(.+?)\\.rkt$" (list _ v)) (string->symbol v)]))

;; Return bodies (wrapped in begin) of the module indicated by
;; file and sub-mod-syms.
(define (submodule file sub-mod-syms stx)
  (submodule* (cons (file-module file) sub-mod-syms) stx))

(define (submodule* mods stx)
  (match-define (cons this more) mods)
  (define (subs stxs)
    (if (empty? more)
        #`(begin . #,stxs)
         (ormap (λ (stx) (submodule* more stx))
                (syntax->list stxs))))
  (syntax-case* stx (module #%module-begin) syntax-e-eq?
    [(module name _ (#%module-begin . stxs))
     (eq? this (syntax-e #'name))
     (subs #'stxs)]
    [(module name _ . stxs)
     (eq? this (syntax-e #'name))
     (subs #'stxs)]
    [_ #f]))

(module+ test
  (require rackunit)
  (check-equal? (syntax->datum
                 (submodule "/path/to/file.rkt" '(a b c)
                            #'(module file racket
                                (module a racket
                                  (module not-b racket #f)
                                  (module b racket
                                    (module not-c racket #f)
                                    (module c racket "bingo")
                                    (module not-c racket #f))
                                  (module not-b racket #f)))))
                '(begin "bingo")))

;; Given a symbol and syntax, return syntax corresponding to the
;; definition. Intentionally does NOT walk into module forms, so, give
;; us the module bodies wrapped in begin.
;;
;; If `stx` is expanded we can find things defined via definer
;; macros.
;;
;; If `stx` is not expanded, we will miss some things, however the
;; syntax will be closer to what a human expects -- e.g. `(define (f
;; x) x)` instead of `(define-values (f) (lambda (x) x))`.
(define ($definition sym stx) ;;symbol? syntax? -> syntax?
  (define eq-sym? (make-eq-sym? sym))
  ;; This is a hack to handle definer macros that neglect to set
  ;; srcloc properly using syntax/loc or (format-id ___ #:source __):
  ;; If the stx lacks srcloc and its parent stx has srcloc, return the
  ;; parent stx instead. Caveats: 1. Assumes caller only cares about
  ;; the srcloc. 2. We only check immediate parent. 3. We only use
  ;; this for define-values and define-syntaxes, below, on the
  ;; assumption that this only matters for fully-expanded syntax.
  (define (loc s)
    (if (and (not (syntax-line s))
             (syntax-line stx))
        stx
        s))
  (syntax-case* stx
      (begin define-values define-syntaxes
             define define/contract
             define-syntax struct define-struct)
      syntax-e-eq?
    [(begin . stxs)                 (ormap (λ (stx) ($definition sym stx))
                                           (syntax->list #'stxs))]
    [(define          (s . _) . _)  (eq-sym? #'s) stx]
    [(define/contract (s . _) . _)  (eq-sym? #'s) stx]
    [(define s . _)                 (eq-sym? #'s) stx]
    [(define-values (ss ...) . _)   (ormap eq-sym? (syntax->list #'(ss ...)))
                                    (loc (ormap eq-sym? (syntax->list #'(ss ...))))]
    [(define-syntax (s .  _) . _)   (eq-sym? #'s) stx]
    [(define-syntax s . _)          (eq-sym? #'s) stx]
    [(define-syntaxes (ss ...) . _) (ormap eq-sym? (syntax->list #'(ss ...)))
                                    (loc (ormap eq-sym? (syntax->list #'(ss ...))))]
    [(define-struct s . _)          (eq-sym? #'s) stx]
    [(define-struct (s _) . _)      (eq-sym? #'s) stx]
    [(struct s . _)                 (eq-sym? #'s) stx]
    [(struct (s _) . _)             (eq-sym? #'s) stx]
    [_                              #f]))

;; Given a symbol and syntax, return syntax corresponding to the
;; function definition signature. The input syntax should NOT be
;; `expand`ed. This intentionally does NOT walk into module forms, so,
;; give us the module bodies wrapped in begin.
(define ($signature sym stx) ;;symbol? syntax? -> (or/c #f list?)
  (define eq-sym? (make-eq-sym? sym))
  (syntax-case* stx (begin define define/contract case-lambda) syntax-e-eq?
    [(begin . stxs)                               (ormap (λ (stx) ($signature sym stx))
                                                         (syntax->list #'stxs))]
    [(define          (s . as) . _)               (eq-sym? #'s) #'(s . as)]
    [(define/contract (s . as) . _)               (eq-sym? #'s) #'(s . as)]
    [(define s (case-lambda [(ass ...) . _] ...)) (eq-sym? #'s) #'((s ass ...) ...)]
    [_                                            #f]))

;; Find sym in a contracting and/or renaming provide, and return the
;; syntax for the ORIGINAL identifier (before being contracted and/or
;; renamed). The input syntax should NOT be expanded.
(define ($renaming-provide sym stx) ;;symbol? syntax? -> syntax?
  (define eq-sym? (make-eq-sym? sym))
  (syntax-case* stx (begin provide provide/contract) syntax-e-eq?
    [(begin . stxs)
     (ormap (λ (stx) ($renaming-provide sym stx))
            (syntax->list #'stxs))]
    [(provide/contract . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case stx ()
         [(s _) (eq-sym? #'s)]
         [_     #f]))]
    [(provide . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case* stx (contract-out rename-out) syntax-e-eq?
         [(contract-out . stxs)
          (for/or ([stx (syntax->list #'stxs)])
            (syntax-case* stx (rename) syntax-e-eq?
              [(rename orig s _) (eq-sym? #'s) #'orig]
              [(s _)             (eq-sym? #'s) #'s]
              [_                 #f]))]
         [(rename-out . stxs)
          (for/or ([stx (syntax->list #'stxs)])
            (syntax-case* stx () syntax-e-eq?
              [(orig s) (eq-sym? #'s) #'orig]
              [_        #f]))]
         [_ #f]))]
    [_              #f]))

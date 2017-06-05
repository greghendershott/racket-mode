#lang racket/base

(require racket/contract
         racket/function
         racket/match
         syntax/modread)

(provide
 (contract-out
  [find-definition
   (-> string?
       (or/c #f 'kernel (list/c path-string?
                                natural-number/c
                                natural-number/c)))]
  [find-signature
   (-> string?
       (or/c #f pair?))]))

;; Try to find the definition of `str`, returning a list with the file
;; name, line and column, 'kernel, or #f if not found.
(define (find-definition str)
  (match (find-definition/stx str)
    [(cons stx where)
     (list (path->string (or (syntax-source stx) where))
           (or (syntax-line stx) 1)
           (or (syntax-column stx) 0))]
    [v v]))

;; Try to find the definition of `str`, returning its signature or #f.
;; When defined in 'kernel, returns a form saying so, not #f.
(define (find-signature str)
  (match (find-definition/stx str)
    ['kernel '("defined in #%kernel, signature unavailable")]
    [(cons stx where)
     (match (signature (syntax-e stx) (file->syntax where #:expand? #f))
       [(? syntax? stx) (syntax->datum stx)]
       [_ #f])]
    [v v]))

(define (find-definition/stx str)
  ;; (-> string? (or/c #f 'kernel (cons/c syntax? path?)))
  (match (identifier-binding* str)
    [(? list? xs)
     (for/or ([x (in-list xs)])
       (match x
         [(cons id 'kernel) 'kernel]
         [(cons id (? path? where))
          (define expanded (file->syntax where #:expand? #t))
          (define stx
            (or (definition id expanded)
                ;; Handle rename + contract
                (match (renaming-provide id (file->syntax where #:expand? #f))
                  [(? syntax? stx) (definition (syntax-e stx) expanded)]
                  [_ #f])))
          (and stx
               (cons stx where))]))]
    [_ #f]))

;; A wrapper for identifier-binding. Keep in mind that unfortunately
;; it can't report the definition id in the case of a contract-out and
;; a rename-out, both. For `(provide (contract-out [rename orig new
;; contract]))` it reports (1) the contract-wrapper as the id, and (2)
;; `new` as the nominal-id -- but NOT (3) `orig`.
(define/contract (identifier-binding* v)
  (-> (or/c string? symbol? identifier?)
      (or/c #f (listof (cons/c symbol? (or/c path? 'kernel #f)))))
  (define sym->id namespace-symbol->identifier)
  (define id (cond [(string? v)     (sym->id (string->symbol v))]
                   [(symbol? v)     (sym->id v)]
                   [(identifier? v) v]))
  (match (identifier-binding id)
    [(list source-mpi source-id
           nominal-source-mpi nominal-source-id
           source-phase import-phase nominal-export-phase)
     (define (mpi->path mpi)
       (match (resolved-module-path-name (module-path-index-resolve mpi))
         [(? path-string? path)        path]
         ['#%kernel                    'kernel]
         [(? symbol? sym)              (sym->path sym)]
         [(list (? symbol? sym) _ ...) (sym->path sym)]
         [_                            #f]))
     (list (cons source-id         (mpi->path source-mpi))
           (cons nominal-source-id (mpi->path nominal-source-mpi)))]
    [_ #f]))

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
        (expand stx) ;expand while current-load-relative-directory is set
        stx)))

(define (read-syntax/count-lines)
  (port-count-lines! (current-input-port))
  (read-syntax))

;; Given a symbol? and syntax?, return syntax? corresponding to the
;; definition.
;;
;; If `stx` is expanded we can find things defined via definer
;; macros.
;;
;; If `stx` is not expanded, we will miss some things, however the
;; syntax will be closer to what a human expects -- e.g. `(define (f
;; x) x)` instead of `(define-values (f) (lambda (x) x))`.
(define (definition sym stx) ;;symbol? syntax? -> syntax?
  (define eq-sym? (make-eq-sym? sym))
  ;; This is a hack to handle definer macros that neglect to set
  ;; srcloc properly using syntx/loc or (format-id ___ #:source __):
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
      (module #%module-begin define-values define-syntaxes
              define define/contract
              define-syntax struct define-struct)
      syntax-e-eq?
    [(module _ _ (#%module-begin . stxs))
     (ormap (λ (stx) (definition sym stx))
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

;; Given a symbol? and syntax?, return syntax? corresponding to the
;; function definition signature. Note that we do NOT want stx to be
;; run through `expand`.
(define (signature sym stx) ;;symbol? syntax? -> (or/c #f list?)
  (define eq-sym? (make-eq-sym? sym))
  (syntax-case* stx
      (module #%module-begin define define/contract case-lambda)
      syntax-e-eq?
    [(module _ _ (#%module-begin . stxs))
     (ormap (λ (stx)
              (signature sym stx))
            (syntax->list #'stxs))]
    [(module _ _ . stxs)
     (ormap (λ (stx)
              (signature sym stx))
            (syntax->list #'stxs))]
    [(define          (s . as) . _)               (eq-sym? #'s) #'(s . as)]
    [(define/contract (s . as) . _)               (eq-sym? #'s) #'(s . as)]
    [(define s (case-lambda [(ass ...) . _] ...)) (eq-sym? #'s) #'((s ass ...) ...)]
    [_                                            #f]))

;; Given a symbol? and syntax?, return syntax? corresponding to the
;; contracted provide. Note that we do NOT want stx to be run through
;; `expand` because we want the original contract definitions (if
;; any). ** This is currently not used. If we ever add a
;; `find-provision` function, it would use this.
(define (contracting-provide sym stx) ;;symbol? syntax? -> syntax?
  (define eq-sym? (make-eq-sym? sym))
  (syntax-case* stx
      (module #%module-begin provide provide/contract)
      syntax-e-eq?
    [(module _ _ (#%module-begin . ss))
     (ormap (λ (stx) (contracting-provide sym stx))
            (syntax->list #'ss))]
    [(provide/contract . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case stx ()
         [(s _) (eq-sym? #'s) stx]
         [_     #f]))]
    [(provide . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case* stx (contract-out) syntax-e-eq?
         [(contract-out . stxs)
          (for/or ([stx (syntax->list #'stxs)])
            (syntax-case* stx (rename struct) syntax-e-eq?
              [(struct s _ ...)     (eq-sym? #'s) stx]
              [(struct (s _) _ ...) (eq-sym? #'s) stx]
              [(rename _ s _)       (eq-sym? #'s) stx]
              [(s _)                (eq-sym? #'s) stx]
              [_                    #f]))]
         ;; Only care about contracting provides.
         ;; [s (eq-sym? #'s) stx]
         [_ #f]))]
    [_ #f]))

;; Find sym in a contracting and/or renaming provide, and return the
;; syntax for the ORIGINAL identifier (before being contracted and/or
;; renamed).
(define (renaming-provide sym stx) ;;symbol? syntax? -> syntax?
  (define eq-sym? (make-eq-sym? sym))
  (syntax-case* stx
      (module #%module-begin provide provide/contract)
      syntax-e-eq?
    [(module _ _ (#%module-begin . ss))
     (ormap (λ (stx) (renaming-provide sym stx))
            (syntax->list #'ss))]
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
    [_ #f]))

;; For use with syntax-case*. When we use syntax-case for syntax-e equality.
(define (syntax-e-eq? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define ((make-eq-sym? sym) stx)
  (and (eq? sym (syntax-e stx)) stx))

#lang racket/base

(require syntax/modread
         racket/match
         racket/function
         racket/pretty
         racket/contract
         racket/list)

(provide
 (contract-out
  [find-definition
   (-> string?
       (or/c #f 'kernel (list/c path-string? natural-number/c natural-number/c)))]
  [find-provision
   (-> string?
       (or/c #f 'kernel (list/c path-string? natural-number/c natural-number/c)))]))

;; Try to find the definition of `str`
(define (find-definition str)
  (find-x str 'define definition-in-stx #:expand? #t))

;; Try to find the provide of `str`.
(define (find-provision str)
  (find-x str 'provide provision-in-stx #:expand? #f))

(define/contract (find-x str which f #:expand? expand?)
  (-> string?
      (or/c 'define 'provide)
      (-> symbol? syntax? (or/c #f syntax?))
      #:expand? boolean?
      (or/c #f 'kernel (list/c path-string? natural-number/c natural-number/c)))
  (define-values (ids where) (source str which))
  (and ids
       (match where
         ['kernel 'kernel]
         [path? (define file-stx (file->syntax where #:expand? expand?))
                (or (for/or ([id (in-list ids)])
                      (match (f id file-stx)
                        [(? syntax? stx)
                         (list (path->string (or (syntax-source stx) where))
                               (or (syntax-line stx) 1)
                               (or (syntax-column stx) 0))]
                        [_ #f]))
                    (list (path->string where) 1 0))]
         [_ #f])))

;; Return the source where an identifier binding is defined or
;; provided, as well as a list of potential ids used in the source.
;; Unfortunately it's possible that none of the ids are used in the
;; definition: `identifier-binding` doesn't report the definition id
;; in the case of (provide (contract-out rename old new contract)),
;; i.e. a contract-out and a rename-out, both. It reports just the id
;; for the contract wrapper, and the id used for `new`, the rename.
;; But not `old`.
(define/contract (source v which)
  (-> (or/c string? symbol? identifier?)
      (or/c 'define 'provide)
      (values (or/c (listof symbol?) #f) (or/c path? #f 'kernel)))
  (define sym->id namespace-symbol->identifier)
  (define id (cond [(string? v)     (sym->id (string->symbol v))]
                   [(symbol? v)     (sym->id v)]
                   [(identifier? v) v]))
  (match (identifier-binding id)
    [(list source-mpi source-id
           nominal-source-mpi nominal-source-id
           source-phase import-phase nominal-export-phase)
     (define try-ids (remove-duplicates (list source-id nominal-source-id)))
     (define use-mpi (case which
                       ['define source-mpi]
                       ['provide nominal-source-mpi]))
     (match (resolved-module-path-name (module-path-index-resolve use-mpi))
       [(? path-string? path)        (values try-ids path)]
       ['#%kernel                    (values try-ids 'kernel)]
       [(? symbol? sym)              (values try-ids (sym->path sym))]
       [(list (? symbol? sym) _ ...) (values try-ids (sym->path sym))]
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
  (require rackunit
           racket/list)
  (check-equal? (syntax->datum
                 (provision-in-stx 'foo
                                   #'(module a b
                                       (#%module-begin
                                        (define x 1)
                                        (provide baz)
                                        (provide/contract [foo x] [bar y])))))
                '(foo x))
  (check-equal? 'kernel (find-definition "display"))
  (check-regexp-match "/racket/private/misc\\.rkt$"
                      (first (find-definition "displayln")))
  (check-regexp-match "/racket/base\\.rkt$"
                      (first (find-provision "display")))
  (check-regexp-match "/racket/base\\.rkt$"
                      (first (find-provision "displayln"))))

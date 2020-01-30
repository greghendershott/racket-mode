#lang racket/base

(require racket/contract
         racket/format
         racket/match
         (only-in "syntax.rkt" path->existing-expanded-syntax))

(provide ->identifier
         ->identifier/namespace
         ->identifier/expansion
         resolve-identifier-binding-info)

(define/contract (->identifier/namespace v)
  (-> (or/c symbol? string?) identifier?)
  (define sym->id namespace-symbol->identifier)
  (cond [(string? v) (sym->id (string->symbol v))]
        [(symbol? v) (sym->id v)]))

;; A simplifying helper for commands that want to work both ways, and
;; accept a first "how" or "context" argument that is either
;; 'namespace or a path string.
(define (->identifier how v)
  (match how
    ['namespace                       (->identifier/namespace   v)]
    [(? (and string? path-string?) p) (->identifier/expansion p v)]))

;; exp-mod-stx-or-path-str should either be:
;;
;; - The syntax for a module form that was read
;;   with-module-reading-parameterization and expanded. Such syntax
;;   has a 'module-body-context syntax property -- starting in Racket
;;   6.5 -- which can be used as lexical context to make an
;;   identifier. This lets identifier-binding work for identifiers as
;;   if they were in that body's lexical context -- including imported
;;   identifiers that aren't actually used as bindings in the module
;;   body.
;;
;; - A path string, in which case we get such expanded module syntax
;;   from file->expanded-syntax.
(define/contract (->identifier/expansion exp-mod-stx-or-path v)
  (-> (or/c syntax? path-string?)
      (or/c symbol? string?)
      identifier?)
  (define exp-mod-stx (match exp-mod-stx-or-path
                        [(? syntax? v) v]
                        [(? path-string? v) (path->existing-expanded-syntax v)]))
  (define (sym->id v)
    (if exp-mod-stx
        (expanded-module+symbol->identifier exp-mod-stx v)
        #'undefined)) ;not sure about this choice?
  (cond [(string? v) (sym->id (string->symbol v))]
        [(symbol? v) (sym->id v)]))

(define/contract (expanded-module+symbol->identifier exp-mod-stx sym)
  (-> syntax? symbol? identifier?)
  (datum->syntax (syntax-property exp-mod-stx 'module-body-context)
                 sym))


;;; Massaging values returned by identifier-binding

;; Given the result from identifier-binding, returns a subset of the
;; information, where the module path indexes are resolved to actual
;; paths, and where 'lexical value is treated as #f.
(define/contract (resolve-identifier-binding-info binding-info)
  (-> (or/c 'lexical
            #f
            (list/c module-path-index?
                    symbol?
                    module-path-index?
                    symbol?
                    exact-nonnegative-integer?
                    (or/c exact-integer? #f)
                    (or/c exact-integer? #f))
            (list/c symbol?))
      (or/c #f
             (listof (cons/c symbol?
                             (or/c 'kernel
                                   (cons/c path-string? (listof symbol?)))))))
  (match binding-info
     [(list source-mpi         source-id
            nominal-source-mpi nominal-source-id
            source-phase
            import-phase
            nominal-export-phase)
      (list (cons source-id         (mpi->path source-mpi))
            (cons nominal-source-id (mpi->path nominal-source-mpi)))]
     [_ #f]))

(define/contract (mpi->path mpi)
  (-> module-path-index?
      (or/c 'kernel
            (cons/c path-string? (listof symbol?))))
  (define (hash-percent-symbol v)
    (and (symbol? v)
         (regexp-match? #px"^#%" (symbol->string v))))
  (match (resolved-module-path-name (module-path-index-resolve mpi))
    [(? hash-percent-symbol) 'kernel]
    [(? path-string? path)   (list path)]
    [(? symbol? sym)
     (list (build-path (current-load-relative-directory)
                       (~a sym ".rkt")))]
    [(list (? path-string? path) (? symbol? subs) ...)
     (list* path subs)]))

(module+ test
  (require rackunit
           version/utils
           "syntax.rkt")
  ;; Check something that is in the namespace resulting from
  ;; module->namespace on, say, this source file.
  (parameterize ([current-namespace (module->namespace (syntax-source #'here))])
    (check-not-false (resolve-identifier-binding-info
                      (identifier-binding
                       (->identifier/namespace 'match))))
    (check-not-false (resolve-identifier-binding-info
                      (identifier-binding
                       (->identifier/namespace "match")))))
  (when (version<=? "6.5" (version))
    ;; Check something that is not in the current namespace, but is an
    ;; identifier in the lexical context of an expanded module form,
    ;; including imported identifiers.
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-load-relative-directory "/path/to"])
      (define path-str "/path/to/foobar.rkt")
      (define-values (stx _ns) (string->expanded-syntax-and-namespace
                                path-str
                                "(module foobar racket/base (require net/url racket/set)) 42"))
      ;; Simple
      (check-not-false (resolve-identifier-binding-info
                        (identifier-binding
                         (->identifier/expansion stx 'set?))))
      (check-not-false (resolve-identifier-binding-info
                        (identifier-binding
                         (->identifier/expansion stx "set?"))))
      ;; Renaming/contracting involved
      (check-not-false (resolve-identifier-binding-info
                        (identifier-binding
                         (->identifier/expansion stx 'get-pure-port))))
      (check-not-false (resolve-identifier-binding-info
                        (identifier-binding
                         (->identifier/expansion stx "get-pure-port"))))
      ;; Exercise use of path-string->existing-expanded-syntax
      (check-equal? (identifier-binding
                     (->identifier/expansion path-str 'define))
                    (identifier-binding
                     (->identifier/expansion stx 'define)))
      (check-equal? (syntax->datum
                     (->identifier/expansion "/not/yet/expanded.rkt" "whatever"))
                    'undefined))))

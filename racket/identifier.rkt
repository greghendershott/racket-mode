#lang racket/base

(require racket/contract
         racket/format
         racket/match)

(provide identifier-binding*)

;; A wrapper for identifier-binding.
;;
;; When given identifier syntax, uses that as-is.
;;
;; When given a string or symbol, makes an identifier. The lexical
;; context depends:
;;
;; - If supplied, #:expanded-module-syntax should be the syntax for a
;;   module form expanded using with-module-reading-parameterization.
;;   Its 'module-body-context syntax property -- added in Racket 6.5
;;   -- is used for the identifier's lexical context. This lets
;;   identifier-binding work for identifiers as if they were in that
;;   body's lexical context, including imported identifiers that
;;   aren't actually used as bindings in the module body.
;;
;; - Otherwise namespace-symbol->identifier is used to make the
;;   identifier, which will work e.g. when current-namespace is from
;;   module->namespace.
;;
;; Distills the return value to what we need in find.rkt, omitting
;; unused values and "cashing in" resolved module paths.
;;
;; Note: Unfortunately this can't report the definition id in the case
;; of a contract-out and a rename-out, both. For `(provide
;; (contract-out [rename orig new contract]))` identifier-binding
;; reports (1) the contract-wrapper as the id, and (2) `new` as the
;; nominal-id -- but NOT (3) `orig`. Instead the caller will need try
;; other strategies; see e.g. find.rkt.

(define/contract (identifier-binding* v
                                      #:expanded-module-syntax [exp-mod-stx #f])
  (->* ((or/c string? symbol? identifier?))
       (#:expanded-module-syntax (or/c #f syntax?))
       (or/c #f
             (listof (cons/c symbol?
                             (or/c 'kernel
                                   (cons/c path-string? (listof symbol?)))))))
  (define (sym->id sym)
    (match (and exp-mod-stx
                (syntax-property exp-mod-stx 'module-body-context))
      [#f   (namespace-symbol->identifier sym)]
      [lctx (datum->syntax lctx sym)]))
  (define id (cond [(string? v)     (sym->id (string->symbol v))]
                   [(symbol? v)     (sym->id v)]
                   [(identifier? v) v]))
  (match (identifier-binding id)
    [(list source-mpi         source-id
           nominal-source-mpi nominal-source-id
           source-phase       import-phase nominal-export-phase)
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
    (check-not-false (identifier-binding* #'match))
    (check-not-false (identifier-binding* 'match))
    (check-not-false (identifier-binding* "match")))
  (when (version<=? "6.5" (version))
    ;; Check something that is not in the current namespace, but is an
    ;; identifier in the lexical context of an expanded module form,
    ;; including imported identifiers.
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-load-relative-directory "/path/to"])
      (define-values (stx _ns) (string->expanded-syntax-and-namespace
                                "/path/to/foobar.rkt"
                                "(module foobar racket/base (require net/url racket/set)) 42"))
      ;; Simple
      (check-not-false (identifier-binding* #:expanded-module-syntax stx 'set?))
      (check-not-false (identifier-binding* #:expanded-module-syntax stx "set?"))
      ;; Renaming/contracting involved
      (check-not-false (identifier-binding* #:expanded-module-syntax stx 'get-pure-port))
      (check-not-false (identifier-binding* #:expanded-module-syntax stx "get-pure-port")))))

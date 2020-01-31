#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         (only-in xml xexpr->string)
         (only-in "../find.rkt" find-signature)
         "../identifier.rkt"
         "../syntax.rkt"
         "../scribble.rkt")

(provide type
         describe)

(define (type v)
  (type-or-sig v))

(define (type-or-sig v)
  (or (type-or-contract v)
      (sig v)))

(define (sig how v) ;any/c -> (or/c #f string?)
  (define as-str
    (match v
      [(? syntax? v) (syntax->datum v)]
      [(? symbol? v) (symbol->string v)]
      [_             #f]))
  (and as-str
       (match (find-signature how as-str)
         [#f #f]
         [x (~a x)])))

(define (type-or-contract v) ;any/c -> (or/c #f string?)
  (or
   ;; 1. Try using Typed Racket's REPL simplified type.
   (with-handlers ([exn:fail? (λ _ #f)])
     (match (with-output-to-string
              (λ ()
                ((current-eval)
                 (cons '#%top-interaction v))))
       [(pregexp "^- : (.*) \\.\\.\\..*\n" (list _ t)) t]
       [(pregexp "^- : (.*)\n$"            (list _ t)) t]))
   ;; 2. Try to find a contract.
   (with-handlers ([exn:fail? (λ _ #f)])
     (parameterize ([error-display-handler (λ _ (void))])
       ((current-eval)
        (cons '#%top-interaction
              `(if (has-contract? ,v)
                (~a (contract-name (value-contract ,v)))
                (error ""))))))))

(define (sig-and/or-type how stx)
  (define dat (syntax->datum stx))
  (define s (sig how dat))
  (define t (and (eq? how 'namespace) (type-or-contract stx)))
  (xexpr->string
   `(div ()
     (h1 () ,(or s (~a dat)))
     ,(cond [(not (or s t))
             `(p ()
               (em ()  ,(if (eq? how 'namespace)
                            "(Found no documentation, signature, type, or contract.)"
                            "(Found no documentation.")))]
            [t `(pre () ,t)]
            [else ""])
     (br ()))))

;;; describe

;; If a symbol has installed documentation, display it.
;;
;; Otherwise, walk the source to find the signature of its definition
;; (because the argument names have explanatory value), and also look
;; for Typed Racket type or a contract, if any.

(define/contract (describe how str)
  (-> (or/c 'namespace path-string? (cons/c path-string? string?))
      string?
      string?)
  (match how
    [(and (cons (? path-string?) (? string?)) path+anchor)
     (path+anchor->html path+anchor)]
    [(and (or 'namespace (? path-string?)) how)
     (define stx (->identifier how str))
     (define path+anchor (binding->path+anchor stx))
     (or (path+anchor->html path+anchor)
         (sig-and/or-type how stx))]))

(define here (syntax-source #'here))

#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         (only-in xml xexpr->string)
         (only-in "../find.rkt" find-signature)
         "../scribble.rkt")

(provide type
         describe)

(define (type v)
  (type-or-sig v))

(define (type-or-sig v)
  (or (type-or-contract v)
      (sig v)
      ""))

(define (sig v) ;any/c -> (or/c #f string?)
  (and (symbol? v)
       (match (find-signature (symbol->string v))
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

(define (sig-and/or-type stx)
  (define dat (syntax->datum stx))
  (define s (sig dat))
  (define t (type-or-contract stx))
  (xexpr->string
   `(div ()
     (h1 () ,(or s (~a dat)))
     ,(cond [(not (or s t))
             `(p ()
               (em ()  "(Found no documentation, signature, type, or contract.)"))]
            [t `(pre () ,t)]
            [else ""])
     (br ()))))

;;; describe

;; If a symbol has installed documentation, display it.
;;
;; Otherwise, walk the source to find the signature of its definition
;; (because the argument names have explanatory value), and also look
;; for Typed Racket type or a contract, if any.

(define/contract (describe str)
  (-> string? string?)
  (define stx (namespace-symbol->identifier (string->symbol str)))
  (or (scribble-doc/html stx)
      (sig-and/or-type stx)))

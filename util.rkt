#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide display-commented
         with-dynamic-requires
         box-swap!
         string->namespace-syntax)

(define (display-commented str)
  (eprintf "; ~a\n"
           (regexp-replace* "\n" str "\n; ")))

(define-syntax (with-dynamic-requires stx)
  (syntax-parse stx
    [(_ ([lib:id id:id] ...+) body:expr ...+)
     #'(let ([id (dynamic-require 'lib 'id)] ...)
         body ...)]))

(define (box-swap! box f . args)
  (let loop ()
    (let* ([old (unbox box)]
           [new (apply f old args)])
      (if (box-cas! box old new)
          new
          (loop)))))

(define (string->namespace-syntax str)
  (namespace-syntax-introduce
   (read-syntax "string->namespace-syntax" (open-input-string str))))

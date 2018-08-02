#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide display-commented
         with-dynamic-requires
         string->namespace-syntax)

(define (display-commented str)
  (eprintf "; ~a\n"
           (regexp-replace* "\n" str "\n; ")))

(define-syntax (with-dynamic-requires stx)
  (syntax-parse stx
    [(_ ([lib:id id:id] ...+) body:expr ...+)
     #'(let ([id (dynamic-require 'lib 'id)] ...)
         body ...)]))

(define (string->namespace-syntax str)
  (namespace-syntax-introduce
   (read-syntax #f (open-input-string str))))

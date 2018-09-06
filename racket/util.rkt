#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide display-commented
         with-dynamic-requires
         string->namespace-syntax
         syntax-or-sexpr->syntax
         syntax-or-sexpr->sexpr
         nat/c
         pos/c)

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

(define (syntax-or-sexpr->syntax v)
  (if (syntax? v)
      v
      (namespace-syntax-introduce (datum->syntax #f v))))

(define (syntax-or-sexpr->sexpr v)
  (if (syntax? v)
      (syntax-e v)
      v))

(define nat/c exact-nonnegative-integer?)
(define pos/c exact-positive-integer?)

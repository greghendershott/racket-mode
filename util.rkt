#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide display-commented
         with-dynamic-requires
         syntax-or-sexpr->syntax
         syntax-or-sexpr->sexpr
         name-only)

(define (display-commented str)
  (eprintf "; ~a\n"
           (regexp-replace* "\n" str "\n; ")))

(define-syntax (with-dynamic-requires stx)
  (syntax-parse stx
    [(_ ([lib:id id:id] ...+) body:expr ...+)
     #'(let ([id (dynamic-require 'lib 'id)] ...)
         body ...)]))

(define (syntax-or-sexpr->syntax v)
  (if (syntax? v)
      v
      (namespace-syntax-introduce (datum->syntax #f v))))

(define (syntax-or-sexpr->sexpr v)
  (if (syntax? v)
      (syntax-e v)
      v))

;; racket/path provides `path-only` but not a `name-only`.
(define (name-only path)
  (define-values (_ name dir?) (split-path path))
  (and (not dir?) name))

(module+ test
  (require rackunit)
  (check-equal? (name-only (string->path "/path/to/foo.bar"))
                (string->path "foo.bar"))
  (check-equal? (name-only (string->path "/foo.bar"))
                (string->path "foo.bar"))
  (check-equal? (name-only (string->path "/path/to/dir/"))
                #f))

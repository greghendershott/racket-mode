#lang racket/base

(require racket/match
         "require.rkt")

(define top 10)

(define (f x)
  (let ([y x])
    (+ top (h x))))

(define (h x)
  (g x))

(f 10)

(for/list ([x (in-list '(1 2 3))])
  (format "~a" (f x)))

(define (add xs amt)
  (match xs
    [(list) (list)]
    [(cons x xs) (cons (+ amt x)
                       (add xs amt))]))

(add '(1 2 3) 10)

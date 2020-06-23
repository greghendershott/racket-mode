#lang racket/base

;; WIP example of an indenter for sexp langs

(require racket/match
         "token-map.rkt")

(provide indent-line)

(define (indent-line tm pos)
  ;; Keep in mind that token-maps use 1-based positions not 0-based
  (define str (token-map-str tm))
  (define bol (beginning-of-line tm pos))
  (define prev-beg (backward-sexp tm bol))
  (define containing (backward-up tm prev-beg))
  (- prev-beg (beginning-of-line tm prev-beg)))

(define str "#lang racket\n(foo\n  bar\nbaz)\n(foo)")
;;           1234567890123 45678 901234 56789 01234
;;                     1           2           3
(define tm (create str))
(indent-line tm 25)
(indent-line tm 31)

(+ 1
   2)

(+ 1 2 3
   )

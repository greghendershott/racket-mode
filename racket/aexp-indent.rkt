#lang racket/base

(require (only-in "sexp-indent.rkt"
                  [indent-amount sexp:indent-amount]))

(provide indent-amount)

(define (indent-amount tm indent-pos)
  ;; TODO: Special cases, else sexp:indent-amount.
  (sexp:indent-amount tm indent-pos))

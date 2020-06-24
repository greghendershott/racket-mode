#lang racket/base

;; WIP example of an indenter for sexp langs, implemented on a
;; token-map.

(require racket/match
         "token-map.rkt")

(provide indent-amount)

(define (indent-amount tm pos)
  ;; Keep in mind that token-maps use 1-based positions not 0-based
  (match (backward-up tm (beginning-of-line tm pos))
    [(? number? n)
     (define bol (beginning-of-line tm n))
     (define eol (end-of-line tm n))
     (define 1st-sexp (forward-whitespace/comment tm (add1 n)))
     ;; Here we could check the text for 1st-sexp and do the custom
     ;; indent lookup thing. The remaining code here now would be
     ;; the default, no-special-lookup-found case.
     (define 2nd-sexp (backward-sexp tm
                                     (forward-sexp tm
                                                   (forward-sexp tm 1st-sexp))))
     (if (< 2nd-sexp eol) ;2nd on same line as 1st?
         (- 2nd-sexp bol)
         (- 1st-sexp bol))]
    [#f 0]))

(define str "#lang racket\n(foo\n  bar\nbaz)\n(foo bar\nbaz)")
;;           1234567890123 45678 901234 56789 012345678 9012
;;                     1           2           3         4
(define tm (create str))
(indent-amount tm 22)
(indent-amount tm 25)
(indent-amount tm 31)
(indent-amount tm 39) ;35

#;
(+ 1
   2)

#;
(+ 1 2 3
   4)


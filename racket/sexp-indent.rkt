#lang racket/base

;; WIP example of an indenter for sexp langs, implemented on a
;; token-map.

(require racket/match
         "token-map.rkt")

(provide indent-amount)

;; token-map? positive-integer? -> nonnegative-integer?
(define (indent-amount tm pos)
  ;; Keep in mind that token-maps use 1-based positions not 0-based
  (match (backward-up tm (sub1 (beginning-of-line tm pos)))
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

(module+ test
  (require rackunit)
  (define str "#lang racket\n(foo\n  bar\nbaz)\n(foo bar baz\nbap)")
  ;;           1234567890123 45678 901234 56789 012345678 90123456
  ;;                     1           2           3         4
  (define tm (create str))
  (check-equal? (indent-amount tm  1) 0
                "not within any sexpr, should indent 0")
  (check-equal? (indent-amount tm 14) 0
                "not within any sexpr, should indent 0")
  (check-equal? (indent-amount tm 15) 0
                "not within any sexpr, should indent 0")
  (check-equal? (indent-amount tm 22) 1
                "bar should indent with foo")
  (check-equal? (indent-amount tm 25) 1
                "baz should indent with bar (assumes bar not yet re-indented)")
  (check-equal? (indent-amount tm 30) 0
                "not within any sexpr, should indent 0")
  (check-equal? (indent-amount tm 31) 0
                "not within any sexpr, should indent 0")
  (check-equal? (indent-amount tm 43) 5
                "bap should indent with the 2nd sexp on the same line i.e. bar"))

#;
(+ 1
   2)

#;
(+ 1 2 3
   4)


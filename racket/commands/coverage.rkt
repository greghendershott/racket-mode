#lang racket/base

(require racket/list
         racket/match
         (only-in "../instrument.rkt" get-test-coverage-info))

(provide get-uncovered)

(define (get-uncovered file)
  (consolidate-coverage-ranges
   (for*/list ([x (in-list (get-test-coverage-info))]
               [covered? (in-value (first x))]
               #:when (not covered?)
               [src (in-value (second x))]
               #:when (equal? file src)
               [pos (in-value (third x))]
               [span (in-value (fourth x))])
     (cons pos (+ pos span)))))

(define (consolidate-coverage-ranges xs)
  (remove-duplicates (sort xs < #:key car)
                     same?))

(define (same? x y)
  ;; Is x a subset of y or vice versa?
  (match-define (cons x/beg x/end) x)
  (match-define (cons y/beg y/end) y)
  (or (and (<= x/beg y/beg) (<= y/end x/end))
      (and (<= y/beg x/beg) (<= x/end y/end))))

(module+ test
  (require rackunit)
  (check-true (same? '(0 . 9) '(0 . 9)))
  (check-true (same? '(0 . 9) '(4 . 5)))
  (check-true (same? '(4 . 5) '(0 . 9)))
  (check-false (same? '(0 . 1) '(1 . 2)))
  (check-equal? (consolidate-coverage-ranges
                 '((10 . 20) (10 . 11) (19 . 20) (10 . 20)
                   (20 . 30) (20 . 21) (29 . 30) (20 . 30)))
                '((10 . 20)
                  (20 . 30)))
  ;; This is a test of actual coverage data I got from one example,
  ;; where the maximal subsets were (164 . 197) and (214. 247).
  (check-equal?
   (consolidate-coverage-ranges
    '((164 . 197) (164 . 197) (164 . 197)
      (173 . 180) (173 . 180) (173 . 180) (173 . 180) (173 . 180) (187 . 196)
      (214 . 247) (214 . 247) (214 . 247)
      (223 . 230) (223 . 230) (223 . 230) (223 . 230) (223 . 230) (237 . 246)))
   '((164 . 197) (214 . 247))))

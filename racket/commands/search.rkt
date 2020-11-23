#lang racket/base

(require
 racket/list
 racket/format
 racket/contract
 scribble/xref
 scribble/manual-struct
 "help.rkt")

(define index (xref-index xref))

(define (entry-score entry expr)
  (define words (entry-words entry))
  (cond
    [(equal? expr (car words)) 0]
    [else 10000]))

(define from-libs/c
  (or/c symbol?
        (listof symbol?)
        #f))

(define entry/c
  (list/c
   (non-empty-listof string?)
   from-libs/c
   (cons/c path-string? string?)))

(define (entry->list entry)
  (define desc (entry-desc entry))
  (define-values (path anchor) (xref-tag->path+anchor xref (entry-tag entry)))
  (list
   (entry-words entry)
   (if (exported-index-desc? desc)
       (exported-index-desc-from-libs desc)
       desc)
   (cons (~a path) anchor)))

(define/contract (search expr num-results)
  (-> string? integer?
      (listof entry/c))
  (for/fold
      ([results '()]
       ; Keep the length of the list in memory, to avoid O(n) `length` operation.
       [len 0]
       #:result
       (map
        (compose entry->list cdr)
        results))
      ([entry (in-list index)])
    (define sorted
      (sort
       (cons
        (cons (entry-score entry expr) entry)
        results)
       less-than?))
    (if (> num-results len)
        (values sorted (add1 len))
        (values (take sorted num-results) len))))

(define (less-than? x y)
  (< (car x) (car y)))

(provide search)

#lang racket/base

(require
 racket/string
 racket/function
 racket/list
 racket/format
 racket/contract
 scribble/xref
 scribble/manual-struct
 "help.rkt")

(define index (xref-index xref))

(define (string->sub-words str)

  (define (pair->list pair)
    (list (car pair) (cdr pair)))

  (define points
    (append-map
     pair->list
     ; Use any punctuation as a delimiter
     (regexp-match-positions* #px"\\p{P}+" str)))

  (for/list
      ([start (cons 0 points)]
       [end (append points (list (string-length str)))]
       #:when (not (eq? start end)))
    (substring str start end)))

; Variant of `filter-not` which only removes a single element
(define (filter-not-1 f lst)
  (define (loop lst result)
    (cond
      [(null? lst)
       (reverse result)]
      [(f (car lst))
       (append
        (reverse result)
        (cdr lst))]
      [else
       (loop (cdr lst) (cons (car lst) result))]))
  (loop lst '()))

(define (entry-score entry expr)
  (define words (entry-words entry))
  ; This algorithm is roughly-speaking the same one as
  ; found in `search.js` which is responsible for the search on the manual.
  (cond
    [(equal? expr (car words)) 0]
    [(string-prefix? (car words) expr) 1]
    [(string-contains? (car words) expr) 2]
    [else
     (let*
         ([expr-sub-words (string->sub-words expr)]
          [entry-sub-words (string->sub-words (car words))]
          [matched? (for/and
                        ([sub-word (in-list expr-sub-words)])
                      (member sub-word entry-sub-words))]
          [same-length? (eq? (length expr-sub-words)
                             (length entry-sub-words))]
          [complete? (for/fold
                         ([sub-words entry-sub-words])
                         ([sub-word (in-list expr-sub-words)])
                         (filter-not-1
                          (curry equal? sub-word)
                          sub-words))])
       (cond
         [(and matched? same-length? complete?) 3]
         [(and matched? same-length?) 4]
         [matched? 5]
         [else 6]))]))

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
    (define score
      (entry-score entry expr))
    (define-values (sorted sorted-len)
      (if (< score 6)
          (values
           (sort
            (cons
             (cons score entry)
             results)
            less-than?)
           (add1 len))
          (values results len)))
    (if (> num-results sorted-len)
        (values sorted sorted-len)
        (values (take sorted num-results) len))))

(define (less-than? x y)
  (< (car x) (car y)))

(provide search)

(module+ test
  (require rackunit)
  (check-equal? (string->sub-words "foo-bar") '("foo" "-" "bar"))
  (check-equal? (string->sub-words "foo") '("foo"))
  (check-equal? (string->sub-words "-") '("-"))

  (check-equal? (filter-not-1 (curry eq? 1) '(1 1 2 3 4)) '(1 2 3 4))
  (check-equal? (filter-not-1 (curry eq? 3) '(1 2 3 4)) '(1 2 4))

  (check-true (list? (search "set!" 1))))

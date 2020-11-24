#lang racket/base

(require
 racket/string
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

(define (entry-score entry expr)
  (define words (entry-words entry))
  ; This algorithm is roughly-speaking the same one as
  ; found in `search.js` which is responsible for the search on the manual.
  (cond
    [(equal? expr (car words)) 0]
    [(string-prefix? expr (car words)) 1]
    [(string-contains? expr (car words)) 2]
    [else
     (let*
         ([expr-sub-words (string->sub-words expr)]
          [entry-sub-words (string->sub-words (car words))]
          [matched? (for/and
                        ([sub-word (in-list expr-sub-words)])
                      (member sub-word entry-sub-words))]
          [same-length? (eq? (length expr-sub-words)
                             (length entry-sub-words))])
       (cond
         [(and matched? same-length?) 3]
         [matched? 4]
         [else 5]))]))

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

(module+ test
  (require rackunit)
  (check-equal? (string->sub-words "foo-bar") '("foo" "-" "bar"))
  (check-equal? (string->sub-words "foo") '("foo"))
  (check-equal? (string->sub-words "-") '("-"))

  (check-true (list? (search "set!" 1))))

#lang racket/base

(require racket/contract
         racket/stream
         "core.rkt"
         (submod "core.rkt" private))

(provide in-tokens-forward
         in-tokens-backward)

(struct forward (tm b+t)
  #:methods gen:stream
  [(define (stream-empty? this)
     (not (forward-b+t this)))
   (define (stream-first this)
     (forward-b+t this))
   (define (stream-rest this)
     (define tm (forward-tm this))
     (define b+t (forward-b+t this))
     (forward tm
              (token-map-ref tm (bounds+token-end b+t))))])

(define/contract (in-tokens-forward tm pos)
  (-> token-map? position/c sequence?)
  (in-stream (forward tm (token-map-ref tm pos))))

(struct backward (tm b+t)
  #:methods gen:stream
  [(define (stream-empty? this)
     (not (backward-b+t this)))
   (define (stream-first this)
     (backward-b+t this))
   (define (stream-rest this)
     (define tm (backward-tm this))
     (define b+t (backward-b+t this))
     (backward tm
               (token-map-ref tm (sub1 (bounds+token-beg b+t)))))])

(define/contract (in-tokens-backward tm pos)
  (-> token-map? position/c sequence?)
  (in-stream (backward tm (token-map-ref tm (sub1 pos)))))

(module+ test
  (require rackunit)
  (define str "#lang racket\n42 #:keyword\n\"string\"\nsymbol")
  (define max-pos (string-length str)) ;pos 1.. but str 0..
  (define tm (create str))
  (define forward (for/list ([v (in-tokens-forward tm 1)])
                    v))
  (define backward (for/list ([v (in-tokens-backward tm max-pos)])
                     v))
  (check-equal? forward
                (list
                 (bounds+token 1  13 (token:misc "#lang racket" 0 'other))
                 (bounds+token 13 14 (token:misc "\n" 0 'end-of-line))
                 (bounds+token 14 16 (token:misc "42" 0 'constant))
                 (bounds+token 16 17 (token:misc " " 0 'white-space))
                 (bounds+token 17 26 (token:misc "#:keyword" 0 'hash-colon-keyword))
                 (bounds+token 26 27 (token:misc "\n" 0 'end-of-line))
                 (bounds+token 27 35 (token:misc "\"string\"" 0 'string))
                 (bounds+token 35 36 (token:misc "\n" 0 'end-of-line))
                 (bounds+token 36 42 (token:misc "symbol" 0 'symbol))))
  (check-equal? forward (reverse backward))
  ;; Positions that satisfy position/c but are out of range simply
  ;; produce empty streams
  (check-equal? (for/list ([v (in-tokens-forward tm (add1 max-pos))]) v)
                (list))
  (check-equal? (for/list ([v (in-tokens-forward tm 9999)]) v)
                (list))
  (check-equal? (for/list ([v (in-tokens-backward tm 1)]) v)
                (list)))

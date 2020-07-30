#lang racket/base

;; "Navigation": Useful for a lang indenter -- these roughly
;; correspond to methods from text<%> that an indenter might need --
;; as well as an end user text editor. Note that these work in terms
;; of open and close tokens -- not necessarily traditional lisp
;; s-expressions.

(require racket/contract
         racket/match
         "core.rkt"
         (submod "core.rkt" private))

(provide beg-of-line
         end-of-line
         backward-up
         forward-whitespace
         forward-whitespace/comment
         backward-whitespace/comment
         forward-sexp
         backward-sexp)

(module+ test (require rackunit))

(define navigator/c (-> token-map? (or/c #f position/c)
                        (or/c #f position/c)))

(define/contract (beg-of-line tm start-pos) navigator/c
  (let loop ([pos start-pos])
    (match (token-map-ref tm pos)
      [(bounds+token beg end (? token:misc? t))
       #:when (and (eq? (token:misc-kind t) 'end-of-line)
                   ( < beg start-pos))
       end]
      [(bounds+token beg _end (? token?))
       (loop (sub1 beg))]
      [#f #:when (< 1 pos) (loop (sub1 pos))]
      [#f 1])))

(define/contract (end-of-line tm pos) navigator/c
  (let loop ([pos pos])
    (match (token-map-ref tm pos)
      [(bounds+token _beg end (? token:misc? t))
       #:when (eq? (token:misc-kind t) 'end-of-line)
       end]
      [(bounds+token _beg end (? token?))
       (loop end)]
      [#f (add1 (string-length (token-map-str tm)))])))

(define/contract (backward-up tm pos) navigator/c
  (let loop ([pos pos])
    (match (backward-sexp tm pos list)
      [(list 1) #f]
      [(list pos) pos]
      [(? number? pos) (loop pos)])))

(define/contract (forward-whitespace tm pos) navigator/c
  (match (token-map-ref tm pos)
    [#f #f]
    [(bounds+token beg end (token:misc _lexeme _backup 'white-space))
     (forward-whitespace tm end)]
    [_ pos]))

(define/contract (forward-whitespace/comment tm pos) navigator/c
  (match (token-map-ref tm pos)
    [#f #f]
    [(bounds+token beg end (token:misc _lexeme _backup (or 'end-of-line
                                                            'white-space
                                                            'comment
                                                            'sexp-comment)))
     (forward-whitespace/comment tm end)]
    [_ pos]))

(define/contract (backward-whitespace/comment tm pos) navigator/c
  (match (token-map-ref tm pos)
    [#f #f]
    [(bounds+token beg end (token:misc _lexme _backup (or 'end-of-line
                                                          'white-space
                                                          'comment
                                                          'sexp-comment)))
     (backward-whitespace/comment tm (sub1 beg))]
    [_ pos]))

(define navigator/fail/c
  (->* (token-map? (or/c #f position/c)) ((-> position/c any/c))
       (or/c (or/c #f position/c)
             any/c)))

(define/contract (forward-sexp tm pos [fail (λ (_pos) #f)]) navigator/fail/c
  (match (forward-whitespace/comment tm pos)
    [#f (fail pos)]
    [pos
     (match (token-map-ref tm pos)
       ;; Close token: Fail with position of failure.
       [(bounds+token _beg end (? token:close?))
        (fail end)]
       ;; Open token: Scan for matching close token.
       [(bounds+token beg end (? token:open? open-t))
        (let loop ([pos pos]
                   [depth 0])
          ;;(println (list pos depth (interval-map-ref im pos)))
          (match (token-map-ref tm pos)
            [#f (fail pos)]
            [(bounds+token _beg end (? token:open? t))
             #:when (equal? (token-lexeme open-t)
                            (token-lexeme t))
             (loop end
                   (add1 depth))]
            [(bounds+token _beg end (? token:close? t))
             #:when (equal? (token-lexeme open-t)
                            (token:close-open t))
             (if (= depth 1)
                 end
                 (loop end
                       (sub1 depth)))]
            [(bounds+token _beg end (? token?))
             (loop end
                   depth)]))]
       ;; Some other non-white-space/comment token. Simply use last
       ;; char pos.
       [(bounds+token _beg end (? token?))
        end])]))

(define/contract (backward-sexp tm pos [fail (λ (_pos) #f)]) navigator/fail/c
  (match (and pos
              (< 1 pos)
              (backward-whitespace/comment tm (and pos (sub1 pos))))
    [#f (fail 1)]
    [pos
     (match (token-map-ref tm pos)
      ;; Open token: Fail with position of failure.
      [(bounds+token beg _end (? token:open?))
       (fail beg)]
      ;; Close token: Scan for matching open token.
      [(bounds+token _beg _end (? token:close? close-t))
       (let loop ([pos pos]
                  [depth 0])
         ;;(println (list pos depth (interval-map-ref im pos)))
         (match (token-map-ref tm pos)
           [#f (fail 1)]
           [(bounds+token beg _end (? token:open? t))
            #:when (equal? (token:close-open close-t)
                           (token-lexeme t))
            (if (= depth 1)
                beg
                (loop (sub1 beg)
                      (sub1 depth)))]
           [(bounds+token beg _end (? token:close? t))
            #:when (equal? (token-lexeme close-t)
                           (token-lexeme t))
            (loop (sub1 beg)
                  (add1 depth))]
           [(bounds+token beg _end (? token?))
            (loop (sub1 beg)
                  depth)]))]
      ;; Some other token. Simply use first char pos.
      [(bounds+token beg _end (? token?))
       beg])]))

(module+ test
  (let* ([str "#lang racket\n(a (b (c  foo))) (bar ((x)) y)\n"]
         ;;    1234567890123 4567890123456789012345678901234
         ;;             1          2         3         4
         [tm (create str)])
    (check-equal? (classify tm 1)
                  (bounds+token 1 13 (token:misc "#lang racket" 0 'other)))
    (check-equal? (classify tm 13)
                  (bounds+token 13 14 (token:misc "\n" 0 'end-of-line)))
    (check-equal? (beg-of-line tm 1) 1)
    (check-equal? (beg-of-line tm 2) 1)
    (check-equal? (beg-of-line tm 3) 1)
    (check-equal? (beg-of-line tm 13) 1)
    (check-equal? (beg-of-line tm 14) 14)
    (check-equal? (beg-of-line tm 15) 14)
    (check-equal? (beg-of-line tm 44) 14)
    (check-equal? (backward-up tm 16) 14)
    (check-equal? (backward-up tm 17) 14)
    (check-equal? (backward-up tm 18) 17)
    (check-equal? (backward-up tm 20) 17)
    (check-equal? (backward-up tm 22) 20)
    (check-equal? (backward-up tm 34) 31)
    (check-equal? (backward-up tm 42) 31)
    (check-equal? (backward-up tm 43) 31)
    (check-false  (backward-up tm  1))
    (check-false  (backward-up tm 12))
    (check-false  (backward-up tm 13))
    (check-false  (backward-up tm 14))
    (check-false  (backward-up tm 30))
    (check-false  (backward-up tm 31))
    (check-equal? (forward-whitespace/comment tm 23) 24)
    (check-equal? (backward-whitespace/comment tm 22) 21)
    (check-equal? (forward-sexp tm 14) 30)
    (check-equal? (forward-sexp tm 24) 27)
    (check-equal? (backward-sexp tm  1) #f)
    (check-equal? (backward-sexp tm  1 values) 1)
    (check-equal? (backward-sexp tm 14)  1)
    (check-equal? (backward-sexp tm 17) 15)
    (check-equal? (backward-sexp tm 26) 24)
    (check-equal? (backward-sexp tm 28) 20)
    (check-equal? (backward-sexp tm 29) 17)
    (check-equal? (backward-sexp tm 30) 14)))

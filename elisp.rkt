#lang racket/base

(require racket/format
         racket/match)

(provide elisp-read
         elisp-print
         elisp-println
         ->elisp)

(module+ test
  (require rackunit
           racket/port))

;;; read elisp values

(define (elisp-read)
  ;; Elisp prints '() as 'nil. Reverse that. (Assumption: Although
  ;; some Elisp code puns nil/() also to mean "false", _our_ Elisp
  ;; code _won't_ do that.)
  (match (read)
    ['nil '()]
    [x x]))

(module+ test
  (check-equal? (with-input-from-string "nil" elisp-read)
                '())
  (check-equal? (with-input-from-string "()" elisp-read)
                '())
  (check-equal? (with-input-from-string "42" elisp-read)
                42))

;;; print elisp values

(define (elisp-println v)
  (elisp-print v)
  (newline))

(define (elisp-print v)
  (print (->elisp v)))

(define (->elisp v)
  (match v
    [(or #f (list))   'nil]
    [#t               't]
    [(? list? xs)     (map ->elisp xs)]
    [(cons x y)       (cons (->elisp x) (->elisp y))]
    [(? path? p)      (path->string p)]
    [(? struct? x)    (~a x)]
    [(? procedure? x) (~a x)]
    [(? hash? ht)     (hash->list ht)]
    [v                v]))

(module+ test
  (check-equal? (with-output-to-string
                  (λ ()
                    (elisp-println `(1 #t nil () (a . b)))))
                "'(1 t nil nil (a . b))\n")
  (check-equal? (with-output-to-string
                  (λ ()
                    (elisp-println (string->path "/tmp/foo"))))
                "\"/tmp/foo\"\n")
  (check-equal? (with-output-to-string
                  (λ ()
                    (struct foo (a b))
                    (struct bar (a b) #:transparent)
                    (elisp-println (list + (foo 1 2) (bar 1 2)))))
                "'(\"#<procedure:+>\" #<foo> \"#(struct:bar 1 2)\")\n")
  (check-equal? (with-output-to-string
                  (λ ()
                    (elisp-println (hash 'a 0 'b 1))))
                "'((a . 0) (b . 1))\n"))

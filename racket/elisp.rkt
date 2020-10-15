#lang racket/base

(require racket/contract
         racket/match
         racket/port
         racket/set)

(provide elisp-read
         elisp-bool/c
         as-racket-bool
         elisp-writeln)

;;; Read a subset of Emacs Lisp values as Racket values

(define (elisp-read in)
  (elisp->racket (read in)))

(define (elisp->racket v)
  (match v
    ['nil             '()] ;not #f -- see as-racket-bool
    ['t               #t]
    [(? list? xs)     (map elisp->racket xs)]
    [(cons x y)       (cons (elisp->racket x) (elisp->racket y))]
    [(vector s _ ...) s] ;Emacs strings can be #("string" . properties)
    [v                v]))

(define elisp-bool/c (or/c #t '()))
(define (as-racket-bool v)
  ;; elisp->racket "de-puns" 'nil as '() -- not #f. Use this helper when
  ;; instead you want to treat it as a boolean and get #f.
  (and v (not (null? v))))

;;; Write a subset of Racket values as Emacs Lisp values

(define (elisp-writeln v out)
  (elisp-write v out)
  (newline out))

(define (elisp-write v out)
  (write (racket->elisp v) out))

(define (racket->elisp v)
  (match v
    [(or #f (list))     'nil]
    [#t                 't]
    [(? list? xs)       (map racket->elisp xs)]
    [(cons x y)         (cons (racket->elisp x) (racket->elisp y))]
    [(? path? v)        (path->string v)]
    [(? hash? v)        (for/list ([(k v) (in-hash v)])
                          (cons (racket->elisp k) (racket->elisp v)))]
    [(? generic-set? v) (map racket->elisp (set->list v))]
    [(? void?)          'void] ;avoid Elisp-unreadable "#<void>"
    [v                  v]))

(module+ test
  (require rackunit)
  (check-equal? (with-output-to-string
                  (λ () (elisp-write '(1 #t nil () (a . b) #hash((1 . 2) (3 . 4)))
                                     (current-output-port))))
                "(1 t nil nil (a . b) ((1 . 2) (3 . 4)))"))

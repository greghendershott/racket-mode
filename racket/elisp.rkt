#lang racket/base

(require racket/contract
         racket/match
         racket/port
         racket/set
         racket/string)

(provide elisp-read
         elisp-writeln
         elisp-bool/c
         as-racket-bool)

;;; read/write Emacs Lisp values

(define (elisp-read in)
  (elisp->racket (read in)))

(define (elisp-writeln v out)
  (elisp-write v out)
  (newline out))

(define (elisp-write v out)
  (write (racket->elisp v) out))

(define elisp-bool/c (or/c #t '()))
(define (as-racket-bool v)
  ;; elisp->racket "de-puns" 'nil as '() -- not #f. Use this helper to
  ;; treat as a boolean.
  (and v (not (null? v))))

(define (elisp->racket v)
  (match v
    ['nil             '()] ;not #f -- see as-racket-bool
    ['t               #t]
    [(? list? xs)     (map elisp->racket xs)]
    [(cons x y)       (cons (elisp->racket x) (elisp->racket y))]
    [(vector s _ ...) s] ;Emacs strings can be #("string" . properties)
    [v                v]))

(define (racket->elisp v)
  (match v
    [(or #f (list)) 'nil]
    [#t             't]
    [(? list? xs)   (map racket->elisp xs)]
    [(cons x y)     (cons (racket->elisp x) (racket->elisp y))]
    [(? path? v)    (path->string/emacs v)]
    [(? hash? v)    (for/list ([(k v) (in-hash v)])
                      (cons (racket->elisp k) (racket->elisp v)))]
    [(? set? v)     (map racket->elisp (set->list v))]
    [v              v]))

(module+ test
  (require rackunit)
  (check-equal? (with-output-to-string
                  (λ () (elisp-write '(1 #t nil () (a . b) #hash((1 . 2) (3 . 4)))
                                     (current-output-port))))
                "(1 t nil nil (a . b) ((1 . 2) (3 . 4)))"))

(define (path->string/emacs p)
  (when (string? p)
    (set! p (string->path p)))
  (match (map path->string (explode-path p))
    [(list* (pregexp "([a-zA-z]:)\\\\" (list _ drive)) vs)
     #:when (eq? 'windows (system-type))
     (string-join (cons drive vs))]
    [_ (path->string p)]))

(module+ test
  (when (eq? 'windows (system-type))
    (check-equal? (path->string/emacs "C:\\path\\to\\foo.rkt")
                  "C:/path/to/foo.rkt"))
  (check-equal? (path->string/emacs "/path/to/foo.rkt")
                "/path/to/foo.rkt")
  (check-equal? (path->string/emacs (string->path "/path/to/foo.rkt"))
                "/path/to/foo.rkt"))

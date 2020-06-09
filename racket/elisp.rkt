;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/match
         racket/port
         racket/set
         syntax/parse/define)

(provide elisp-read
         elisp-bool/c
         as-racket-bool
         with-parens
         elisp-write
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

(define (elisp-writeln v)
  (elisp-write v)
  (newline))

(define-simple-macro (with-parens e:expr ...+)
  (begin (display "(")
         e ...
         (display ")")))

(define (elisp-write v)
  (match v
    [(or #f (list))     (write 'nil)]
    [#t                 (write 't)]
    [(? list? xs)       (with-parens
                          (for-each (λ (v)
                                      (elisp-write v)
                                      (display " "))
                                    xs))]
    [(cons x y)         (with-parens
                          (elisp-write x)
                          (display " . ")
                          (elisp-write y))]
    [(? path? v)        (elisp-write (path->string v))]
    [(? hash? v)        (with-parens
                          (hash-for-each v
                                         (λ (k v)
                                           (elisp-write (cons k v))
                                           (display " "))))]
    [(? generic-set? v) (with-parens
                          (set-for-each v
                                        (λ (v)
                                          (elisp-write v)
                                          (display " "))))]
    [(? void?)          (display "void")] ;avoid Elisp-unreadable "#<void>"
    [(? procedure? w)   (w)]
    [(or (? number? v)
         (? symbol? v)
         (? string? v)) (write v)]
    [(? bytes? bstr)    (write (bytes->string/utf-8 bstr))] ; ???
    [v                  (eprintf "elisp-write can't write Racket value ~v\n" v)
                        (void)]))

(module+ test
  (require rackunit)
  (check-equal? (with-output-to-string
                  (λ () (elisp-write '(1 #t nil () (a . b) #hash((1 . 2) (3 . 4))))))
                "(1 t nil nil (a . b) ((1 . 2) (3 . 4) ) )"))

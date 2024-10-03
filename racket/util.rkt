;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define
         racket/format)

(provide string->namespace-syntax
         syntax-or-sexpr->syntax
         syntax-or-sexpr->sexpr
         nat/c
         pos/c
         memq?
         log-racket-mode-debug
         log-racket-mode-info
         log-racket-mode-warning
         log-racket-mode-error
         log-racket-mode-fatal
         time-apply/log
         with-time/log
         define-polyfill
         define-fallbacks)

(define (string->namespace-syntax str)
  (namespace-syntax-introduce
   (read-syntax #f (open-input-string str))))

(define (syntax-or-sexpr->syntax v)
  (if (syntax? v)
      v
      (namespace-syntax-introduce (datum->syntax #f v))))

(define (syntax-or-sexpr->sexpr v)
  (if (syntax? v)
      (syntax-e v)
      v))

(define nat/c exact-nonnegative-integer?)
(define pos/c exact-positive-integer?)

(define (memq? x xs)
  (and (memq x xs) #t))

;;; logger / timing

(define-logger racket-mode)

(define (time-apply/log what proc args)
  (define-values (vs cpu real gc) (time-apply proc args))
  (define (fmt n) (~v #:align 'right #:min-width 4 n))
  (log-racket-mode-debug "~a cpu | ~a real | ~a gc :: ~a"
                         (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

;;; dynamic-require with backup implementation

(define-simple-macro (define-polyfill (id:id arg:expr ...)
                       #:module mod:id
                       body:expr ...+)
  (define id
    (with-handlers ([exn:fail? (λ (_exn)
                                 (λ (arg ...) body ...))])
      (dynamic-require 'mod 'id))))

;;; similar, but defining fallbacks only for items not imported by a
;;; normal require

;; advantage: Things like go-to definition work as expected in the
;; normal, non-fallback case. Whereas with define-polyfill, the
;; identifier _always_ results from the dynamic-require, and go-to def
;; always goes there.

(define-syntax-parser define-fallback
  [(_ mod:id (id:id arg:expr ...) body:expr ...+)
   (if (with-handlers ([exn:fail? (λ _ #f)])
         (dynamic-require (syntax-e #'mod) (syntax-e #'id)))
       #'(void)
       #'(define (id arg ...)
           body ...))])

(define-syntax-parser define-fallbacks
  [(_ mod:id [(id:id arg:expr ...) body:expr ...+] ...+)
   #'(begin
       (define-fallback mod (id arg ...) body ...) ...)])


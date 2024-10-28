;; Copyright (c) 2013-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define
         racket/format
         "define-fallbacks.rkt"
         "safe-dynamic-require.rkt")

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
         (all-from-out "define-fallbacks.rkt")
         (all-from-out "safe-dynamic-require.rkt"))

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
  (define (fmt n) (~s #:align 'right #:min-width 4 n))
  (log-racket-mode-debug "~a cpu | ~a real | ~a gc :: ~a"
                         (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

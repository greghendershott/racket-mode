;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/logging
         racket/match
         racket/set
         syntax/parse/define
         "util.rkt")

(provide current-online-check-syntax
         with-online-check-syntax)

;;; online-check-syntax logger monitor

;; There exists a protocol for macros to communicate tooltips to
;; DrRacket via a log-message to the logger 'online-check-syntax. This
;; might seem strange, but one motivation for this protocol is that
;; e.g. a type-checker might learn things during expansion that it
;; would like to show the user -- even if expansion fails.

(define current-online-check-syntax (make-parameter (mutable-set)))

(define-simple-macro (with-online-check-syntax source:expr e:expr ...+)
  (call-with-online-check-syntax source (λ () e ...)))

(define (call-with-online-check-syntax source proc)
  (current-online-check-syntax (mutable-set)) ;reset
  (with-intercepted-logging (make-interceptor source) proc
    'info 'online-check-syntax))

(define ((make-interceptor src) event)
  (match-define (vector _level _message stxs _topic) event)
  (for ([stx (in-list stxs)])
    (let walk ([v (syntax-property stx 'mouse-over-tooltips)])
      (match v
        ;; "The value of the 'mouse-over-tooltips property is
        ;; expected to be to be a tree of cons pairs (in any
        ;; configuration)..."
        [(cons v more)
         (walk v)
         (walk more)]
        ;; "...whose leaves are either ignored or are vectors of the
        ;; shape:"
        [(vector (? syntax? stx)
                 (? exact-positive-integer? beg)
                 (? exact-positive-integer? end)
                 (or (? string? string-or-thunk)
                     (? procedure? string-or-thunk)))
         (when (equal? src (syntax-source stx))
           ;; Force now; the resulting string will likely use less
           ;; memory than a thunk closure.
           (define (force v) (if (procedure? v) (v) v))
           (define str (force string-or-thunk))
           (set-add! (current-online-check-syntax)
                     (list beg end str)))]
        ;; Expected; quietly ignore
        [(or (list) #f) (void)]
        ;; Unexpected; log warning and ignore
        [v (log-racket-mode-warning "unknown online-check-syntax ~v" v)
           (void)]))))

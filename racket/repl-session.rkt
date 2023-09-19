;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require racket/format
         racket/match
         "util.rkt")

(provide next-session-id!
         call-with-session-context
         current-session-id
         current-repl-msg-chan
         current-session-maybe-mod
         (struct-out session)
         get-session
         set-session!
         remove-session!)

;;; REPL session "housekeeping"

;; Session IDs are strings based on time + monotonic number
(define next-session-id!
  (let ([n 0])
    (λ ()
      (format "repl-session-~a-~a"
              (current-inexact-milliseconds)
              (begin0 n
                (inc! n))))))

;; Each REPL session has an entry in this hash-table.
(define sessions (make-hash)) ;string? => session?

(struct session
  (thread           ;thread? the repl manager thread
   repl-msg-chan    ;channel?
   maybe-mod        ;(or/c #f module-path?)
   namespace)
  #:transparent)

(define (get-session sid)
  (hash-ref sessions sid #f))

(define (set-session! sid maybe-mod)
  (hash-set! sessions sid (session (current-thread)
                                   (current-repl-msg-chan)
                                   maybe-mod
                                   (current-namespace)))
  (log-racket-mode-debug @~a{(set-session! @~v[sid] @~v[maybe-mod]) => sessions: @~v[sessions]}))

(define (remove-session! sid)
  (hash-remove! sessions sid)
  (log-racket-mode-debug @~a{(remove-session! @~v[sid]) => sessions: @~v[sessions]}))

(define current-session-id (make-parameter #f))
(define current-repl-msg-chan (make-parameter #f))
(define current-session-maybe-mod (make-parameter #f))

;; A way to parameterize e.g. commands that need to work with a
;; specific REPL session. Called from e.g. a command-server thread.
(define (call-with-session-context sid proc . args)
  (match (get-session sid)
    [(? session? s)
     (log-racket-mode-debug @~a{@car[args]: using session ID @~v[sid]})
     (parameterize ([current-session-id          sid]
                    [current-repl-msg-chan       (session-repl-msg-chan s)]
                    [current-session-maybe-mod   (session-maybe-mod s)]
                    [current-namespace           (session-namespace s)])
       (apply proc args))]
    [_
     (unless (equal? sid '())
       (log-racket-mode-warning @~a{@car[args]: session ID @~v[sid] not found}))
     (apply proc args)]))

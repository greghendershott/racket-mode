;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require racket/match
         racket/format)

(provide (rename-out [command-channel logger-command-channel]
                     [notify-channel logger-notify-channel]))

;; "On start-up, Racket creates an initial logger that is used to
;; record events from the core run-time system. For example, an 'debug
;; event is reported for each garbage collection (see Garbage
;; Collection)." Use that; don't create new one. See issue #325.
(define global-logger (current-logger))

(define command-channel (make-channel))
(define notify-channel (make-channel))

;; Go ahead and start our log receiver thread early so we can see our
;; own racket-mode topic's 'debug level ouput in the front end.
;;
;; On the other hand (see #631) set all other topics to the 'fatal
;; level (least noisy). This avoids sending excessive logger
;; notifications to the front end, until/unless it gives us the user's
;; logger configuration, with whatever verbosity they desire.
(define (racket-mode-log-receiver-thread)
  (let wait ([receiver (make-receiver '((racket-mode . debug)
                                        (*           . fatal)))])
    (sync
     (handle-evt command-channel
                 (λ (v)
                   (wait (make-receiver v))))
     (handle-evt receiver
                 (match-lambda
                   [(vector level message _v topic)
                    (channel-put notify-channel
                                 `(logger
                                   ,(~a (label level) " "
                                        (ensure-topic-in-message topic message)
                                        "\n")))
                    (wait receiver)])))))
(void (thread racket-mode-log-receiver-thread))

(define (ensure-topic-in-message topic message)
  (match message
    [(pregexp (format "^~a: " (regexp-quote (~a topic))))
     message]
    [message-without-topic
     (format "~a: ~a" (or topic "*") message-without-topic)]))

(module+ test
  (require rackunit)
  (check-equal? (ensure-topic-in-message 'topic "topic: message")
                "topic: message")
  (check-equal? (ensure-topic-in-message 'topic "message")
                "topic: message")
  (check-equal? (ensure-topic-in-message #f "message")
                "*: message"))

(define (label level)
  ;; justify
  (case level
    [(debug)   "[  debug]"]
    [(info)    "[   info]"]
    [(warning) "[warning]"]
    [(error)   "[  error]"]
    [(fatal)   "[  fatal]"]
    [else      @~a{[level]}]))

(define (make-receiver alist)
  (apply make-log-receiver (list* global-logger
                                  (alist->spec alist))))

;; Convert from ([logger . level] ...) alist to the format used by
;; make-log-receiver: (level logger ... ... default-level). In the
;; alist, treat the logger '* as the default level.
(define (alist->spec xs) ;(Listof (Pairof Symbol Symbol)) -> (Listof Symbol)
  (for/fold ([spec '()])
            ([x (in-list xs)])
    (append spec
            (match x
              [(cons '*     level) (list level)]
              [(cons logger level) (list level logger)]))))

#lang at-exp racket/base

(require racket/match
         "util.rkt")

(provide (rename-out [notify-channel trace-notify-channel]))

(define notify-channel (make-channel))

(define topic 'vestige-trace)
(define level 'debug)
(define receiver (make-log-receiver (current-logger) level topic 'none #f))

(define (trace-thread)
  (match (sync receiver)
    [(vector (== level)
             _message
             (hash-table ['kind      kind]
                         ['show      show]
                         ['name      name]
                         ['level     level]
                         ['def-site  def-site]
                         ['call-site call-site])
             (== topic))
     (channel-put notify-channel
                  `(trace
                    ,kind
                    ,show
                    ,name
                    ,level
                    ,def-site
                    ,call-site))]
    [data
     (log-racket-mode-debug "unexpected data for ~v: ~v" topic data)])
  (trace-thread))

;; Go ahead and start this early.
(void (thread trace-thread))


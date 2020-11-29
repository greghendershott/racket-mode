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
             (hash-table ['call       call]
                         ['tail       tail]
                         ['show       show]
                         ['name       name]
                         ['level      level]
                         ['definition definition]
                         ['signature  signature]
                         ['caller     caller]
                         ['context    context]
                         ['thread     thread]
                         ['msec       msec])
             (== topic))
     (channel-put notify-channel
                  `(trace
                    ,call
                    ,tail
                    ,show
                    ,name
                    ,level
                    ,definition
                    ,signature
                    ,caller
                    ,context
                    ,thread
                    ,msec))]
    [data
     (log-racket-mode-error "unexpected data for ~v: ~v" topic data)])
  (trace-thread))

;; Go ahead and start this early.
(void (thread trace-thread))


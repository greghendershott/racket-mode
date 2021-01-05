#lang at-exp racket/base

(require racket/match
         racket/format
         "util.rkt")

(provide (rename-out [command-channel logger-command-channel]
                     [notify-channel logger-notify-channel]))

;; "On start-up, Racket creates an initial logger that is used to
;; record events from the core run-time system. For example, an 'debug
;; event is reported for each garbage collection (see Garbage
;; Collection)." Use that; don't create new one. See issue #325.
(define global-logger (current-logger))

(define command-channel (make-channel))
(define notify-channel (make-channel))

(define (logger-thread)
  (let wait ([receiver (make-receiver '((racket-mode . debug)
                                        (*           . warning)))])
    (sync (handle-evt command-channel
                      (λ (v)
                        (wait (make-receiver v))))
          (handle-evt receiver
                      (λ (v)
                        (channel-put notify-channel
                                     `(logger ,(vector->notify-value v)))
                        (wait receiver))))))

(define (vector->notify-value vec)
  (match (add-presentation-sites
          (log-receiver-vector->hasheq vec))
    [(and ht
          (hash-table ['level   level]
                      ['topic   topic]
                      ['message message]
                      ['depth   depth]
                      ['caller  caller]
                      ['context context]
                      ['info    info]
                      ['tracing tracing]))
     (define (lookup ht key #:coerce [coerce values] #:default [default #f])
       (and ht
            (cond [(hash-ref ht key #f) => coerce]
                  [else default])))
     (list level
           (~a (or topic "*"))
           (remove-topic-from-message topic message)
           depth
           context
           (lookup info 'msec)
           (lookup info 'thread #:coerce object-name)
           (and tracing #t)
           (lookup tracing 'call)
           (lookup tracing 'tail)
           (lookup ht 'primary-site)
           (lookup ht 'secondary-site))]))

(define-polyfill (log-receiver-vector->hasheq v)
  #:module vestige/receiving
  (match v
    [(vector level message _data topic)
     (hasheq 'message message
             'topic   topic
             'level   level
             'depth   0
             'caller  #f
             'context #f
             'info    #f
             'tracing #f)]))

(define-polyfill (add-presentation-sites ht)
  #:module vestige/receiving
  ht)

(define (remove-topic-from-message topic message)
  (match message
    [(pregexp (format "^~a: (.*)$" (regexp-quote (~a topic)))
              (list _ m))
     m]
    [m m]))

(module+ test
  (require rackunit)
  (check-equal? (remove-topic-from-message 'topic "topic: message")
                "message")
  (check-equal? (remove-topic-from-message 'topic "message")
                "message")
  (check-equal? (remove-topic-from-message #f "message")
                "message"))

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

;; Go ahead and start this early so we can see our own
;; log-racket-mode-xxx ouput in the front end.
(void (thread logger-thread))

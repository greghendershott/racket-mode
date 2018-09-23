#lang at-exp racket/base

(require racket/match
         racket/format
         racket/tcp
         "elisp.rkt"
         "util.rkt")

(provide start-logger-server)

;; "On start-up, Racket creates an initial logger that is used to
;; record events from the core run-time system. For example, an 'debug
;; event is reported for each garbage collection (see Garbage
;; Collection)." Use that; don't create new one. See issue #325.
(define global-logger (current-logger))

(define (start-logger-server port launch-token)
  (void (thread (logger-thread port launch-token))))

(define ((logger-thread port launch-token))
  (define listener (tcp-listen port 4 #t "127.0.0.1"))
  (let accept ()
    (define-values (in out) (tcp-accept listener))
    (unless (or (not launch-token)
                      (equal? launch-token (elisp-read in)))
            (display-commented "Authorization failed; exiting")
            (exit 1))
    ;; Assumption: Any network fail means the client has disconnected,
    ;; therefore we should go back to waiting to accept a connection.
    (with-handlers ([exn:fail:network? void])
      (let wait ([receiver never-evt])
        ;; Assumption: Our Emacs code will write complete sexprs,
        ;; therefore when `in` becomes ready `read` will return
        ;; without blocking.
        (match (sync in receiver)
          [(? input-port? in) (match (read in)
                                [(? eof-object?) (void)]
                                [v               (wait (make-receiver v))])]
          [(vector level message _v topic)
           (parameterize ([current-output-port out])
             (display-log level topic message)
             (flush-output))
           (wait receiver)])))
    (close-input-port in)
    (close-output-port out)
    (accept)))

(define (display-log level topic message)
  (display (label level))
  (display " ")
  (display (ensure-topic-in-message topic message))
  (newline))

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

#lang at-exp racket/base

(require racket/match
         racket/format
         racket/tcp)

(provide start-logger-server)

(define global-logger (make-logger))
(current-logger global-logger)

(define (start-logger-server port)
  (void (thread (logger-thread port))))

(define ((logger-thread port))
  (define listener (tcp-listen port 4 #t))
  (let accept ()
    (define-values (in out) (tcp-accept listener))
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
          [(vector l m v _) (displayln @~a{[@l] @m} out)
                            (flush-output out)
                            (wait receiver)])))
    (close-input-port in)
    (close-output-port out)
    (accept)))

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

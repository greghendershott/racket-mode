#lang racket/base

(require racket/async-channel
         racket/port
         "repl-session.rkt")

(provide repl-output-channel
         repl-output-error
         repl-output-message
         repl-output-run
         repl-output-prompt
         repl-output-value
         repl-output-exit
         make-repl-output-port
         make-repl-error-port)

;;; REPL output

;; We want structured output -- distinctly separated:
;;  - current-output-port
;;    - raw bytes
;;    - images via this port supporting write-special
;;  - current-print values
;;  - current-error-port
;;  - errors from error-display-handler

(define repl-output-channel (make-async-channel))

(define (repl-output kind value)
  (async-channel-put repl-output-channel
                     (list 'repl-output (current-session-id) kind value)))

;; To be called from the error-display-handler. Instead of raw text,
;; `v` may be any structured data that elisp-write can handle.
(define (repl-output-error v)
  (repl-output 'error v))

;; Replacement for the old `display-commented`: Some miscellaneous
;; message from us as opposed to from Racket or from the user program.
(define (repl-output-message v)
  (repl-output 'message v))

(define (repl-output-run v)
  (repl-output 'run v))

;; To be called from get-interaction, i.e. "display-prompt".
(define (repl-output-prompt v)
  (repl-output 'prompt v))

;; To be called from `current-print`
(define (repl-output-value v)
  (repl-output 'value v))

(define (repl-output-exit)
  (repl-output 'exit "REPL session ended"))

(define (make-repl-output-port)
  (make-repl-port 'stdout))

(define (make-repl-error-port)
  (make-repl-port 'stderr))

(define (make-repl-port kind)
  (define name (format "racket-mode-repl-~a" kind))
  (define (write-out bstr start end non-block? breakable?)
    (async-channel-put repl-output-channel
                       (repl-output kind (subbytes bstr start end)))
    (- end start))
  (define (write-out-special v non-block? breakable?)
    (async-channel-put repl-output-channel
                       (repl-output 'image "TODO: Image"))
    #t)
  (define close void)
  (make-output-port name
                    repl-output-channel
                    write-out
                    close
                    write-out-special))

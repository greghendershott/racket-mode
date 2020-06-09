;; Copyright (c) 2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/async-channel
         "repl-session.rkt")

(provide repl-output-channel
         repl-output-error
         repl-output-message
         repl-output-run
         repl-output-prompt
         repl-output-exit
         repl-output-value
         repl-output-value-special
         make-repl-output-port
         make-repl-error-port
         repl-error-port?)

;;; REPL output

;; Traditionally a REPL's output is a hopeless mix of things dumped
;; into stdout and stderr. This forces a client to use unreliable
;; regexps in an attempt to recover the original pieces.
;;
;; Instead we want structured output -- distinctly separated:
;;  - current-output-port
;;  - current-error-port
;;  - current-print values
;;    - strings
;;    - image files
;;  - prompts
;;  - structured errors from error-display-handler
;;  - various messages from the back end

;; A channel from which the command-server can sync.
(define repl-output-channel (make-async-channel))

(define (repl-output kind value)
  (async-channel-put repl-output-channel
                     (list 'repl-output (current-session-id) kind value)))

;; Various wrappers around repl-output:

;; To be called from the error-display-handler. Instead of raw text,
;; `v` may be any structured data that elisp-write can handle. As long
;; as the front end understands the structure, here we don't care.
(define (repl-output-error v)
  (repl-output 'error v))

;; Replacement for the old `display-commented`: Miscellaneous messages
;; from this back end, as opposed to from Racket or from the user
;; program.
(define (repl-output-message v)
  (repl-output 'message v))

;; To be called from get-interaction, i.e. "display-prompt".
(define (repl-output-prompt v)
  (repl-output 'prompt v))

(define (repl-output-run v)
  (repl-output 'run v))

(define (repl-output-exit)
  (repl-output 'exit "REPL session ended"))

;; For current-print
(define (repl-output-value v)
  (repl-output 'value v))

(define (repl-output-value-special v)
  (repl-output 'value-special v))

;; Output port wrappers around repl-output:

;; Tuck the port in a struct just for a simple, reliable
;; repl-error-port? predicate.
(struct repl-error-port (p)
  #:property prop:output-port 0)
(define (make-repl-error-port)
  (repl-error-port (make-repl-port 'stderr)))

;; And do same for this, just for conistency.
(struct repl-output-port (p)
  #:property prop:output-port 0)
(define (make-repl-output-port)
  (repl-output-port (make-repl-port 'stdout)))

(define (make-repl-port kind)
  (define name (format "racket-mode-repl-~a" kind))
  (define special-kind (string->symbol (format "~a-special" kind)))
  (define (write-out bstr start end non-block? breakable?)
    (async-channel-put repl-output-channel
                       (repl-output kind (subbytes bstr start end)))
    (- end start))
  (define (write-out-special v _non-block? _breakable?)
    (async-channel-put repl-output-channel
                       (repl-output special-kind v))
    #t)
  (define close void)
  (make-output-port name
                    repl-output-channel
                    write-out
                    close
                    write-out-special))

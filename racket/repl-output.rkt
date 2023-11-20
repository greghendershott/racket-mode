;; Copyright (c) 2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/async-channel
         racket/match
         "repl-session.rkt")

(provide repl-output-channel
         repl-output-error
         repl-output-message
         repl-output-run
         repl-output-prompt
         repl-output-exit
         repl-output-value
         repl-output-value-special
         make-repl-output-manager
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

;; This manager thread mediates between the `repl-output' function and
;; the `repl-output-channel` async-channel. It seeks a warm bowl of
;; porridge for the number and size of stdout and stderr outputs.
;;
;; - stdout/stderr items may be held awhile in case the next item is
;; the same kind. A run of consecutive items within a time span are
;; consolidated into one.
;;
;; - On the other hand a very large stdout/stderr item is split into
;; multiple smaller ones.
;;
;; So this is a kind of buffering or "batching", but using a timer
;; instead of needing explicit flushing. At the same time, any
;; non-stdout/stderr kind of output will automatically "flush",
;; including items like 'prompt or 'run, so this works out fine as
;; well.
(struct repl-output-item (kind value))
(define ((repl-output-manager-thread session-id))
  (define msec-threshold 500)
  (define size-threshold 4096)

  (define (put* kind value)
    (async-channel-put repl-output-channel
                       (list 'repl-output session-id kind value)))
  (define (put item)
    (match item
      [(repl-output-item (and kind (or (== 'stdout) (== 'stderr))) bstr)
       (define len (bytes-length bstr))
       (for ([beg (in-range 0 len size-threshold)])
         (put* kind (subbytes bstr beg (min len (+ beg size-threshold)))))]
      [(repl-output-item kind value)
       (put* kind value)]))

  (define pending-item #f)
  (define pending-flush-alarm-evt never-evt)

  (define (queue item)
    (match-define (repl-output-item kind value) item)
    (match pending-item
      ;; No pending item. When the new item is stdout or stderr, and
      ;; doesn't already exceed the size-threshold, set it as the
      ;; pending item and start our countdown.
      [#f
       #:when (and (memq kind '(stdout stderr))
                   (< (bytes-length value) size-threshold))
       (set! pending-item item)
       (set! pending-flush-alarm-evt
             (alarm-evt (+ (current-inexact-milliseconds)
                           msec-threshold)))]
      ;; No pending item. Just send new item now.
      [#f
       (put item)]
      ;; There's a pending item. New item is same kind. When appending
      ;; their values is under the size-threshold, combine them.
      [(repl-output-item (== kind) pending-value)
       #:when (< (+ (bytes-length pending-value) (bytes-length value))
                 size-threshold)
       (set! pending-item
             (repl-output-item kind
                               (bytes-append pending-value value)))]
      ;; There's a pending item. Send it then the new item, now.
      [(? repl-output-item?)
       (flush-pending)
       (put item)]))

  (define (flush-pending)
    (when pending-item
      (put pending-item)
      (set! pending-item #f))
    (set! pending-flush-alarm-evt never-evt))

  (let loop ()
    (sync (handle-evt (thread-receive-evt)
                      (λ (_evt) (queue (thread-receive))))
          (handle-evt pending-flush-alarm-evt
                      (λ (_evt) (flush-pending))))
    (loop)))

(define (make-repl-output-manager session-id)
  (thread (repl-output-manager-thread session-id)))

(define (repl-output kind value)
  (define t (current-repl-output-manager))
  (when t
    (thread-send t
                 (repl-output-item kind value)
                 void)))

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
  (define (write-out bstr start end non-block? breakable?)
    (repl-output kind (subbytes bstr start end))
    (- end start))
  (define close void)
  (make-output-port name
                    repl-output-channel
                    write-out
                    close))

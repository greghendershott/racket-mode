;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require racket/format
         racket/gui/dynamic
         racket/match
         racket/set
         "safe-dynamic-require.rkt"
         "gui.rkt"
         "repl-output.rkt"
         "repl-session.rkt"
         "stack-checkpoint.rkt")

(provide get-interaction)

;; This input port holds the unread remainder of the most-recent
;; submission string from the current-submissions channel. Although
;; commonly each submission is one read-able value, like "1\n", it
;; might contain more than one read-able value, e.g. if the user
;; submits "1 2 3\n". We want to read all. Furthermore, we don't want
;; to display unnecessary prompts for the subsequent ones.
(define current-submission-input-port (make-parameter (open-input-string "")))

(define (get-interaction prompt)
  (maybe-warn-for-session)
  (define (get)
    (with-handlers ([exn:fail:read?
                     (λ (exn)
                       ;; Discard remainder after this read error.
                       (current-submission-input-port (open-input-string ""))
                       (raise exn))])
      (current-get-interaction-input-port (λ () (current-submission-input-port)))
      (with-stack-checkpoint
        ((current-read-interaction) 'racket-mode-repl (current-submission-input-port)))))
  (define v (get))
  (cond
    [(eof-object? v)
     (repl-output-prompt (string-append prompt ">"))
     (match-define (cons expr echo?) (get-submission))
     (when echo?
       (repl-output-message (string-append expr " => ")))
     (current-submission-input-port (open-input-string expr))
     (port-count-lines! (current-submission-input-port))
     (get)]
    [else v]))

(define current-get-interaction-evt
  (safe-dynamic-require 'racket/base 'current-get-interaction-evt))

;; Get value from current-submissions channel in the best manner
;; available given the version of Racket. Avoids hard dependency on
;; Racket 8.4+.
(define (get-submission)
  (cond
    [current-get-interaction-evt
     (let loop ()
       (sync
        (handle-evt ((current-get-interaction-evt)) ;allow GUI yield
                    (λ (thk)
                      (thk)
                      (loop)))
        (current-submissions)))]
    [else
     ((txt/gui sync yield) (current-submissions))]))

;; Note: We try to eagerly load racket/gui/base in gui.rkt. See
;; comments there, explaining why.
;;
;; As a result, gui-available? here merely means that a user program
;; _could_ use it (e.g. gui-lib is installed and running on a
;; non-headless system where Gtk can initialize).
;;
;; As a result, a user on a GUI-capable Racket install will see the
;; warning at the start of _every_ REPL session -- not just when first
;; running a GUI program (which would be more desirable, but I don't
;; immediately see how to do that).
(define warned-sessions (mutable-set))
(define (maybe-warn-for-session)
  (unless current-get-interaction-evt
    (when (gui-available?)
      (unless (set-member? warned-sessions (current-session-id))
        (set-add! warned-sessions (current-session-id))
        (repl-output-message
         @~a{Warning: GUI programs might not work correctly because
             your version of Racket lacks `current-get-interaction-evt`,
             which was added in Racket 8.4.})))))

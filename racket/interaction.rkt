;; Copyright (c) 2013-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require racket/format
         racket/gui/dynamic
         racket/set
         "gui.rkt"
         "repl-output.rkt"
         "repl-session.rkt"
         "stack-checkpoint.rkt")

(provide get-interaction)

(define (get-interaction prompt)
  (maybe-warn-for-session)
  (repl-output-prompt (string-append prompt ">"))
  (define str (get-submission))
  (define in (open-input-string str))
  (with-stack-checkpoint
    ((current-read-interaction) 'racket-mode-repl in)))

(define current-get-interaction-evt
  (dynamic-require 'racket/base 'current-get-interaction-evt (λ () #f)))

;; Get a string from current-submissions channel in the best manner
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

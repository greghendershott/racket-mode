;;; racket-cmd.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.
;; Image portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

;;; TCP connection to back end command server

(require 'racket-custom)

(declare-function racket--repl-live-p "racket-repl")
(declare-function racket--repl-start  "racket-repl")
(declare-function  racket--debug-on-break "racket-debug" (response))
(autoload         'racket--debug-on-break "racket-debug")

(defvar racket--cmd-auth nil
  "A value we give the Racket back-end when we launch it and when we connect.
See issue #327.")

(defvar racket--cmd-proc nil
  "Process for talking to the command server.
Most code should use `racket--cmd-open-p' to check this.")

(defun racket--cmd-open-p ()
  "Does an open process exist for the command server?"
  (and racket--cmd-proc
       (eq 'open (process-status racket--cmd-proc))))

(defvar racket--cmd-nonce->callback (make-hash-table :test 'eq)
  "A hash from nonce to callback function.")
(defvar racket--cmd-nonce 0
  "Number that increments for each command request we send.")

(defvar racket--cmd-connect-attempts 15
  "How many times to `racket--cmd-connect-attempt', at roughly 1 second intervals.")

(defvar racket--cmd-connect-timer nil)

(defun racket--cmd-connect-scheduled-p ()
  (and racket--cmd-connect-timer
       (timerp racket--cmd-connect-timer)))

(defun racket--cmd-connect-schedule-start ()
  (unless (racket--cmd-connect-scheduled-p)
    (setq racket--cmd-connect-timer
          (run-at-time 0.5 nil
                       #'racket--cmd-connect-attempt
                       1))))

(defun racket--cmd-connect-schedule-retry (attempt)
  (when (racket--cmd-connect-scheduled-p)
    (cancel-timer racket--cmd-connect-timer)
    (setq racket--cmd-connect-timer
          (run-at-time 1.0 nil
                       #'racket--cmd-connect-attempt
                       (1+ attempt)))))

(defun racket--cmd-connect-stop ()
  (when (racket--cmd-connect-scheduled-p)
    (cancel-timer racket--cmd-connect-timer)
    (setq racket--cmd-connect-timer nil)))


(defun racket--cmd-connect-attempt (attempt)
  "Attempt to make a TCP connection to the command server.
If that fails, retry by scheduling ourself to run again later.
When we do make a connection, call
`racket--repl-call-after-live-thunks'."
  (unless (featurep 'make-network-process '(:nowait t))
    (error "Racket Mode needs Emacs make-network-process to support the :nowait feature"))
  (setq
   racket--cmd-proc
   (make-network-process
    :name    "racket-command"
    :host    "127.0.0.1"
    :service racket-command-port
    :nowait  t
    :sentinel
    (lambda (proc event)
      ;; (message "sentinel got (%S %S) [attempt %s]" proc (substring event 0 -1) attempt)
      (cond ((string-match-p "^open" event)
             (let ((buf (generate-new-buffer (concat " *" (process-name proc) "*"))))
               (set-process-buffer proc buf)
               (buffer-disable-undo buf))
             (set-process-filter proc #'racket--cmd-process-filter)
             (process-send-string proc (concat racket--cmd-auth "\n"))
             (run-at-time 0.1 nil
                          #'message
                          "Connected to %s process on port %s after %s attempt%s"
                          proc racket-command-port attempt (if (= 1 attempt) "" "s"))
             (run-at-time 0.2 nil
                          #'racket--call-cmd-after-open-thunks)
             (racket--cmd-connect-stop))

            ((string-match-p "^failed" event)
             (delete-process proc) ;we'll get called with "deleted" event, below
             (racket--cmd-connect-schedule-retry attempt)
             (if (<= attempt racket--cmd-connect-attempts)
                 (racket--cmd-connect-schedule-retry attempt)
               (racket--cmd-connect-stop)))

            ((or (string-match-p "^deleted" event)
                 (string-match-p "^connection broken by remote peer" event))
             (clrhash racket--cmd-nonce->callback)
             ;; If process has a buffer -- and do check that it does,
             ;; see #383 -- we can't `kill-buffer' now here in the
             ;; process sentinel. Instead do soon.
             (pcase (process-buffer proc)
               ((and (pred bufferp) buf)
                (run-at-time 0.1 nil #'kill-buffer buf))))

            (t (run-at-time 0.1 nil
                            #'message "sentinel surprised by (%S %S) [attempt %s]"
                            proc event attempt)))))))

(defvar racket--cmd-after-open-thunks nil
  "List of thunks to call when upon connection to the command server.
Thunks are popped off the list when called; in other words this
is NOT like an Emacs hook variable.")

(defun racket--call-cmd-after-open-thunks ()
  (while racket--cmd-after-open-thunks
    (funcall (pop racket--cmd-after-open-thunks))))

(defun racket--call-when-connected-to-command-server (thunk)
  "Call THUNK, connecting to command server if necessary without blocking.

If the command server is available now, call THUNK now.

Otherwise, take steps to make the command server available, and
after it is, call THUNK."
  (if (racket--cmd-open-p)
      ;; Do now
      (funcall thunk)
    ;; Do later
    (push thunk racket--cmd-after-open-thunks)
    ;; Next, let's make "later" happen:
    ;;
    ;; Remember that our back end process has two aspects: stdio
    ;; connected to a comint buffer ("the REPL"), and, a TCP server
    ;; ("the command server"). Furthermore, connecting to the latter
    ;; is something we retry on a timer until it succeeds, so that we
    ;; don't block.
    (if (not (racket--repl-live-p))
        ;; The REPL process is not live, so call `racket--repl-start',
        ;; which also arranges for `racket--cmd-connect-attempt' to be
        ;; called eventually.
        (racket--repl-start)
      ;; The REPL process is live, but we have no connection to the
      ;; command server. Unless we're already connecting, start
      ;; connecting now.
      (unless (racket--cmd-connect-scheduled-p)
        (racket--cmd-connect-schedule-start)))))

(defun racket--cmd-process-filter (proc string)
  "Parse complete sexprs from the process output and give them to
`racket--cmd-dispatch-response'."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (goto-char (point-min))
        (while
            (condition-case ()
                (progn
                  (forward-sexp 1)
                  (let ((sexp (buffer-substring (point-min) (point))))
                    (delete-region (point-min) (point))
                    (ignore-errors
                      (racket--cmd-dispatch-response (read sexp))
                      t)))
              (scan-error nil)))))))

(defun racket--cmd-dispatch-response (response)
  "Do something with a sexpr sent to us from the command server.
Mostly these are responses to command requests. Strictly speaking
'debug-break is a \"notification\", i.e. /not/ one direct response
to one command request."
  (pcase response
    (`(debug-break . ,response)
     (run-at-time 0.001 nil #'racket--debug-on-break response))
    (`(,nonce . ,response)
     (let ((callback (gethash nonce racket--cmd-nonce->callback)))
       (when callback
         (remhash nonce racket--cmd-nonce->callback)
         (run-at-time 0.001 nil callback response))))
    (_ nil)))

(defun racket--cmd/async-raw (command-sexpr &optional callback)
  "Send COMMAND-SEXPR and return. Later call CALLBACK with the response sexp.

If CALLBACK is not supplied or nil, defaults to `ignore'.

Otherwise CALLBACK is called after the command server returns a
response. Because command responses are obtained from the dynamic
extent of a `set-process-filter' proc -- which may have
limitations on what it can or should do -- CALLBACK is not called
immediately but instead using `run-at-time' with a very small
delay.

Important: Do not assume that `current-buffer' is the same when
CALLBACK is called, as it was when the command was sent. If you
need to do something to do that original buffer, save the
`current-buffer' in a `let' and use it in a `with-current-buffer'
form. See `racket--restoring-current-buffer'.

If the command server is not available, we do not block. Instead
we save a thunk to run when it does become available, and call
`racket--repl-start' which also does not block."
  (racket--call-when-connected-to-command-server
   (lambda ()
     (cl-incf racket--cmd-nonce)
     (when (and callback
                (not (equal callback #'ignore)))
       (puthash racket--cmd-nonce callback racket--cmd-nonce->callback))
     (process-send-string racket--cmd-proc
                          (format "%S\n" (cons racket--cmd-nonce
                                               command-sexpr))))))

(defun racket--cmd/async (command-sexpr &optional callback)
  "You probably want to use this instead of `racket--cmd/async-raw'.

CALLBACK is only called for 'ok responses, with (ok v ...)
unwrapped to (v ...).

'error responses are handled here. Note: We use `message' not
`error' here because:

  1. It would show \"error running timer:\" which, although true,
     is confusing or at best N/A for end users.

  2. More simply, we don't need to escape any call stack, we only
     need to ... not call the callback!

'break responses are handled here, too. This is used when a
command is somehow canceled, with no useful response except the
indication we should clean up the pending callback as usual.

The original value of `current-buffer' is temporarily restored
during CALLBACK, because neglecting to do so is a likely
mistake."
  (let ((buf (current-buffer)))
    (racket--cmd/async-raw
     command-sexpr
     (if callback
         (lambda (response)
           (pcase response
             (`(ok ,v)    (with-current-buffer buf (funcall callback v)))
             (`(error ,m) (message "%s" m))
             (`(break)    nil)
             (v           (message "Unknown command response: %S" v))))
       #'ignore))))

(defun racket--cmd/await (command-sexpr)
  "Send COMMAND-SEXPR. Await and return an 'ok response value, or raise `error'."
  (let* ((awaiting 'RACKET-REPL-AWAITING)
         (response awaiting))
    (racket--cmd/async-raw command-sexpr
                           (lambda (v) (setq response v)))
    (with-timeout (racket-command-timeout
                   (error "racket-command process timeout"))
      (while (eq response awaiting)
        (accept-process-output nil 0.001))
      (pcase response
        (`(ok ,v)    v)
        (`(error ,m) (error "%s" m))
        (v           (error "Unknown command response: %S" v))))))

(provide 'racket-cmd)

;; racket-cmd.el ends here

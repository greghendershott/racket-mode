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
(require 'racket-util)

(declare-function  racket--debug-on-break "racket-debug" (response))
(autoload         'racket--debug-on-break "racket-debug")

(defvar racket--cmd-name "racket-process"
  "Name for both the process and its associated buffer")

(defun racket--cmd-process ()
  "Process for talking to the command server.
Most code should use `racket--cmd-open-p' to check this."
  (get-buffer-process racket--cmd-name))

(defun racket--cmd-open-p ()
  "Does an open process exist for the command server?"
  (and (racket--cmd-process)
       (eq 'run (process-status (racket--cmd-process)))))



(defconst racket--minimum-required-version "6.5"
  "The minimum version of Racket required by run.rkt.

Although some functionality may require an even newer version of
Racket, run.rkt will handle that via `dynamic-require` and
fallbacks. The version number here is a baseline for run.rkt to
be able to load at all.")

(defvar racket--run.rkt (expand-file-name "main.rkt" racket--rkt-source-dir)
  "Pathname of run.rkt.")

(defvar racket-adjust-run-rkt #'identity
  "A function used to transform the variable `racket--run.rkt'.

You probably don't need to change this unless you are developing
Racket Mode, AND run Emacs on Windows Subsystem for Linux, AND
want to run your programs using Windows Racket.exe, AND have the
Racket Mode source code under \"/mnt\". Whew. In that case you
can set this variable to the function `racket-wsl-to-windows' so
that Racket Mode can find its own run.rkt file.")

(defun racket--version ()
  "Get the `racket-program' version as a string."
  (with-temp-message "Checking Racket version ..."
    (with-temp-buffer
      (call-process racket-program nil t nil "--version")
      (goto-char (point-min))
      ;; Welcome to Racket v6.12.
      ;; Welcome to Racket v7.0.0.6.
      (save-match-data
        (re-search-forward "[0-9]+\\(?:\\.[0-9]+\\)*")
        (match-string 0)))))

(defun racket--assert-version (at-least)
  "Raise a `user-error' unless Racket is version AT-LEAST."
  (let ((have (racket--version)))
    (unless (version<= at-least have)
      (user-error "Racket Mode needs at least Racket version %s but you have %s"
                  at-least have))))


(defvar racket--cmd-auth nil
  "A value we give the Racket back-end when we launch it and when we connect.
See issue #327.")

(defun racket--cmd-open ()
  ;; Never create more "racket-process<1>" etc processes.
  (racket--cmd-close)
  (racket--assert-version racket--minimum-required-version)
  (prog1
      (make-process
       :name            racket--cmd-name
       :connection-type 'pipe
       :noquery         t
       :buffer          (get-buffer-create racket--cmd-name)
       :stderr          (get-buffer-create "racket-process-stderr")
       :command         (list racket-program
                              (funcall racket-adjust-run-rkt racket--run.rkt)
                              (number-to-string racket-command-port)
                              (setq racket--cmd-auth (format "%S" `(auth ,(random)))))
       :filter          #'racket--cmd-process-filter)
    (racket--call-cmd-after-open-thunks)))

(defun racket--cmd-close ()
  (pcase (get-process racket--cmd-name)
    ((and (pred (processp)) proc) (delete-process proc))))


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
    (racket--cmd-open)))

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

(defvar racket--cmd-nonce->callback (make-hash-table :test 'eq)
  "A hash from nonce to callback function.")
(defvar racket--cmd-nonce 0
  "Number that increments for each command request we send.")

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

(defun racket--cmd/async-raw (repl-session-id command-sexpr &optional callback)
  "Send COMMAND-SEXPR and return. Later call CALLBACK with the response sexp.

REPL-SESSION-ID may be nil for commands that do not need to run
in a specific namespace.

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
     (process-send-string
      (racket--cmd-process)
      (format "%S\n" (cons racket--cmd-nonce
                           (cons repl-session-id
                                 command-sexpr)))))))

(defun racket--cmd/async (repl-session-id command-sexpr &optional callback)
  "You probably want to use this instead of `racket--cmd/async-raw'.

REPL-SESSION-ID may be nil for commands that do not need to run
in a specific namespace.

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
     repl-session-id
     command-sexpr
     (if callback
         (lambda (response)
           (pcase response
             (`(ok ,v)    (with-current-buffer buf (funcall callback v)))
             (`(error ,m) (message "%s" m))
             (`(break)    nil)
             (v           (message "Unknown command response: %S" v))))
       #'ignore))))

(defun racket--cmd/await (repl-session-id command-sexpr)
  "Send COMMAND-SEXPR. Await and return an 'ok response value, or raise `error'.

REPL-SESSION-ID may be nil for commands that do not need to run
in a specific namespace."
  (let* ((awaiting 'RACKET-REPL-AWAITING)
         (response awaiting))
    (racket--cmd/async-raw repl-session-id
                           command-sexpr
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

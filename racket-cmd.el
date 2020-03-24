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

(defun racket-start-back-end ()
  "Start the back end process used by Racket Mode.

If the process is already started, this command will stop and restart it."
  (interactive)
  (racket--cmd-open))

(defun racket-stop-back-end ()
  "Stop the back end process used by Racket Mode.

If the process is not already started, this does nothing."
  (interactive)
  (racket--cmd-close))

(defvar racket--cmd-name "racket-process"
  "Name for both the process and its associated buffer")

(defun racket--cmd-open-p ()
  "Does a running process exist for the command server?"
  (pcase (get-process racket--cmd-name)
    ((and (pred (processp)) proc)
     (eq 'run (process-status proc)))))

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

(defvar racket--cmd-auth nil
  "A value we give the Racket back-end when we launch it and when we connect.
See issue #327.")

(defun racket--cmd-open ()
  ;; Never create more "racket-process<1>" etc processes.
  (racket--cmd-close)
  (make-process
   :name            racket--cmd-name
   :connection-type 'pipe
   :noquery         t
   :buffer          (get-buffer-create (concat "*" racket--cmd-name "*"))
   :stderr          (get-buffer-create "*racket-process-stderr*")
   :command         (list racket-program
                          (funcall racket-adjust-run-rkt racket--run.rkt)
                          (number-to-string racket-command-port)
                          (setq racket--cmd-auth (format "%S" `(auth ,(random)))))
   :filter          #'racket--cmd-process-filter))

(defun racket--cmd-close ()
  (pcase (get-process racket--cmd-name)
    ((and (pred (processp)) proc) (delete-process proc))))

(defun racket--call-when-connected-to-command-server (func)
  "Call FUNC, starting the back end process if necessary."
  (unless (racket--cmd-open-p)
    (racket--cmd-open))
  (funcall func (get-process racket--cmd-name)))

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
form. See `racket--restoring-current-buffer'."
  (racket--call-when-connected-to-command-server
   (lambda (process)
     (cl-incf racket--cmd-nonce)
     (when (and callback
                (not (equal callback #'ignore)))
       (puthash racket--cmd-nonce callback racket--cmd-nonce->callback))
     (process-send-string
      process
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

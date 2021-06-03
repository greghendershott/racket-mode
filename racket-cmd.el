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

;;; Back end command server process

(require 'racket-custom)
(require 'racket-util)

(declare-function  racket--debug-on-break "racket-debug" (response))
(autoload         'racket--debug-on-break "racket-debug")

(declare-function  racket--logger-on-notify "racket-logger" (str))
(autoload         'racket--logger-on-notify "racket-logger")

;;;###autoload
(defvar racket-start-back-end-hook nil
  "Hook run after `racket-start-back-end'.")

;;;###autoload
(defun racket-start-back-end ()
  "Start the back end process used by Racket Mode.

If the process is already started, this command will stop and restart it.

As the final step, runs the hook `racket-start-back-end-hook'."
  (interactive)
  (racket--cmd-open)
  (run-hooks racket-start-back-end-hook))

;;;###autoload
(defun racket-stop-back-end ()
  "Stop the back end process used by Racket Mode.

If the process is not already started, this does nothing."
  (interactive)
  (racket--cmd-close))

(defconst racket--cmd-process-name "racket-mode-back-end"
  "Used to name the process and its associated buffer.")

(defun racket--cmd-open-p ()
  "Does a running process exist for the command server?"
  (pcase (get-process racket--cmd-process-name)
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
  "A value we give the Racket back-end when we launch it and when we create REPLs.
See issue #327.")

(defun racket--cmd-open ()
  ;; Avoid multiple processes/buffers like "racket-process<1>".
  (racket--cmd-close)
  ;; Give the process buffer the current values of some vars; see
  ;; <https://github.com/purcell/envrc/issues/22>.
  (cl-letf* (((default-value 'process-environment) process-environment)
             ((default-value 'exec-path)           exec-path))
    (make-process
     :name            racket--cmd-process-name
     :connection-type 'pipe
     :noquery         t
     :coding          'utf-8
     :buffer          (get-buffer-create (concat " *" racket--cmd-process-name "*"))
     :stderr          (make-pipe-process
                       :name     (concat racket--cmd-process-name "-stderr")
                       :buffer   nil
                       :noquery  t
                       :coding   'utf-8
                       :filter   #'racket--cmd-process-stderr-filter
                       :sentinel #'ignore)
     :command         (list racket-program
                            (funcall racket-adjust-run-rkt racket--run.rkt)
                            "--auth"
                            (setq racket--cmd-auth (format "token-%x" (random)))
                            (if (and (boundp 'image-types)
                                     (fboundp 'image-type-available-p)
                                     (or (and (memq 'svg image-types)
                                              (image-type-available-p 'svg))
                                         (and (memq 'imagemagick image-types)
                                              (image-type-available-p 'imagemagick))))
                                "--use-svg"
                              "--do-not-use-svg"))
     :filter          #'racket--cmd-process-filter)))

(defun racket--cmd-close ()
  (pcase (get-process racket--cmd-process-name)
    ((and (pred (processp)) proc) (delete-process proc))))

(defun racket--cmd-process-stderr-filter (proc string)
  "Show back end process stderr via `message'.
Won't show noise like \"process finished\" if process sentinel is
`ignore'."
  (message "{%s} %s\n" proc string))

(defun racket--cmd-process-filter (proc string)
  "Parse complete sexprs from the process output and give them to
`racket--cmd-dispatch-response'."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (goto-char (point-min))
        (while (pcase (ignore-errors (read buffer))
                 (`nil `nil)
                 (sexp (delete-region (point-min)
                                      (if (eq (char-after) ?\n)
                                          (1+ (point))
                                        (point)))
                       (racket--cmd-dispatch-response sexp)
                       t)))))))

(defvar racket--cmd-nonce->callback (make-hash-table :test 'eq)
  "A hash from nonce to callback function.")
(defvar racket--cmd-nonce 0
  "Number that increments for each command request we send.")

(defun racket--cmd-dispatch-response (response)
  "Do something with a sexpr sent to us from the command server.
Mostly these are responses to command requests. Strictly speaking
'logger and 'debug-break are \"notifications\", i.e. /not/ one
direct response to one command request."
  (pcase response
    (`(logger ,str)
     (run-at-time 0.001 nil #'racket--logger-on-notify str))
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
  (unless (racket--cmd-open-p)
    (racket--cmd-open))
  (cl-incf racket--cmd-nonce)
  (when (and callback
             (not (equal callback #'ignore)))
    (puthash racket--cmd-nonce callback racket--cmd-nonce->callback))
  (process-send-string
   (get-process racket--cmd-process-name)
   (let ((print-length nil) ;for %S
         (print-level nil))
     (format "%S\n" `(,racket--cmd-nonce ,repl-session-id . ,command-sexpr)))))

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
             (v           (let ((print-length nil) ;for %S
                                (print-level nil))
                            (message "Unknown command response: %S" v)))))
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
        (v           (let ((print-length nil) ;for %S
                           (print-level nil))
                       (error "Unknown command response: %S" v)))))))

(provide 'racket-cmd)

;; racket-cmd.el ends here

;;; racket-repl.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2019 by Greg Hendershott.
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

(require 'racket-custom)
(require 'racket-common)
(require 'racket-util)
(require 'comint)
(require 'compile)
(require 'easymenu)
(require 'cl-lib)
(require 'rx)

;; Don't (require 'racket-debug). Mutual dependency. Instead:
(declare-function  racket--debug-send-definition "racket-debug" (beg end))
(autoload         'racket--debug-send-definition "racket-debug")
(declare-function  racket--debug-on-break        "racket-debug" (response))
(autoload         'racket--debug-on-break        "racket-debug")
(declare-function  racket--debuggable-files      "racket-debug" (file-to-run))
(autoload         'racket--debuggable-files      "racket-debug")

(defconst racket--repl-buffer-name/raw
  "Racket REPL"
  "The base buffer name, NOT surrounded in *stars*")
(defconst racket--repl-buffer-name
  (concat "*" racket--repl-buffer-name/raw "*")
  "The actual buffer name as created by comint-mode")

(defmacro with-racket-repl-buffer (&rest body)
  "Execute the forms in BODY with `racket-repl-mode' temporarily current.
The value returned is the value of the last form in BODY --
unless no `racket-repl-mode' buffer exists, in which case no BODY
forms are evaluated and nil is returned. See also
`with-current-buffer'."
  (declare (indent 0) (debug t))
  (let ((repl-buffer (make-symbol "repl-buffer")))
    `(let ((,repl-buffer (get-buffer racket--repl-buffer-name)))
       (when ,repl-buffer
         (with-current-buffer ,repl-buffer
           ,@body)))))

(defun racket-repl--input-filter (str)
  "Don't save anything matching `racket-history-filter-regexp'."
  (not (string-match racket-history-filter-regexp str)))

(defalias 'racket-repl-eval-or-newline-and-indent #'racket-repl-submit)

(defun racket-repl-submit (&optional prefix)
  "Submit your input to the Racket REPL.

If the REPL is running a Racket lang whose language-info has a
'drracket:submit-predicate, that is first called to see if the
input is valid to be submitted.

With a prefix: After sending your input and a newline, also calls
`process-send-eof' -- because some langs require EOF to mark the
end of an interactive expression/statement."
  (interactive "P")
  (let* ((proc (get-buffer-process (current-buffer)))
         (_    (unless proc (user-error "Current buffer has no process")))
         (text (substring-no-properties (funcall comint-get-old-input)))
         (submitp
          (if racket-use-repl-submit-predicate
              (cl-case (racket--cmd/await `(repl-submit? ,text t))
                ((t) t)
                ((nil) (user-error "Not a complete expression, according to the current lang's submit-predicate."))
                ((default) (racket--repl-complete-sexp-p proc)))
            (racket--repl-complete-sexp-p proc))))
    (if (not submitp)
        (newline-and-indent)
      (comint-send-input)
      (remove-text-properties comint-last-input-start
                              comint-last-input-end
                              '(font-lock-face comint-highlight-input))
      ;; Hack for datalog/lang
      (when prefix (process-send-eof proc)))))

(defun racket--repl-complete-sexp-p (proc)
  (condition-case nil
      (let* ((beg    (marker-position (process-mark proc)))
             (end    (save-excursion
                       (goto-char beg)
                       (forward-list 1) ;scan-error unless complete sexp
                       (point)))
             (blankp (save-excursion
                       (save-match-data
                         (goto-char beg)
                         (equal end
                                (re-search-forward (rx (1+ (or (syntax whitespace)
                                                               (syntax comment-start)
                                                               (syntax comment-end))))
                                                   end
                                                   t))))))
        (not (or (equal beg end) blankp)))
    (scan-error nil)))

(defun racket-repl-exit (&optional quitp)
  "End the Racket REPL process.

Effectively the same as entering `(exit)` at the prompt, but
works even when the module language doesn't provide any binding
for `exit`. If there is no connection to the command server, then
this uses `comint-kill-subjob' to send a kill signal.

With a prefix, uses `comint-quit-subjob' to send a quit signal."
  (interactive "P")
  (if quitp
      (comint-quit-subjob)
    (condition-case ()
        (progn (racket--cmd/async `(exit)))
      (error (comint-kill-subjob)))))

;;;###autoload
(defun racket-repl (&optional noselect)
  "Show the Racket REPL buffer in some window.

If NOSELECT is not nil, does not also select the REPL window.

*IMPORTANT*

The main, intended use of Racket Mode's REPL is that you
`find-file' some specific .rkt file, then `racket-run' it. The
REPL will then match that file. Also, various Racket Mode
features will work, such as completion, visiting definitions, and
so on.

If the REPL isn't running, and you want to start it for no file
in particular? Then you could use this command. But the resulting
REPL will have a minimal \"#lang racket/base\" namespace. You
could enter \"(require racket)\" if you want the equivalent of
\"#lang racket\". You could also \"(require racket/enter)\" if
you want things like \"enter!\". But in some sense you'd be
\"using it wrong\". If you really don't want to use Racket Mode's
REPL as intended, then you might as well use a plain Emacs shell
buffer to run command-line Racket."
  (interactive "P")
  (if (racket--repl-live-p)
      (racket--repl-show-and-move-to-end)
    (racket--repl-start))
  (unless noselect
    (select-window (get-buffer-window racket--repl-buffer-name t))))

(defconst racket--minimum-required-version "6.0"
  "The minimum version of Racket required by run.rkt.

Although some functionality may require an even newer version of
Racket, run.rkt will handle that via `dynamic-require` and
fallbacks. The version number here is a baseline for run.rkt to
be able to load at all.")

(defvar racket--run.rkt (expand-file-name "run.rkt" racket--rkt-source-dir)
  "Pathname of run.rkt.")

(defvar racket-adjust-run-rkt #'identity
  "A function used to transform the variable `racket--run.rkt'.

You probably don't need to change this unless you are developing
Racket Mode, AND run Emacs on Windows Subsystem for Linux, AND
want to run your programs using Windows Racket.exe, AND have the
Racket Mode source code under \"/mnt\". Whew. In that case you
can set this variable to the function `racket-wsl-to-windows' so
that Racket Mode can find its own run.rkt file.")

(defvar-local racket-user-command-line-arguments
  nil
  "List of command-line arguments to supply to your Racket program.

Accessible in your Racket program in the usual way --- the
parameter `current-command-line-arguments` and friends.

This is an Emacs buffer-local variable --- convenient to set as a
file local variable. For example at the end of your .rkt file:

#+BEGIN_SRC elisp
    ;; Local Variables:
    ;; racket-user-command-line-arguments: (\"-f\" \"bar\")
    ;; End:
#+END_SRC

Set this way, the value must be an *unquoted* list of strings.
For example:

#+BEGIN_SRC elisp
    (\"-f\" \"bar\")
#+END_SRC

The following values will /not/ work:

#+BEGIN_SRC elisp
    '(\"-f\" \"bar\")
    (list \"-f\" \"bar\")
#+END_SRC
")

(defun racket--repl-live-p ()
  "Does the Racket REPL buffer exist and have a live Racket process?"
  (comint-check-proc racket--repl-buffer-name))

(defvar racket--repl-before-run-hook nil
  "Thunks to do before each `racket--repl-run'.")

(defun racket--repl-run (&optional what-to-run context-level callback)
  "Do an initial or subsequent run.

WHAT-TO-RUN should be a cons of a file name to a list of
submodule symbols. Or if nil, defaults to `racket--what-to-run'.

CONTEXT-LEVEL should be a valid value for the variable
`racket-error-context', 'coverage, or 'profile. Or if nil,
defaults to the variable `racket-error-context'.

CALLBACK is supplied to `racket--repl-run' and is used as the
callback for `racket--cmd/async'; it may be nil which is
equivalent to #'ignore. CALLBACK is called with a single
argument whose value should be ignored.

- If the REPL is not live, start our backend run.rkt passing
  the file to run as a command-line argument.

- If the REPL is live, send a 'run command to the backend's TCP
  server."
  (run-hook-with-args 'racket--repl-before-run-hook)
  (let ((cmd (racket--repl-make-run-command (or what-to-run (racket--what-to-run))
                                            (or context-level racket-error-context))))
    (cond ((racket--repl-live-p)
           (racket--cmd/async cmd callback)
           (racket--repl-show-and-move-to-end))
          (t
           (racket--repl-start cmd)
           (when callback
             (racket--cmd/async `(no-op) callback))))))

(defun racket--repl-make-run-command (what-to-run &optional context-level)
  "Form a `run` command sexpr for the backend.
WHAT-TO-RUN may be nil, meaning just a `racket/base` namespace."
  (let ((context-level (or context-level racket-error-context)))
    (list 'run
          what-to-run
          racket-memory-limit
          racket-pretty-print
          context-level
          racket-user-command-line-arguments
          (when (and what-to-run (eq context-level 'debug))
            (racket--debuggable-files (car what-to-run)))
          racket-retry-as-skeleton)))

(defvar racket--cmd-auth nil
  "A value we give the Racket back-end when we launch it and when we connect.
See issue #327.")

(defun racket--repl-start (&optional run-command)
  "Start the Racket process, creating `racket-repl-mode' buffer if necessary.

Arrange for attempts to connect to the TCP command server to
happen later.

A non-nil RUN-COMMAND is supplied as the second command-line
argument to `racket--run.rkt' so the process can start by
immediately running a desired file.

Always displays the buffer. Never changes selected window.

Note that the value of the variable `racket--run.rkt' is applied
to the function variable `racket-adjust-run-rkt' before being
used. See the latter for more information."
  (when (racket--repl-live-p)
    (error "Racket REPL already running."))
  (racket--assert-version racket--minimum-required-version)
  (let ((run.rkt (funcall racket-adjust-run-rkt racket--run.rkt)))
    (with-current-buffer
        (make-comint racket--repl-buffer-name/raw ;w/o *stars*
                     racket-program
                     nil
                     run.rkt
                     (number-to-string racket-command-port)
                     (setq racket--cmd-auth (format "%S" `(auth ,(random))))
                     (format "%S" (or run-command
                                      (racket--repl-make-run-command nil))))
      ;; Use a local comint output filter function to detect first
      ;; output (e.g. Welcome to Racket banner). Only thereafter might
      ;; our backend TCP command server be ready for connections; no
      ;; point trying sooner. (I'd rather use a process sentinel, but
      ;; a comint process supplies no such "open" or "ready" event.)
      (add-hook 'comint-output-filter-functions
                #'racket-repl--startup-output-filter
                nil t)
      (let ((proc (get-buffer-process racket--repl-buffer-name)))
        (display-buffer (current-buffer)) ;show startup/banner sooner
        (message "Starting %s to run %s ..." racket-program run.rkt)
        (set-process-coding-system proc 'utf-8 'utf-8) ;for e.g. λ
        (racket-repl-mode)))))

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

;;; Connection to command process

(defvar racket--cmd-proc nil
  "Process for talking to the command server.
Most code should use `racket--cmd-open-p' to check this.")

(defun racket--cmd-open-p ()
  "Does an open process exist for the command server?"
  (and racket--cmd-proc
       (eq 'open (process-status racket--cmd-proc))))

(defun racket-repl--startup-output-filter (_txt)
  "As soon as the REPL process displays its first output --
presumably the Racket banner -- schedule an attempt to connect to
the command process. And having done our one job, remove
ourselves from the local cominit output filter functions."
  (run-at-time 0.5 nil #'racket--cmd-connect-attempt 1)
  (remove-hook 'comint-output-filter-functions
               #'racket-repl--startup-output-filter
               t))

(defvar racket--cmd-nonce->callback (make-hash-table :test 'eq)
  "A hash from nonce to callback function.")
(defvar racket--cmd-nonce 0
  "Number that increments for each command request we send.")

(defvar racket--cmd-connect-attempts 15
  "How many times to `racket--cmd-connect-attempt', at roughly 1 second intervals.")

(defun racket--cmd-connect-attempt (attempt)
  (unless (featurep 'make-network-process '(:nowait t))
    (error "Racket Mode needs Emacs to support the :nowait feature"))
  (setq
   racket--cmd-proc
   (make-network-process
    :name    "racket-command"
    :host    "127.0.0.1"
    :service racket-command-port
    :nowait  t
    :sentinel
    (lambda (proc event)
      ;;(message "sentinel got (%S %S) [attempt %s]" proc event attempt)
      (cond ((string-match-p "^open" event)
             (let ((buf (generate-new-buffer (concat " *" (process-name proc) "*"))))
               (set-process-buffer proc buf)
               (buffer-disable-undo buf))
             (set-process-filter proc #'racket--cmd-process-filter)
             (process-send-string proc (concat racket--cmd-auth "\n"))
             (message "Connected to %s process on port %s after %s attempt%s"
                      proc racket-command-port attempt (if (= 1 attempt) "" "s")))

            ((string-match-p "^failed" event)
             (delete-process proc) ;we'll get called with "deleted" event, below
             (when (<= attempt racket--cmd-connect-attempts)
               (run-at-time 1.0 nil
                            #'racket--cmd-connect-attempt
                            (1+ attempt))))

            ((or (string-match-p "^deleted" event)
                 (string-match-p "^connection broken by remote peer" event))
             (clrhash racket--cmd-nonce->callback)
             ;; If process has a buffer -- and do check that it does,
             ;; see #383 -- we can't `kill-buffer' now here in the
             ;; process sentinel. Instead do soon.
             (pcase (process-buffer proc)
               (`() nil)
               (buf (run-at-time 0.1 nil #'kill-buffer buf))))

            (t (message "sentinel surprised by (%S %S) [attempt %s]" proc event attempt)))))))

(defun racket--cmd-process-filter (proc string)
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
  (pcase response
    (`(debug-break . ,response)
     (run-at-time 0.001 nil #'racket--debug-on-break response))
    (`(,nonce . ,response)
     (let ((callback (gethash nonce racket--cmd-nonce->callback)))
       (when callback
         (remhash nonce racket--cmd-nonce->callback)
         (run-at-time 0.001 nil callback response))))
    (_ nil)))

(defun racket--repl-ensure-command-server ()
  "See the variable `racket--repl-wait-for-command-server'."
  (if racket-command-startup
      (progn
        (unless (racket--repl-live-p)
          (racket--repl-start))
        (with-timeout (racket-command-startup
                       (error "Could not connect to racket-command process on port %s. You probably need to `racket-repl-exit' and restart the Racket REPL."
                              racket-command-port))
          (while (not (racket--cmd-open-p))
            (message "Waiting to connect to racket-command process on port %s ... press C-g to quit waiting ..."
                     racket-command-port)
            (sit-for 1.0))))
    (unless (racket--repl-live-p)
      (error "Racket REPL not running. You need to `racket-run' or `racket-repl'."))
    (unless (racket--cmd-open-p)
      (error "Not connected to racket-command process on port %s.\nWait for the Racket REPL to start and try again, or, `racket-repl-exit' and restart the Racket REPL."
             racket-command-port))))

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
form."
  (racket--repl-ensure-command-server)
  (cl-incf racket--cmd-nonce)
  (when (and callback
             (not (equal callback #'ignore)))
    (puthash racket--cmd-nonce callback racket--cmd-nonce->callback))
  (process-send-string racket--cmd-proc
                       (format "%S\n" (cons racket--cmd-nonce
                                            command-sexpr))))

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

;;; Misc

(defun racket--repl-file-name+md5 ()
  "Return the file and MD5 running in the REPL, or nil.

The result can be nil if the REPL is not started, or if it is
running no particular file as with the `,top` command."
  (when (comint-check-proc racket--repl-buffer-name)
    (pcase (racket--cmd/await `(path+md5))
      (`(,(and (pred stringp) path) . ,md5)
       (cons (funcall racket-path-from-racket-to-emacs-function path)
             md5))
      (_ nil))))

(defun racket-repl-file-name ()
  "Return the file running in the REPL, or nil.

The result can be nil if the REPL is not started, or if it is
running no particular file."
  (when (comint-check-proc racket--repl-buffer-name)
    (pcase (racket--repl-file-name+md5)
      (`(,(and (pred stringp) path) . ,_md5) path)
      (_ nil))))

(defun racket--in-repl-or-its-file-p ()
  "Is current-buffer `racket-repl-mode' or buffer for file active in it?"
  (or (eq (current-buffer)
          (get-buffer racket--repl-buffer-name))
      (let ((buf-file  (racket--buffer-file-name))
            (repl-file (racket-repl-file-name)))
        (and buf-file repl-file (string-equal buf-file repl-file)))))

(defun racket-repl-switch-to-edit ()
  "Switch to the window for the buffer of the file running in the REPL.

If no buffer is visting the file, `find-file' it in `other-window'.

If the REPL is running no file -- if the prompt is `>` -- use the
most recent `racket-mode' buffer, if any."
  (interactive)
  (pcase (racket-repl-file-name)
    (`() (let ((buffer (racket--most-recent-racket-mode-buffer)))
           (unless buffer
             (user-error "There are no racket-mode buffers"))
           (pop-to-buffer buffer t)))
    (path (let ((buffer (find-buffer-visiting path)))
            (if buffer
                (pop-to-buffer buffer t)
              (other-window 1)
              (find-file path))))))

(defun racket--most-recent-racket-mode-buffer ()
  (cl-some (lambda (b)
             (with-current-buffer b
               (and (eq major-mode 'racket-mode) b)))
           (buffer-list)))

;;; send to REPL

(defun racket--send-region-to-repl (start end)
  "Internal function to send the region to the Racket REPL.

Before sending the region, calls `racket-repl' and
`racket--repl-forget-errors'. Also inserts a ?\n at the process
mark so that output goes on a fresh line, not on the same line as
the prompt.

Afterwards call `racket--repl-show-and-move-to-end'."
  (unless (and start end)
    (error "start and end must not be nil"))
  ;; Save the current buffer in case something changes it before we
  ;; call `comint-send-region'; see e.g. issue 407.
  (let ((source-buffer (current-buffer)))
    (racket-repl t)
    (racket--repl-forget-errors)
    (let ((proc (get-buffer-process racket--repl-buffer-name)))
      (with-racket-repl-buffer
        (save-excursion
          (goto-char (process-mark proc))
          (insert ?\n)
          (set-marker (process-mark proc) (point))))
      (with-current-buffer source-buffer
        (comint-send-region proc start end)
        (comint-send-string proc "\n")))
    (racket--repl-show-and-move-to-end)))

(defun racket-send-region (start end)
  "Send the current region (if any) to the Racket REPL."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (racket--send-region-to-repl start end))

(defun racket-send-definition (&optional prefix)
  "Send the current definition to the Racket REPL."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (if prefix
          (racket--debug-send-definition (point) end)
        (racket--send-region-to-repl (point) end)))))

(defun racket-send-last-sexp ()
  "Send the previous sexp to the Racket REPL.

When the previous sexp is a sexp comment the sexp itself is sent,
without the #; prefix."
  (interactive)
  (racket--send-region-to-repl (racket--repl-last-sexp-start)
                               (point)))

(defun racket-eval-last-sexp ()
  "Eval the previous sexp asynchronously and `message' the result."
  (interactive)
  (racket--cmd/async
   `(eval
     ,(buffer-substring-no-properties (racket--repl-last-sexp-start)
                                      (point)))
   (lambda (v)
     (message "%s" v))))

(defun racket--repl-last-sexp-start ()
  (save-excursion
    (condition-case ()
        (progn
          (backward-sexp)
          (if (save-match-data (looking-at "#;"))
              (+ (point) 2)
            (point)))
      (scan-error (user-error "There isn't a complete s-expression before point")))))

(defun racket--repl-forget-errors ()
  "Forget existing errors in the REPL.
Although they remain clickable they will be ignored by
`next-error' and `previous-error'"
  (with-racket-repl-buffer
    (compilation-forget-errors)
    ;; `compilation-forget-errors' may have just set
    ;; `compilation-messages-start' to a marker at position 1. But in
    ;; that case process output (including error messages) will be
    ;; inserted ABOVE the marker, in which case `next-error' won't see
    ;; them. Instead use a non-marker position like 1 or use nil.
    (when (and (markerp compilation-messages-start)
               (equal (marker-position compilation-messages-start) 1)
               (equal (marker-buffer compilation-messages-start) (current-buffer)))
      (setq compilation-messages-start nil))))

(add-hook 'racket--repl-before-run-hook #'racket--repl-forget-errors)

(defun racket--repl-show-and-move-to-end ()
  "Make the Racket REPL visible, and move point to end.
Keep original window selected."
  (display-buffer racket--repl-buffer-name)
  (save-selected-window
    (select-window (get-buffer-window racket--repl-buffer-name t))
    (comint-show-maximum-output)))

;;; Inline images in REPL

(defvar racket-image-cache-dir nil)

(defun racket-repl--list-image-cache ()
  "List all the images in the image cache."
  (and racket-image-cache-dir
       (file-directory-p racket-image-cache-dir)
       (let ((files (directory-files-and-attributes
                     racket-image-cache-dir t "racket-image-[0-9]*.png")))
         (mapcar #'car
                 (sort files (lambda (a b)
                               (< (float-time (nth 6 a))
                                  (float-time (nth 6 b)))))))))

(defun racket-repl--clean-image-cache ()
  "Clean all except for the last `racket-images-keep-last'
images in 'racket-image-cache-dir'."
  (interactive)
  (dolist (file (butlast (racket-repl--list-image-cache)
                         racket-images-keep-last))
    (delete-file file)))

(defun racket-repl--replace-images ()
  "Replace all image patterns with actual images"
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward  "\"#<Image: \\(.+racket-image-.+\\.png\\)>\"" nil t)
        ;; can't pass a filename to create-image because emacs might
        ;; not display it before it gets deleted (race condition)
        (let* ((file (match-string 1))
               (begin (match-beginning 0))
               (end (match-end 0)))
          (delete-region begin end)
          (goto-char begin)
          (if (and racket-images-inline (display-images-p))
              (insert-image (create-image file) "[image]")
            (goto-char begin)
            (insert "[image] ; use M-x racket-view-last-image to view"))
          (setq racket-image-cache-dir (file-name-directory file))
          (racket-repl--clean-image-cache))))))

(defun racket-view-last-image (n)
  "Open the last displayed image using `racket-images-system-viewer'.

With prefix arg, open the N-th last shown image."
  (interactive "p")
  (let ((images (reverse (racket-repl--list-image-cache))))
    (if (>= (length images) n)
        (start-process "Racket image view"
                       nil
                       racket-images-system-viewer
                       (nth (- n 1) images))
      (error "There aren't %d recent images" n))))

(defun racket-repl--normal-output-filter (_txt)
  (racket-repl--replace-images))

;;; racket-repl-mode

(defvar racket-repl-mode-map
  (racket--easy-keymap-define
   '(("C-m"             racket-repl-submit)
     ("C-j"             newline-and-indent)
     ("TAB"             indent-for-tab-command)
     ("C-M-u"           racket-backward-up-list)
     ("C-M-q"           prog-indent-sexp)
     ("C-a"             comint-bol)
     ("C-w"             comint-kill-region)
     ("<C-S-backspace>" comint-kill-whole-line)
     ("C-c C-e f"       racket-expand-file)
     ("C-c C-e x"       racket-expand-definition)
     ("C-c C-e e"       racket-expand-last-sexp)
     ("C-c C-e r"       racket-expand-region)
     ("M-C-y"           racket-insert-lambda)
     ("C-c C-d"         racket-doc)
     ("C-c C-."         racket-describe)
     ("M-."             racket-visit-definition)
     ("C-M-."           racket-visit-module)
     ("M-,"             racket-unvisit)
     ("C-c C-z"         racket-repl-switch-to-edit)
     ("C-c C-l"         racket-logger)
     ("C-c C-\\"        racket-repl-exit)
     ((")" "]" "}")     racket-insert-closing)))
  "Keymap for Racket REPL mode.")

(easy-menu-define racket-repl-mode-menu racket-repl-mode-map
  "Menu for Racket REPL mode."
  '("Racket"
    ["Break" comint-interrupt-subjob]
    ["Exit" racket-repl-exit]
    "---"
    ["Insert Lambda" racket-insert-lambda] ;λ in string breaks menu
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes]
    ("Macro Expand"
     ["File" racket-expand-file]
     ["Region" racket-expand-region  :active (region-active-p)]
     ["Definition" racket-expand-definition]
     ["Last S-Expression" racket-expand-last-sexp])
    "---"
    ["Visit Definition" racket-visit-definition]
    ["Visit Module" racket-visit-module]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Racket Documentation" racket-doc]
    ["Describe" racket-describe]
    "---"
    ["Switch to Edit Buffer" racket-repl-switch-to-edit]))

(define-derived-mode racket-repl-mode comint-mode "Racket-REPL"
  "Major mode for Racket REPL.
\\{racket-repl-mode-map}"
  (racket--common-variables)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-scroll-show-maximum-output nil) ;t slow for big outputs
  (setq-local mode-line-process nil)
  (setq-local comint-input-filter #'racket-repl--input-filter)
  (add-hook 'comint-output-filter-functions #'racket-repl--normal-output-filter nil t)
  (compilation-setup t)
  (setq-local
   compilation-error-regexp-alist
   (list
    ;; Any apparent file:line:col
    (list (rx (group-n 1 (+? (not (syntax whitespace))))
              (any ?\: ?\.)
              (group-n 2 (+ digit))
              (any ?\: ?\.)
              (group-n 3 (+ digit)))
          #'racket--adjust-group-1 2 3)
     ;; Any path struct
     (list (rx "#<path:" (group-n 1 (+? (not (any ?\>)))) ?\>)
           #'racket--adjust-group-1 nil nil 0))))

(defun racket--adjust-group-1 ()
  (list (funcall racket-path-from-racket-to-emacs-function (match-string 1))))

(provide 'racket-repl)

;; racket-repl.el ends here

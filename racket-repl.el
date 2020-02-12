;;; racket-repl.el -*- lexical-binding: t; -*-

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

(require 'racket-complete)
(require 'racket-describe)
(require 'racket-eldoc)
(require 'racket-custom)
(require 'racket-common)
(require 'racket-util)
(require 'racket-visit)
(require 'racket-cmd)
(require 'comint)
(require 'compile)
(require 'easymenu)
(require 'cl-lib)
(require 'rx)

;; Don't (require 'racket-debug). Mutual dependency. Instead:
(declare-function  racket--debug-send-definition "racket-debug" (beg end))
(autoload         'racket--debug-send-definition "racket-debug")
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
  (unless (racket--repl-live-p)
    (racket--repl-start))
  (racket--repl-display-buffer-and-move-to-end)
  (unless noselect
    (select-window (get-buffer-window racket--repl-buffer-name t))))

;;; Run

;; Note: These commands are to be run when current-buffer is a
;; `racket-mode' buffer. The reason they are defined here is because
;; they use a `racket-repl-mode' buffer, and, one could use
;; `racket-mode' to edit files without using these commands.

;;;###autoload
(defun racket-run (&optional prefix)
  "Save and evaluate the buffer in REPL.

With one C-u prefix, uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting.

With two C-u prefixes, instruments code for step debugging. See
`racket-debug-mode' and the variable `racket-debuggable-files'.

If point is within a Racket module form, the REPL \"enters\" that
submodule (uses its language info and namespace).

When you run again, the file is evaluated from scratch --- the
custodian releases resources like threads and the evaluation
environment is reset to the contents of the file. In other words,
like DrRacket, this provides the predictability of a \"static\"
baseline, plus the ability to explore interactively using the
REPL.

See also `racket-run-and-switch-to-repl', which is even more like
DrRacket's Run because it selects the REPL window (gives it the
focus), too.

When `racket-retry-as-skeleton' is true, if your source file has
an error, a \"skeleton\" of your file is evaluated to get
identifiers from module languages, require forms, and
definitions. That way, things like completion and
`racket-repl-describe' are more likely to work while you edit the
file to fix the error. If not even the \"skeleton\" evaluation
succeeds, you'll have only identifiers provided by racket/base,
until you fix the error and run again.

Output in the Racket REPL buffer that describes a file and
position is automatically \"linkified\". Examples of such text
include:

- Racket error messages.
- rackunit test failure location messages.
- print representation of path objects.

To visit these locations, move point there and press RET or mouse
click. Or, use the standard `next-error' and `previous-error'
commands."
  (interactive "P")
  (racket--repl-run (racket--what-to-run)
                    (pcase prefix
                      (`(4)  'high)
                      (`(16) 'debug)
                      (_     racket-error-context))))

(defun racket-run-with-errortrace ()
  "Run with `racket-error-context' temporarily set to \"high\".
This is just `racket-run' with a C-u prefix. Defined as a function so
it can be a menu target."
  (interactive)
  (racket-run '(4)))

(defun racket-run-with-debugging ()
  "Run with `racket-error-context' temporarily set to 'debug.
This is just `racket-run' with a double C-u prefix. Defined as a
function so it can be a menu target."
  (interactive)
  (racket-run '(16)))

(defun racket-run-and-switch-to-repl (&optional prefix)
  "This is `racket-run' followed by `racket-repl'."
  (interactive "P")
  (racket-run prefix)
  (racket-repl))

(defun racket-test (&optional coverage)
  "Run the \"test\" submodule.

With prefix, runs with coverage instrumentation and highlights
uncovered code.

Put your tests in a \"test\" submodule. For example:

#+BEGIN_SRC racket
    (module+ test
      (require rackunit)
      (check-true #t))
#+END_SRC

Any rackunit test failure messages show the location. You may use
`next-error' to jump to the location of each failing test.

See also:
- `racket-fold-all-tests'
- `racket-unfold-all-tests'
"
  (interactive "P")
  (let ((mod-path (list 'submod (racket--buffer-file-name) 'test))
        (buf (current-buffer)))
    (if (not coverage)
        (racket--repl-run mod-path)
      (message "Running test submodule with coverage instrumentation...")
      (racket--repl-run
       mod-path
       'coverage
       (lambda ()
         (message "Getting coverage results...")
         (racket--cmd/async
          `(get-uncovered)
          (lambda (xs)
            (pcase xs
              (`() (message "Full coverage."))
              ((and xs `((,beg0 . ,_) . ,_))
               (message "Missing coverage in %s place(s)." (length xs))
               (with-current-buffer buf
                 (dolist (x xs)
                   (let ((o (make-overlay (car x) (cdr x) buf)))
                     (overlay-put o 'name 'racket-uncovered-overlay)
                     (overlay-put o 'priority 100)
                     (overlay-put o 'face font-lock-warning-face)))
                 (goto-char beg0)))))))))))

(add-hook 'racket--repl-before-run-hook #'racket--remove-coverage-overlays)

(defun racket--remove-coverage-overlays ()
  (remove-overlays (point-min) (point-max) 'name 'racket-uncovered-overlay))

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

(defvar racket--repl-before-run-hook nil
  "Thunks to do before each `racket--repl-run'.")

(defvar racket--repl-after-run-hook nil
  "Thunks to do after each `racket--repl-run'.
Here \"after\" means that the run has completed and e.g. the REPL
is waiting at another prompt.")

(defun racket--repl-run (&optional what-to-run context-level callback)
  "Do an initial or subsequent run.

WHAT-TO-RUN should be a cons of a file name to a list of
submodule symbols. Or if nil, defaults to `racket--what-to-run'.

CONTEXT-LEVEL should be a valid value for the variable
`racket-error-context', 'coverage, or 'profile. Or if nil,
defaults to the variable `racket-error-context'.

CALLBACK is supplied to `racket--repl-run' and is used as the
callback for `racket--cmd/async'; it may be nil which is
equivalent to #'ignore.

- If the REPL is not live, start our backend run.rkt passing
  the file to run as a command-line argument.

- If the REPL is live, send a 'run command to the backend's TCP
  server."
  (unless (eq major-mode 'racket-mode)
    (user-error "Only works from a `racket-mode' buffer"))
  (run-hook-with-args 'racket--repl-before-run-hook)
  (let ((cmd (racket--repl-make-run-command (or what-to-run (racket--what-to-run))
                                            (or context-level racket-error-context)))
        (after (lambda (_ignore)
                 (run-hook-with-args 'racket--repl-after-run-hook)
                 (when callback
                   (funcall callback)))))
    (cond ((and (racket--repl-live-p) (racket--cmd-open-p))
           (racket--cmd/async cmd after)
           (racket--repl-display-buffer-and-move-to-end))
          (t
           (racket--repl-start cmd)
           (racket--cmd/async `(no-op) after)))))

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

(defun racket--repl-live-p ()
  "Does the Racket REPL buffer exist and have a live Racket process?"
  (comint-check-proc racket--repl-buffer-name))

(defun racket--repl-start (&optional run-command)
  "Start the Racket process, creating `racket-repl-mode' buffer if necessary.

This does not display the buffer or change the selected window.

This function does not block waiting for the TCP command server
to be ready. Instead: After `make-comint' creates the process and
buffer, we set up `racket--repl-startup-output-filter' and return
immediately. Later that filter will detect the first output from
the server, and call `racket--cmd-connect-schedule-start'. Even
that does not block waiting for a connection, but see that for
more details.

A non-nil RUN-COMMAND is supplied as the second command-line
argument to `racket--run.rkt' so the process can start by
immediately running a desired file.

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
                #'racket--repl-startup-output-filter
                nil t)
      (add-hook 'racket--cmd-after-open-thunks
                #'racket--repl-refresh-namespace-symbols)
      (let ((proc (get-buffer-process racket--repl-buffer-name)))
        (message "Starting %s to run %s ..." racket-program run.rkt)
        (set-process-coding-system proc 'utf-8 'utf-8) ;for e.g. λ
        (racket-repl-mode)))))

(defun racket--repl-startup-output-filter (_txt)
  "As soon as the REPL process displays its first output --
presumably the Racket banner -- schedule an attempt to connect to
the command process. And having done our one job, remove
ourselves from the local cominit output filter functions."
  (racket--cmd-connect-schedule-start)
  (remove-hook 'comint-output-filter-functions
               #'racket--repl-startup-output-filter
               t))

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

Afterwards call `racket--repl-display-buffer-and-move-to-end'."
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
    (racket--repl-display-buffer-and-move-to-end)))

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

(defun racket--repl-display-buffer-and-move-to-end ()
  "Display the Racket REPL buffer in a window, and move point to end.
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

(defun racket--repl-normal-output-filter (_txt)
  (racket-repl--replace-images))

;;; Completion

(defvar racket--repl-namespace-symbols nil)

(defun racket--repl-refresh-namespace-symbols ()
  (racket--cmd/async
   '(syms)
   (lambda (syms)
     (setq racket--repl-namespace-symbols syms))))

(add-hook 'racket--repl-after-run-hook   #'racket--repl-refresh-namespace-symbols)

(defun racket--repl-completion-candidates-for-prefix (prefix)
  (all-completions prefix racket--repl-namespace-symbols))

(defun racket-repl-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

Completion candidates are drawn from the REPL namespace symbols.

Returns extra :company-doc-buffer and :company-location
properties for use by the `company-mode' backend `company-capf'
-- but not :company-docsig, because it is frequently impossible
to supply this quickly enough or at all."
  (racket--call-with-completion-prefix-positions
   (lambda (beg end)
     (list beg
           end
           (completion-table-dynamic
            #'racket--repl-completion-candidates-for-prefix)
           :predicate #'identity
           :exclusive 'no
           :company-doc-buffer #'racket--repl-company-doc-buffer
           :company-location #'racket--repl-company-location))))

(defun racket--repl-company-doc-buffer (str)
  (racket--do-describe 'namespace str))

(defun racket--repl-company-location (str)
  (pcase (racket--cmd/await `(def-in-namespace ,str))
    (`(,path ,line ,_) (cons path line))))

(defun racket-repl-eldoc-function ()
  "A value for the variable `eldoc-documentation-function'.

By default `racket-repl-mode' sets `eldoc-documentation-function'
to nil -- no `eldoc-mode' support. You may set it to this
function in a `racket-repl-mode-hook' if you really want to use
`eldoc-mode'. But it is not a very satisfying experience because
Racket is not a very \"eldoc friendly\" language.

Sometimes we can discover argument lists from source -- but this
can be slow.

For code that has been run in the REPL, we can use its namespace
to discover contracts or types -- but otherwise we cannot.

Many interesting Racket forms are syntax (macros) without any
easy way to discover their \"argument lists\". Similarly many
Racket functions or syntax are defined in #%kernel and the source
is not available. If they have documentation with a \"bluebox\",
we can show it -- but often it is not a single-line format
typical for eldoc.

So if you are expecting an eldoc experience similar to Emacs
Lisp, you will be disappointed.

A more satisfying experience is to use `racket-repl-describe' or
`racket-repl-documentation'."
  (racket--do-eldoc 'namespace))

;;; describe

(defun racket-repl-describe (&optional prefix)
"Describe the identifier at point in a `*Racket Describe*` buffer.

The intent is to give a quick reminder or introduction to
something, regardless of whether it has installed documentation
-- and to do so within Emacs, without switching to a web browser.

This buffer is also displayed when you use `company-mode' and
press F1 or C-h in its pop up completion list.

- If the identifier has installed Racket documentation, then a
  simplified version of the HTML is presented in the buffer,
  including the \"blue box\", documentation prose, and examples.

- Otherwise, if the identifier is a function, then its signature
  is displayed, for example `(name arg-1-name arg-2-name)`. If it
  has a contract or a Typed Racket type, that is also displayed.

You can quit the buffer by pressing q. Also, at the bottom of the
buffer are Emacs buttons -- which you may navigate among using
TAB, and activate using RET -- for `racket-visit-definition' and
`racket-doc'."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Describe: "
                                            racket--repl-namespace-symbols)
    ((and (pred stringp) str)
     (racket--do-describe 'namespace str t
                          (lambda ()
                            (racket--do-visit-def-or-mod `(def namespace ,str)))
                          (lambda ()
                            (racket--cmd/async `(doc namespace ,str)
                                               #'browse-url))))))

;;; Visit

(defun racket-repl-visit-definition (&optional prefix)
  "Visit definition of identifier at point.

If there is no identifier at point, prompt for it.

With a prefix, always prompt for the identifier.

Use `racket-unvisit' to return.

Please keep in mind the following limitations:

- Finds symbols defined in the REPL's namespace, which only
  includes imported and module binding -- but not local bindings.

- If the definition is found in Racket's \"#%kernel\" module, it
  will tell you so but won't visit the definition site."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Visit definition of: "
                                            racket--repl-namespace-symbols)
    ((and (pred stringp) str) (racket--repl-visit-symbol-definition str))))

;; TODO: Move to `racket-xp-mode', or arrange for this to call that or
;; this depending on current-buffer.
(defun racket-lispy-visit-symbol-definition (str)
  "Function called by lispy.el's `lispy-goto-symbol' for Racket
symbol definition lookup."
  (racket--repl-visit-symbol-definition str))

(defun racket--repl-visit-symbol-definition (str)
  (racket--do-visit-def-or-mod `(def namespace ,str)))

;;; Doc

(defun racket-repl-documentation (&optional prefix)
  "View documentation of the identifier or string at point.

Uses the default external web browser.

If point is an identifier required in the current namespace that
has help, opens the web browser directly at that help
topic. (i.e. Uses the identifier variant of racket/help.)

Otherwise, opens the 'search for a term' page, where you can
choose among multiple possibilities. (i.e. Uses the string
variant of racket/help.)

With a C-u prefix, prompts for the identifier or quoted string,
instead of looking at point."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Documentation for: "
                                            racket--repl-namespace-symbols)
    ((and (pred stringp) str) (racket--cmd/async `(doc namespace ,str)
                                                 #'browse-url))))

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
     ("C-c C-d"         racket-repl-documentation)
     ("C-c C-."         racket-repl-describe)
     ("M-."             racket-repl-visit-definition)
     ("C-M-."           racket-visit-module)
     ("M-,"             racket-unvisit)
     ("C-c C-z"         racket-repl-switch-to-edit)
     ("C-c C-l"         racket-logger)
     ("C-c C-\\"        racket-repl-exit)
     ((")" "]" "}")     racket-insert-closing)))
  "Keymap for Racket REPL mode.")

(easy-menu-define racket-repl-mode-menu racket-repl-mode-map
  "Menu for Racket REPL mode."
  '("Racket-REPL"
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
  (setq-local completion-at-point-functions (list #'racket-repl-complete-at-point))
  (setq-local eldoc-documentation-function nil)
  (add-hook 'comint-output-filter-functions #'racket--repl-normal-output-filter nil t)
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

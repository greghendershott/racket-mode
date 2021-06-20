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

(require 'racket-browse-url)
(require 'racket-complete)
(require 'racket-describe)
(require 'racket-doc)
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
(require 'cl-macs)
(require 'rx)
(require 'xref)
(require 'semantic/symref/grep)

;; Don't (require 'racket-debug). Mutual dependency. Instead:
(declare-function  racket--debug-send-definition "racket-debug" (beg end))
(autoload         'racket--debug-send-definition "racket-debug")
(declare-function  racket--debuggable-files      "racket-debug" (file-to-run))
(autoload         'racket--debuggable-files      "racket-debug")

;;; racket-mode <=> racket-repl-mode associations

;; There are some nuances here regarding these variables being
;; buffer-local or not, and, whether the variables have any meaning in
;; certain modes, or not. We use Emacs variable semantics to handle
;; the association between `racket-mode' edit buffers and
;; `racket-repl-mode' buffers, for a variety of use cases the user
;; might prefer. These range from all edit buffers sharing one REPL
;; buffer (the traditional default for Racket Mode), up to each edit
;; buffers having its own REPL (as in Dr Racket), or anything in
;; between (such as one REPL per projectile project, or whatever).
;; Some of these scenarios might benefit from some higher-level UI.
;; But ultimately they reduce to setting the variable
;; `racket-repl-buffer-name' globally and/or locally for `racket-mode'
;; buffers -- that is the fundamental representation. Similarly, each
;; `racket-repl-mode' buffer has an always-buffer-local value for the
;; variable `racket--repl-session-id'. (Note that
;; `racket-repl-buffer-name' only has meaning for `racket-mode'
;; buffers, and `racket--repl-session-id' only has meaning for
;; `racket-repl-mode' buffers. Emacs variables exist for all buffers
;; using all major modes. All we can do is remember in which buffers
;; they mean something as opposed to being ignored..)

(defvar racket-repl-buffer-name "*Racket REPL*"
  "The name of the `racket-repl-mode' buffer associated with `racket-mode' buffer.

Important: This variable only means something in each
`racket-mode' buffer. It has no meaning in `racket-repl-mode' or
other buffers.

By default all `racket-mode' edit buffers share the same REPL.
However, a buffer may `setq-local' this to some other value. See
the defcustom `racket-repl-buffer-name-function' and example
values for it in racket-repl-buffer-name.el.")

(defvar-local racket--repl-session-id nil
  "The REPL session ID returned from the back end.

Must be supplied in command requests, although for some commands
it can be nil.

Important: This variable only means something in each
`racket-repl-mode' buffer. It has no meaning in `racket-mode' or
other buffers. Futhermore, it is /always/ buffer-local in each
`racket-repl-mode' buffer. Instead of accessing this directly,
use the function `racket--repl-session-id', which helps select
the correct `racket-repl-mode' buffer, if any.")

(defun racket--repl-session-id ()
  "Use this to get a REPL session ID.
The result might be nil if no REPL buffer exists, or if it does
but does not have a live session."
  (if (eq major-mode 'racket-repl-mode)
      racket--repl-session-id
    (let ((buffer (get-buffer racket-repl-buffer-name)))
      (when buffer
        (with-current-buffer racket-repl-buffer-name
          racket--repl-session-id)))))

(defun racket--call-with-repl-buffer (thunk)
  (pcase (get-buffer (if (eq major-mode 'racket-repl-mode)
                         (buffer-file-name)
                         racket-repl-buffer-name))
    ((and (pred bufferp) buf)
     (with-current-buffer buf (funcall thunk)))))

(defmacro with-racket-repl-buffer (&rest body)
  "Execute forms in BODY with `racket-repl-mode' temporarily current buffer."
  (declare (indent 0) (debug t))
  `(racket--call-with-repl-buffer (lambda () ,@body)))

(defun racket--repl-live-p ()
  "Does a Racket REPL buffer exist and have a live Racket process?"
  (and (racket--repl-session-id)
       (comint-check-proc racket-repl-buffer-name)))

;;; Misc

(defun racket-repl--input-filter (str)
  "Don't save anything matching `racket-history-filter-regexp'."
  (not (string-match racket-history-filter-regexp str)))

(defalias 'racket-repl-eval-or-newline-and-indent #'racket-repl-submit)

(defun racket-repl-submit (&optional prefix)
  "Submit your input to the Racket REPL.

If the REPL is running a Racket lang whose language-info has a
'drracket:submit-predicate, that is first called to see if the
input is valid to be submitted.

With \\[universal-argument] after sending your input and a
newline, also calls `process-send-eof' -- because some langs
require EOF to mark the end of an interactive
expression/statement."
  (interactive "P")
  (let* ((proc (get-buffer-process (current-buffer)))
         (_    (unless proc (user-error "Current buffer has no process")))
         (text (substring-no-properties (funcall comint-get-old-input)))
         (submitp
          (if racket-use-repl-submit-predicate
              (cl-case (racket--cmd/await (racket--repl-session-id)
                                          `(repl-submit? ,text t))
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

(defun racket-repl-break ()
  "Send a break to the REPL program's main thread."
  (interactive)
  (cond ((racket--cmd-open-p) ;don't auto-start the back end
         (racket--cmd/async (racket--repl-session-id) `(break break)))
        (t
         (user-error "Back end is not running"))))

(defun racket-repl-exit (&optional killp)
  "Send a terminate break to the REPL program's main thread.

If your program is running, equivalent to `racket-repl-break'.

If already at the REPL prompt, effectively the same as entering
\"(exit)\" at the prompt, but works even when the module language
doesn't provide any binding for \"exit\".

With \\[universal-argument] terminates the entire Racket Mode
back end process --- the command server and all REPL sessions."
  (interactive "P")
  (cond (killp
         (message "Killing entire Racket Mode back end process")
         (racket--cmd-close))
        ((racket--cmd-open-p) ;don't auto-start the back end
         (racket--cmd/async (racket--repl-session-id) `(break terminate)))
        (t
         (user-error "Back end is not running"))))

;;;###autoload
(defun racket-repl (&optional noselect)
  "Show a Racket REPL buffer in some window.

*IMPORTANT*

The main, intended use of Racket Mode's REPL is that you
`find-file' some specific .rkt file, then `racket-run' it. The
REPL will then match that file.

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
  (cl-labels
      ((display-and-maybe-select
        ()
        (display-buffer racket-repl-buffer-name)
        (unless noselect
          (select-window (get-buffer-window racket-repl-buffer-name t)))))
    (if (racket--repl-live-p)
        (display-and-maybe-select)
      (racket--repl-start
       (lambda ()
         (racket--repl-refresh-namespace-symbols)
         (display-and-maybe-select))))))

;;; Run

;; Note: These commands are to be run when current-buffer is a
;; `racket-mode' buffer. The reason they are defined here is because
;; they use a `racket-repl-mode' buffer, and, one could use
;; `racket-mode' to edit files without using these commands.

;;;###autoload
(defun racket-run (&optional prefix)
  "Save the buffer in REPL and run your program.

As well as evaluating the outermost, file module, automatically
runs the submodules specified by the customization variable
`racket-submodules-to-run'.

See also `racket-run-module-at-point', which runs just the
specific module at point.

With \\[universal-argument] uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting.

With \\[universal-argument] \\[universal-argument] instruments
code for step debugging. See `racket-debug-mode' and the variable
`racket-debuggable-files'.

Each run occurs within a Racket custodian. Any prior run's
custodian is shut down, releasing resources like threads and
ports. Each run's evaluation environment is reset to the contents
of the source file. In other words, like Dr Racket, this provides
the benefit that your source file is the \"single source of
truth\". At the same time, the run gives you a REPL inside the
namespace of the module, giving you the ability to explore it
interactively. Any explorations are temporary, unless you also
make them to your source file, they will be lost on the next run.

See also `racket-run-and-switch-to-repl', which is even more like
Dr Racket's Run command because it selects the REPL window after
running.

In the `racket-repl-mode' buffer, output that describes a file
and position is automatically \"linkified\". Examples of such
text include:

- Racket error messages.
- rackunit test failure location messages.
- print representation of path objects.

To visit these locations, move point there and press RET or mouse
click. Or, use the standard `next-error' and `previous-error'
commands."
  (interactive "P")
  (racket--repl-run (list (racket--buffer-file-name))
                    racket-submodules-to-run
                    (pcase prefix
                      (`(4)  'high)
                      (`(16) 'debug)
                      (_     racket-error-context))))

;;;###autoload
(defun racket-run-module-at-point (&optional prefix)
  "Save the buffer and run the module at point.

Like `racket-run' but runs the innermost module around point,
which is determined textually by looking for \"module\",
\"module*\", or \"module+\" forms nested to any depth, else
simply the outermost, file module."
  (interactive "P")
  (racket--repl-run (racket--what-to-run)
                    '()
                    (pcase prefix
                      (`(4)  'high)
                      (`(16) 'debug)
                      (_     racket-error-context))))

(defun racket-run-with-errortrace ()
  "Run with `racket-error-context' temporarily set to \"high\".

This is equivalent to \\[universal-argument] \\[racket-run].

Defined as a function so it can be a menu target."
  (interactive)
  (racket-run '(4)))

(defun racket-run-with-debugging ()
  "Run with `racket-error-context' temporarily set to 'debug.

This is equivalent to \\[universal-argument] \\[universal-argument] \\[racket-run].

Defined as a function so it can be a menu target."
  (interactive)
  (racket-run '(16)))

(defun racket-run-and-switch-to-repl (&optional prefix)
  "This is `racket-run' followed by selecting the REPL buffer window.

This is similar to how Dr Racket behaves.

To make it even more similar, you may add `racket-repl-clear' to
the variable `racket-before-run-hook'."
  (interactive "P")
  (racket--repl-run (list (racket--buffer-file-name))
                    racket-submodules-to-run
                    (pcase prefix
                      (`(4)  'high)
                      (`(16) 'debug)
                      (_     racket-error-context))
                    (lambda ()
                      (display-buffer racket-repl-buffer-name)
                      (select-window (get-buffer-window racket-repl-buffer-name t)))))

(defun racket-test (&optional coverage)
  "Run the \"test\" submodule.

Put your tests in a \"test\" submodule. For example:

#+BEGIN_SRC racket
    (module+ test
      (require rackunit)
      (check-true #t))
#+END_SRC

Any rackunit test failure messages show the location. You may use
`next-error' to jump to the location of each failing test.

With \\[universal-argument] also runs the tests with coverage
instrumentation and highlights uncovered code using
`font-lock-warning-face'.

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
       '()
       'coverage
       (lambda ()
         (message "Getting coverage results...")
         (racket--cmd/async
          (racket--repl-session-id)
          `(get-uncovered)
          (lambda (xs)
            (pcase xs
              (`() (message "Full coverage."))
              ((and xs `((,beg0 . ,_) . ,_))
               (message "Missing coverage in %s place(s)." (length xs))
               (with-current-buffer buf
                 (with-silent-modifications
                   (overlay-recenter (point-max))
                   (dolist (x xs)
                     (let ((o (make-overlay (car x) (cdr x) buf)))
                       (overlay-put o 'name 'racket-uncovered-overlay)
                       (overlay-put o 'priority 100)
                       (overlay-put o 'face font-lock-warning-face)))
                   (goto-char beg0))))))))))))

(add-hook 'racket--repl-before-run-hook #'racket--remove-coverage-overlays)

(defun racket--remove-coverage-overlays ()
  (remove-overlays (point-min) (point-max) 'name 'racket-uncovered-overlay))

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
  "Thunks to do before each `racket--repl-run'.

This hook is for internal use by Racket Mode. An equivalent hook
for end user customization is `racket-before-run-hook'.")

(defvar racket--repl-after-run-hook nil
  "Thunks to do after each `racket--repl-run'.

This hook is for internal use by Racket Mode. An equivalent hook
for end user customization is `racket-after-run-hook'.

Here \"after\" means that the run has completed and e.g. the REPL
is waiting at another prompt.")

(defun racket--repl-run (&optional what-to-run extra-submods context-level callback)
  "Do an initial or subsequent run.

WHAT-TO-RUN should be a cons of a file name to a list of
submodule symbols. Or if nil, defaults to `racket--what-to-run'.

EXTRA-SUBMODS should be a list of symbols, names of extra
submodules to run, e.g. '(test main). This is intended for use by
`racket-run', which more closely emulates DrRacket, as opposed to
`racket-run-module-at-point'.

CONTEXT-LEVEL should be a valid value for the variable
`racket-error-context', 'coverage, or 'profile. Or if nil,
defaults to the variable `racket-error-context'.

CALLBACK is used as the callback for `racket--cmd/async'; it may
be nil which is equivalent to #'ignore.

- If the REPL is not live, create it.

- If the REPL is live, send a 'run command to the backend's TCP
  server."
  (unless (eq major-mode 'racket-mode)
    (user-error "Only works from a `racket-mode' buffer"))
  (run-hook-with-args 'racket--repl-before-run-hook) ;ours
  (run-hook-with-args 'racket-before-run-hook)       ;users'
  (let* ((cmd (racket--repl-make-run-command (or what-to-run (racket--what-to-run))
                                             extra-submods
                                             (or context-level racket-error-context)))
         (buf (current-buffer))
         (after (lambda (_ignore)
                  (with-current-buffer buf
                    (run-hook-with-args 'racket--repl-after-run-hook) ;ours
                    (run-hook-with-args 'racket-after-run-hook)       ;users'
                    (when callback
                      (funcall callback))))))
    (cond ((racket--repl-live-p)
           (unless (racket--repl-session-id)
             (error "No REPL session"))
           (racket--cmd/async (racket--repl-session-id) cmd after)
           (display-buffer racket-repl-buffer-name))
          (t
           (racket--repl-start
            (lambda ()
              (when noninteractive
                (princ "{racket--repl-run}: callback from racket--repl-start called\n"))
              (with-current-buffer buf
                (unless (racket--repl-session-id)
                  (error "No REPL session"))
                (racket--cmd/async (racket--repl-session-id) cmd after)
                (display-buffer racket-repl-buffer-name))))))))

(defun racket--repl-make-run-command (what-to-run extra-submods context-level)
  "Form a `run` command sexpr for the backend.
WHAT-TO-RUN may be nil, meaning just a `racket/base` namespace."
  (let ((context-level (or context-level racket-error-context)))
    (list 'run
          what-to-run
          extra-submods
          racket-memory-limit
          racket-pretty-print
          (window-width)
          (racket--char-pixel-width)
          context-level
          racket-user-command-line-arguments
          (when (and what-to-run (eq context-level 'debug))
            (racket--debuggable-files (car what-to-run))))))

(defun racket--char-pixel-width ()
  (with-temp-buffer
    (insert "M")
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (car (window-text-pixel-size nil (line-beginning-position) (point))))))

(defun racket--repl-start (callback)
  "Create a `comint-mode' / `racket-repl-mode' buffer connected to a REPL session.

Sets `racket--repl-session-id'.

This does not display the buffer or change the selected window."
  ;; Issue the command to learn the ephemeral TCP port chosen by the
  ;; back end for REPL I/O. As a bonus, this will start the back end
  ;; if necessary.
  (when noninteractive (princ "{racket--repl-start}: entered\n"))
  (racket--cmd/async
   nil
   `(repl-tcp-port-number)
   (lambda (repl-tcp-port-number)
     (when noninteractive
       (princ (format "{racket--repl-start}: (repl-tcp-port-number) replied %s\n" repl-tcp-port-number)))
     (with-current-buffer (get-buffer-create racket-repl-buffer-name)
       ;; Add a pre-output hook that -- possibly over multiple calls
       ;; to accumulate text -- reads `(ok ,id) to set
       ;; `racket--repl-session-id' then removes itself.
       (let ((hook      nil)
             (read-buf  (generate-new-buffer " *racket-repl-session-id-reader*")))
         (when noninteractive
           (princ (format "{racket--repl-start}: buffer is '%s'\n" read-buf)))
         (setq hook (lambda (txt)
                      (when noninteractive
                        (princ (format "{racket--repl-start}: early pre-output-hook called '%s'\n" txt)))
                      (with-current-buffer read-buf
                        (goto-char (point-max))
                        (insert txt)
                        (goto-char (point-min)))
                      (pcase (ignore-errors (read read-buf))
                        (`(ok ,id)
                         (when noninteractive
                           (princ (format "{racket--repl-start}: %s\n" id)))
                         (setq racket--repl-session-id id)
                         (run-with-timer 0.001 nil callback)
                         (remove-hook 'comint-preoutput-filter-functions hook t)
                         (prog1
                             (with-current-buffer read-buf
                               (buffer-substring (if (eq (char-after) ?\n)
                                                     (1+ (point))
                                                   (point))
                                                 (point-max)))
                           (kill-buffer read-buf)))
                        (_ ""))))
         (add-hook 'comint-preoutput-filter-functions hook nil t))

       (condition-case ()
           (progn
             (make-comint-in-buffer racket-repl-buffer-name
                                    (current-buffer)
                                    (cons "127.0.0.1" repl-tcp-port-number))
             (process-send-string (get-buffer-process (current-buffer))
                                  (format "\"%s\"\n" racket--cmd-auth))
             (when noninteractive
               (princ "{racket--repl-start}: did process-send-string of auth\n"))
             (set-process-coding-system (get-buffer-process (current-buffer))
                                        'utf-8 'utf-8) ;for e.g. λ
             ;; Buffer might already be in `racket-repl-mode' -- e.g.
             ;; `racket-repl-exit' was used and now we're
             ;; "restarting". In that case avoid re-initialization
             ;; that is at best unnecessary or at worst undesirable
             ;; (e.g. `comint-input-ring' would lose input history).
             (unless (eq major-mode 'racket-repl-mode)
               (when noninteractive
                 (princ "{racket--repl-start}: (racket-repl-mode)\n"))
               (racket-repl-mode)))
         (file-error
          (let ((kill-buffer-query-functions nil)
                (kill-buffer-hook nil))
            (kill-buffer)) ;don't leave partially initialized REPL buffer
          (message "Could not connect to REPL server at 127.0.0.1:%s" repl-tcp-port-number)))))))

;;; Misc

(defun racket-repl-file-name ()
  "Return the file running in the REPL, or nil.

The result can be nil if the REPL is not started, or if it is
running no particular file."
  (when (comint-check-proc racket-repl-buffer-name)
    (racket--cmd/await (racket--repl-session-id) `(path))))

(defun racket--in-repl-or-its-file-p ()
  "Is current-buffer `racket-repl-mode' or buffer for file active in it?"
  (or (eq (current-buffer)
          (get-buffer racket-repl-buffer-name))
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
    ((and (pred stringp) path)
     (pcase (find-buffer-visiting path)
       ((and (pred bufferp) buffer) (pop-to-buffer buffer t))
       (_ (other-window 1)
          (find-file path))))
    (_ (pcase (racket--most-recent-racket-mode-buffer)
         ((and (pred bufferp) buffer) (pop-to-buffer buffer t))
         (_ (user-error "There are no racket-mode buffers"))))))

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

Afterwards displays the buffer in some window."
  (unless (and start end)
    (error "start and end must not be nil"))
  ;; Save the current buffer in case something changes it before we
  ;; call `comint-send-region'; see e.g. issue 407.
  (let ((source-buffer (current-buffer)))
    (racket-repl t)
    (racket--repl-forget-errors)
    (let ((proc (get-buffer-process racket-repl-buffer-name)))
      (with-racket-repl-buffer
        (save-excursion
          (goto-char (process-mark proc))
          (insert ?\n)
          (set-marker (process-mark proc) (point))))
      (with-current-buffer source-buffer
        (comint-send-region proc start end)
        (comint-send-string proc "\n")))
    (display-buffer racket-repl-buffer-name)))

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
  (unless (racket--repl-live-p)
    (user-error "No REPL session available"))
  (racket--cmd/async
   (racket--repl-session-id)
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

;;; Inline images in REPL

(defvar racket-image-cache-dir nil)

(defvar racket-image-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'racket-view-image)
    (define-key map "\C-m" #'racket-view-image)
    map)
  "Keymap for images.")

(defun racket-repl--list-image-cache ()
  "List all the images in the image cache."
  (and racket-image-cache-dir
       (file-directory-p racket-image-cache-dir)
       (let ((files (directory-files-and-attributes
                     racket-image-cache-dir t "^racket-image-.+")))
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

(defun racket-repl-display-images (_txt)
  "Replace all image patterns with actual images.
A value for the variable `comint-output-filter-functions'."
  (with-silent-modifications
    (save-excursion
      (goto-char (if (and (markerp comint-last-output-start)
                          (eq (marker-buffer comint-last-output-start)
                              (current-buffer))
                          (marker-position comint-last-output-start))
                     comint-last-output-start
                   (point-min-marker)))
      (forward-line 0) ;in case comint-last-output-start left mid line: #535
      (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
        (while (re-search-forward "\"#<Image: \\(.+?racket-image-.+?\\)>\""
                                  pmark
                                  t)
          (let* ((beg (match-beginning 0))
                 (file (match-string-no-properties 1)))
            (cond ((and racket-images-inline (display-images-p))
                   (replace-match "")
                   (insert-image (apply #'create-image
                                        file
                                        (and (image-type-available-p 'imagemagick)
                                             racket-imagemagick-props
                                             'imagemagick)
                                        nil  ;file not data
                                        (append
                                         '(:scale 1.0) ;#529
                                         (and (image-type-available-p 'imagemagick)
                                              racket-imagemagick-props)))))
                  (t
                   (replace-match (format "[file://%s]" file))))
            (set-marker pmark (max pmark (point)))
            (add-text-properties beg (point)
                                 `(keymap ,racket-image-map
                                   racket-image ,file
                                   help-echo "RET or Mouse-2 to view image"))
            (setq racket-image-cache-dir (file-name-directory file))
            (racket-repl--clean-image-cache)))))))

(defun racket-view-image ()
  "View the image at point using `racket-images-system-viewer'."
  (interactive)
  (pcase (get-text-property (point) 'racket-image)
    ((and (pred stringp) file)
     (start-process "Racket image view"
                     nil
                     racket-images-system-viewer
                     file))))

(defun racket-view-last-image (n)
  "Open the last displayed image using `racket-images-system-viewer'.

With a numeric command prefix argument, open the N-th last shown
image."
  (interactive "p")
  (let ((images (reverse (racket-repl--list-image-cache))))
    (if (>= (length images) n)
        (start-process "Racket image view"
                       nil
                       racket-images-system-viewer
                       (nth (- n 1) images))
      (error "There aren't %d recent images" n))))

;;; Completion

(defvar racket--repl-namespace-symbols nil)

(defun racket--repl-refresh-namespace-symbols ()
  (racket--cmd/async
   (racket--repl-session-id)
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
  (racket--do-describe 'namespace (racket--repl-session-id) str))

(defun racket--repl-company-location (str)
  (pcase (racket--cmd/await (racket--repl-session-id)
                            `(def-in-namespace ,str))
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
  (racket--do-eldoc 'namespace (racket--repl-session-id)))

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
TAB, and activate using RET -- for `racket-repl-visit-definition'
and `racket-repl-documentation'."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Describe: "
                                            racket--repl-namespace-symbols)
    ((and (pred stringp) str)
     (let ((repl-session-id (racket--repl-session-id)))
       (racket--do-describe
        'namespace
        repl-session-id
        str
        t
        (pcase (xref-backend-definitions 'racket-repl-xref str)
          (`(,xref) (lambda () (racket--pop-to-xref-location xref))))
        (lambda () (racket--doc-command repl-session-id 'namespace str)))))))

;;; racket-xref-repl

(defun racket-repl-xref-backend-function ()
  'racket-repl-xref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql racket-repl-xref)))
  (or (racket--module-path-name-at-point)
      (thing-at-point 'symbol)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql racket-repl-xref)))
  (completion-table-dynamic
   (lambda (prefix)
     (all-completions prefix racket--repl-namespace-symbols))))

(cl-defmethod xref-backend-definitions ((_backend (eql racket-repl-xref)) str)
  (or
   (pcase (get-text-property 0 'racket-module-path str)
     (`absolute
      (pcase (racket--cmd/await nil `(mod ,(substring-no-properties str)))
        (`(,path ,line ,col)
         (list (xref-make str (xref-make-file-location path line col))))))
     (`relative
      (let ((path (expand-file-name (substring-no-properties str 1 -1))))
        (list (xref-make str (xref-make-file-location path 1 0))))))
   (pcase (racket--cmd/await racket--repl-session-id `(def namespace ,str))
     (`(,path ,line ,col)
      (list (xref-make str (xref-make-file-location path line col))))
     (`kernel
      (list (xref-make str (xref-make-bogus-location
                            "Defined in #%%kernel -- source not available")))))))

(cl-defmethod xref-backend-references ((backend (eql racket-repl-xref)) str)
  ;; See comments for `racket-xp-mode' implementiation.
  (cl-call-next-method backend (substring-no-properties str)))

;;; Doc

(defun racket-repl-documentation (&optional prefix)
  "View documentation in an external web browser.

The command varies based on how many \\[universal-argument] command prefixes you supply.

1. None.

   Uses the symbol at point. Tries to find documentation for an
   identifer defined in the current namespace.

   If no such identifer exists, opens the Search Manuals page. In
   this case, the variable `racket-documentation-search-location'
   determines whether the search is done locally as with `raco
   doc`, or visits a URL.

2. \\[universal-argument]

   Prompts you to enter a symbol, defaulting to the symbol at
   point if any.

   Otherwise behaves like 1.

3. \\[universal-argument] \\[universal-argument]

   Prompts you to enter anything, defaulting to the symbol at
   point if any.

   Proceeds directly to the Search Manuals page. Use this if you
   would like to see documentation for all identifiers named
   \"define\", for example."
  (interactive "P")
  (racket--doc prefix 'namespace racket--repl-namespace-symbols))

;;; compilation-mode

(defconst racket--compilation-error-regexp-alist
  (list
   ;; Any apparent file:line[:.]col
   (list (rx (group-n 1 (+? (not (syntax whitespace))))
             ?\:
             (group-n 2 (+ digit))
             (any ?\: ?\.)
             (group-n 3 (+ digit)))
         #'racket--adjust-group-1 2 3)
   ;; Any path struct
   (list (rx "#<path:" (group-n 1 (+? (not (any ?\>)))) ?\>)
         #'racket--adjust-group-1 nil nil 0)
   ;; Any (srcloc path line column ...) struct
   (list (rx "(" "srcloc" (+ space)
             ;; path
             "\"" (group-n 1 (+? any)) "\""
             ;; line
             (+ space) (group-n 2 (+ digit))
             ;; column
             (+ space) (group-n 3 (+ digit)))
         #'racket--adjust-group-1 2 3 0 1)
   ;; Any htdp check-expect failure message
   (list (rx "In "
             (group-n 1 (+? (not (syntax whitespace))))
             " at line "
             (group-n 2 (+ digit))
             " column "
             (group-n 3 (+ digit)))
         #'racket--adjust-group-1 2 3))
  "Our value for the variable `compilation-error-regexp-alist'.")

(defun racket--adjust-group-1 ()
  (list (funcall racket-path-from-racket-to-emacs-function (match-string 1))))

;;; racket-repl-mode definition per se

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
     ("C-c C-z"         racket-repl-switch-to-edit)
     ("C-c C-l"         racket-logger)
     ("C-c C-c"         racket-repl-break)
     ("C-c C-\\"        racket-repl-exit)
     ((")" "]" "}")     racket-insert-closing)))
  "Keymap for Racket REPL mode.")

(easy-menu-define racket-repl-mode-menu racket-repl-mode-map
  "Menu for Racket REPL mode."
  '("Racket-REPL"
    ["Break" racket-repl-break]
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
    ["Visit Definition" xref-find-definitions]
    ["Return from Visit" xref-pop-marker-stack]
    ["Find References" xref-find-references]
    "---"
    ["Racket Documentation" racket-doc]
    ["Describe" racket-describe]
    "---"
    ["Switch to Edit Buffer" racket-repl-switch-to-edit]))

(define-derived-mode racket-repl-mode comint-mode "Racket-REPL"
  "Major mode for Racket REPL.

You may use `xref-find-definitions' \\[xref-find-definitions] and
`xref-pop-marker-stack' \\[xref-pop-marker-stack]:
`racket-repl-mode' adds a backend to the variable
`xref-backend-functions'. This backend uses information about
identifier bindings and modules from the REPL's namespace.

\\{racket-repl-mode-map}"
  (racket--common-variables)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-scroll-show-maximum-output nil) ;t slow for big outputs
  (setq-local mode-line-process nil)
  (setq-local completion-at-point-functions (list #'racket-repl-complete-at-point))
  (setq-local eldoc-documentation-function nil)
  (define-key racket-repl-mode-map [menu-bar signals] 'undefined)
  (add-hook 'comint-output-filter-functions #'racket-repl-display-images nil t)
  (compilation-setup t)
  (setq-local compilation-error-regexp-alist racket--compilation-error-regexp-alist)
  ;; Persistent history
  (setq-local comint-input-autoexpand nil) ;#450
  (setq-local comint-input-filter #'racket-repl--input-filter)
  (make-directory racket-repl-history-directory t)
  (setq-local comint-input-ring-file-name
              (expand-file-name (racket--buffer-name-slug)
                                racket-repl-history-directory))
  (comint-read-input-ring t)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)
  (add-hook 'kill-emacs-hook #'racket--repl-save-all-histories nil t)
  (add-hook 'xref-backend-functions #'racket-repl-xref-backend-function nil t)
  (add-to-list 'semantic-symref-filepattern-alist
               '(racket-repl-mode "*.rkt" "*.rktd" "*.rktl")))

(defun racket--repl-save-all-histories ()
  "Call comint-write-input-ring for all `racket-repl-mode' buffers.
A suitable value for the hook `kill-emacs-hook'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'racket-repl-mode)
        (comint-write-input-ring)))))

(defun racket--buffer-name-slug ()
  "Change `buffer-name' to a string that is a valid filename."
  ;; 3. Finally use `shell-quote-argument' to try to catch anything
  ;; else.
  (shell-quote-argument
   ;; 2. But not leading or trailing ?-
   (replace-regexp-in-string
    (rx (or (seq bos (+ ?-))
            (seq (+ ?-) eos)))
    ""
    ;; 1. Replace runs of anything that is not alnum with a single ?-.
    (replace-regexp-in-string
     (rx (+ (not (any alnum))))
     "-"
     (buffer-name)))))

(defun racket-repl-clear ()
  "Delete all text in the REPL.

A suitable value for the hook `racket-before-run-hook' if you
want the REPL buffer to be cleared before each run, much like
with Dr Racket. To do so you can use `customize', or, add to your
Emacs init file something like:

  (add-hook 'racket-before-run-hook #'racket-repl-clear)

See also the command `racket-repl-clear-leaving-last-prompt'."
  ;; This prevents a first blank line, by telling the back end that
  ;; output is no longer sitting at some non-zero column after a
  ;; prompt; therefore fresh-line won't need to issue a newline.
  (racket--cmd/async (racket--repl-session-id) `(repl-zero-column))
  (racket--do-repl-clear nil))

(defun racket-repl-clear-leaving-last-prompt ()
  "Delete all text in the REPL, except for the last prompt."
  (interactive)
  (racket--do-repl-clear t))

(defun racket--do-repl-clear (leave-last-prompt-p)
  (cl-case major-mode
    (racket-repl-mode
     (racket--delete-all-buffer-text leave-last-prompt-p))
    (racket-mode
     (when (get-buffer racket-repl-buffer-name)
       (with-current-buffer racket-repl-buffer-name
         (racket--delete-all-buffer-text leave-last-prompt-p))))
    (otherwise
     (user-error "Current buffer is not a Racket Mode edit or REPL buffer"))))

(defun racket--delete-all-buffer-text (leave-last-prompt-p)
  (with-silent-modifications
    (widen)
    (let ((end (if leave-last-prompt-p
                   (save-excursion
                     (goto-char (point-max))
                     (comint-previous-prompt 1)
                     (comint-next-prompt 1)
                     (forward-line 0)   ;BOL ignoring fields
                     (point))
                 (point-max)))
          (inhibit-read-only t))
      (delete-region (point-min) end)
      (goto-char (point-max))
      (dolist (win (get-buffer-window-list))
        (set-window-point win (point-max))))))

(provide 'racket-repl)

;; racket-repl.el ends here

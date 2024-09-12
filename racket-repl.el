;;; racket-repl.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.
;; Image portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-browse-url)
(require 'racket-scribble-anchor)
(require 'racket-complete)
(require 'racket-describe)
(require 'racket-doc)
(require 'racket-eldoc)
(require 'racket-custom)
(require 'racket-common)
(require 'racket-show)
(require 'racket-util)
(require 'racket-visit)
(require 'racket-cmd)
(require 'racket-back-end)
(require 'ansi-color)
(require 'compile)
(require 'easymenu)
(require 'cl-lib)
(require 'cl-macs)
(require 'rx)
(require 'xref)
(require 'semantic/symref/grep)
(require 'ring)

(declare-function  racket--what-to-run-p "racket-common" (v))

;; Don't (require 'racket-debug). Mutual dependency. Instead:
(declare-function  racket--debuggable-files      "racket-debug" (file-to-run))
(autoload         'racket--debuggable-files      "racket-debug")

;;; edit buffers <=> `racket-repl-mode' buffers

;; There are some nuances here regarding these variables being
;; buffer-local or not, and, whether the variables have any meaning in
;; certain modes, or not. We use Emacs variable semantics to handle
;; the association between `racket-mode' or `racket-hash-lang-mode'
;; edit buffers and `racket-repl-mode' buffers, for a variety of use
;; cases the user might prefer. These range from all edit buffers
;; sharing one REPL buffer (the traditional default for Racket Mode),
;; up to each edit buffers having its own REPL (as in Dr Racket), or
;; anything in between (such as one REPL per projectile project, or
;; whatever).
;;
;; Although some of these scenarios might benefit from a higher-level
;; UI, they all come down to setting the variable
;; `racket-repl-buffer-name' globally and/or locally for each edit
;; buffer -- that is the fundamental representation.
;;
;; Similarly, each `racket-repl-mode' buffer has an
;; always-buffer-local value for the variable
;; `racket--repl-session-id'. (Note that `racket-repl-buffer-name'
;; only has meaning for `racket-mode' buffers, and
;; `racket--repl-session-id' only has meaning for `racket-repl-mode'
;; buffers. Emacs variables exist for all buffers using all major
;; modes. All we can do is remember in which buffers they mean
;; something as opposed to being ignored.)

(defvar racket-repl-buffer-name nil
  "The name of the `racket-repl-mode' buffer associated with `racket-mode' buffer.

Important: This variable only means something in each
`racket-mode' or `racket-hash-lang-mode' edit buffer. It has no
meaning in `racket-repl-mode' or other buffers.

When nil, all `racket-mode' edit buffers share the same REPL.
However, a buffer may `setq-local' this to some other value. See
the defcustom `racket-repl-buffer-name-function' as well as several
values for it in racket-repl-buffer-name.el.")

(defun racket--call-with-repl-buffer (thunk)
  (pcase (if (eq major-mode 'racket-repl-mode)
             (buffer-name)
           racket-repl-buffer-name)
    ((and (pred stringp) name)
     (pcase (get-buffer name)
       ((and (pred bufferp) (pred buffer-live-p) buf)
        (with-current-buffer buf (funcall thunk)))))))

(defmacro with-racket-repl-buffer (&rest body)
  "Execute forms in BODY with `racket-repl-mode' temporarily current buffer."
  (declare (indent 0) (debug t))
  `(racket--call-with-repl-buffer (lambda () ,@body)))

;;; REPL back end sessions <=> `racket-repl-mode' buffers

(defvar racket--repl-next-session-id 0)

(defvar-local racket--repl-session-id nil
  "An ID for each back end REPL session.

Commands that are about a specific REPL session must supply this;
see `racket--cmd/async'.

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
    (when (stringp racket-repl-buffer-name)
      (let ((buffer (get-buffer racket-repl-buffer-name)))
        (when buffer
          (with-current-buffer racket-repl-buffer-name
            racket--repl-session-id))))))

(defun racket--call-with-repl-session-id (id proc &rest args)
  "Find `racket-repl-mode' buffer with `racket--repl-session-id'
`eq' to ID. Apply ARGS to PROC while that is current buffer."
  ;; If searching buffer-list too slow, we could maintain a hash table
  ;; and clean it with a kill-buffer hook.
  (seq-some (lambda (buf)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (when (and (eq major-mode 'racket-repl-mode)
                             (eq racket--repl-session-id id))
                    (apply proc args)
                    t))))
            (buffer-list)))

(defun racket--repl-on-stop-back-end ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (eq major-mode 'racket-repl-mode)
                 (buffer-live-p buf))
        (racket--repl-insert-output 'exit "REPL session stopped")))))
(add-hook 'racket-stop-back-end-hook #'racket--repl-on-stop-back-end)

;;; Markers for run, interactions prompt, and program I/O

(defvar-local racket--repl-run-mark nil
  "The point at which a run command was issued.")

;; Note: One goal here is to make read-only all of the output, as well
;; as "old" input that has already been submitted. This involves
;; paying careful attention to the read-only and rear-nonsticky
;; properties.

(defvar-local racket--repl-prompt-mark nil
  "A marker for the start of the active prompt, if any.

Non-nil only when the REPL is in a prompt-read.

Marker insertion type is non-nil: text inserted there
automatically advances the marker position.

The prompt itself is read-only. `racket--repl-prompt-mark-end'
gives the position where the following read/write portion
starts.")

(defvar-local racket--repl-output-mark nil
  "A marker where REPL output should be inserted, and user may input.

Plays a role similar to `process-mark' in `comint-mode', except
we have no process.

Various kinds of output get various field property values. All
output is read-only, but we arrange for the last character to be
rear-nonsticky so self-insert-command will let the user type
input. When the user types text there and presses RET, then that
is submitted as plain input -- as opposed to REPL interaction
input.

When `racket--repl-prompt-mark' marker exists, that always
/follows/ `racket--repl-output-mark'. If e.g. the user program
has a thread that continues to run after we're back at a prompt,
its output is displayed /before/ the prompt. Otherwise with no
live prompt this marker will be at `point-max'.")

(defun racket--repl-make-prompt-mark (prompt-str)
  (when racket--repl-prompt-mark
    (racket--repl-delete-prompt-mark t))
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp)
      (insert ?\n))
    (let ((start (point)))
      (insert (propertize (concat prompt-str " ")
                          'read-only t
                          'font-lock-face racket-repl-prompt
                          'field 'prompt
                          'racket-prompt t))
      (add-text-properties (1- (point)) (point)
                           (list 'rear-nonsticky t))
      (setq racket--repl-prompt-mark (make-marker))
      (set-marker racket--repl-prompt-mark start)
      ;; Marker /does/ advance when text inserted there.
      (set-marker-insertion-type racket--repl-prompt-mark t)
      ;; Ensure output marker position <= prompt marker position;
      ;; output always goes /before/ the last active prompt, if any.
      (set-marker racket--repl-output-mark
                  (min (marker-position racket--repl-output-mark)
                       (marker-position racket--repl-prompt-mark))))))

(defun racket--repl-delete-prompt-mark (abandon-p)
  (when racket--repl-prompt-mark
    (let ((inhibit-read-only t))
      (if abandon-p
          (delete-region racket--repl-prompt-mark (point-max))
        (add-text-properties (racket--repl-prompt-mark-end)
                             (point-max)
                             (list 'read-only t
                                   'field 'input)))
      (goto-char (point-max))
      (set-marker racket--repl-prompt-mark nil)
      (setq racket--repl-prompt-mark nil)
      (set-marker racket--repl-output-mark (point-max)))))

(defun racket--repl-prompt-mark-end ()
  "May return nil when there is no live prompt."
  (when racket--repl-prompt-mark
    (or (next-single-property-change racket--repl-prompt-mark 'racket-prompt)
        (point-max))))

;;; Output

(defun racket--repl-on-output (session-id kind value)
  ;;;(message "%S" (list 'racket--repl-on-output session-id kind value))
  (racket--call-with-repl-session-id session-id
                                     #'racket--repl-insert-output
                                     kind value))

(defun racket--repl-insert-output (kind value)
  (let ((moving (= (point) racket--repl-output-mark))
        (inhibit-read-only t))
    (save-excursion
      (goto-char racket--repl-output-mark)
      (let ((pt (point)))
        ;; Previous chunks of output may have ended with a
        ;; rear-nonsticky property to allow input to follow. Now that
        ;; we're adding more output, remove that property so there are
        ;; no read/write "seams" between chunks.
        (remove-text-properties (point-min) (point) '(rear-nonsticky nil))
        (cl-flet*
            ((fresh-line () (unless (bolp) (newline)))
             (faced (str face) (propertize str 'font-lock-face face))
             (insert-faced (str face) (insert (faced str face)))
             (insert-filtered (str face) (insert (racket--repl-filter-output
                                                  (faced str face)))))
          (cl-case kind
            ((run)
             (racket--repl-delete-prompt-mark 'abandon)
             (unless (equal value "")
               (fresh-line)
               (insert-faced (format "————— run %s —————\n" value) 'racket-repl-message)))
            ((prompt)
             (racket--repl-make-prompt-mark value))
            ((message)
             (fresh-line)
             (insert-faced value 'racket-repl-message)
             (unless (bolp) (newline)))
            ((exit)
             (racket--repl-delete-prompt-mark 'abandon)
             (fresh-line)
             (insert-faced value 'racket-repl-message)
             (unless (bolp) (newline))
             (setq moving t) ;leave point after, for tests
             (setq racket--repl-session-id nil))
            ((value)
             (insert-faced value 'racket-repl-value))
            ((value-special)
             (pcase-let ((`(image . ,file) value))
               (racket--repl-insert-image file)))
            ((error)
             (pcase value
               (`(,msg ,srclocs (,context-kind . ,context-names-and-locs))
                (fresh-line)
                (insert-faced msg 'racket-repl-error-message)
                (newline)
                ;; Heuristic: When something supplies exn-srclocs,
                ;; show those only. Otherwise show context if any.
                ;; This seems to work well for most runtime
                ;; exceptions, as well as for rackunit test failures
                ;; (where the srcloc suffices and the context esp
                ;; w/errortrace is useless noise).
                (cond (srclocs
                       (dolist (loc srclocs)
                         (insert " ")
                         (insert (racket--format-error-location loc))
                         (newline)))
                      (context-names-and-locs
                       (insert-faced (format "Context (%s):" context-kind)
                                     'racket-repl-error-message)
                       (newline)
                       (dolist (v context-names-and-locs)
                         (pcase-let ((`(,name . ,loc) v))
                           (insert " ")
                           (insert (racket--format-error-location loc))
                           (insert " ")
                           (when name
                             (insert-faced name 'racket-repl-error-label)))
                         (newline)))))))
            ((stdout)
             (insert-filtered value 'racket-repl-stdout))
            ((stderr)
             (insert-filtered value 'racket-repl-stderr))
            (otherwise
             (fresh-line)
             (insert-faced value 'racket-repl-message))))
        (unless (eq kind 'prompt)
          (add-text-properties pt (point)
                               (list
                                'read-only t
                                'field kind))
          ;; Make last character rear-nonsticky. Among other things,
          ;; means `racket--repl-output-mark' won't be read-only; and
          ;; user may input there (for user program reading from
          ;; current-input-port).
          (add-text-properties (max (point-min) (1- (point))) (point)
                               (list 'rear-nonsticky t))
          (set-marker racket--repl-output-mark (point))
          ;; When stdout/stderr output ends with prompt following on
          ;; same line, push the prompt down to its own line.
          (when (and (memq kind '(stdout stderr))
                     racket--repl-prompt-mark
                     (equal (point) (marker-position racket--repl-prompt-mark)))
            (insert (propertize "\n"
                                'read-only t
                                'field kind))))))
    ;; If we just inserted a new prompt, position after it.
    (let ((win (get-buffer-window (current-buffer))))
      (if (eq kind 'prompt)
          (let ((pos (racket--repl-prompt-mark-end)))
            (goto-char pos)
            (when win (set-window-point win pos)))
        ;; When point was exactly at the old output marker value, move
        ;; point to follow it. (Otherwise user is navigating through
        ;; buffer, leave them alone.)
        (when moving
          (goto-char racket--repl-output-mark)
          (when win (set-window-point win racket--repl-output-mark)))))))

(defvar racket-repl-output-filter-functions (list #'ansi-color-apply)
  "List of functions to call before inserting stdout/stderr output.

Similar to `comint-preoutput-filter-functions', but limited to
stdout/stderr kinds of output.

Each function gets one argument, a string propertized by default
with a face for stdout or stderr. It should return a string to
insert instead. The functions are composed.

You can use `add-hook' to add functions to this list either
globally or locally.

If the function uses state that should be reset between runs, do
that via `racket-before-run-hook'; for example see
`racket-ansi-color-context-reset'.")

;; Because we default `racket-repl-output-filter-functions' to
;; `ansi-color-apply', we want to reset its state for a REPL before
;; every run. Although we could hard-code that, use the before-run
;; hook to set an example for users.
(defun racket-ansi-color-context-reset ()
  (with-racket-repl-buffer
    (setq-local ansi-color-context nil)))
(add-hook 'racket-before-run-hook #'racket-ansi-color-context-reset)

(defun racket--repl-filter-output (string)
  ;; Beause there is no run-hooks-xxx variant equivalent to function
  ;; composition, we borrow the equivalent code from comint, which
  ;; also handles the wrinkle of buffer-local values.
  (let ((functions racket-repl-output-filter-functions))
    (while (and functions string)
      (if (eq (car functions) t)
          (let ((functions
                 (default-value 'racket-repl-output-filter-functions)))
            (while (and functions string)
              (setq string (funcall (car functions) string))
              (setq functions (cdr functions))))
        (setq string (funcall (car functions) string)))
      (setq functions (cdr functions))))
  string)

(defun racket--repl-call-with-value-and-input-ranges (from upto proc)
  "Call PROC with sub-ranges of FROM..UPTO, saying whether each
is a value or input since `racket--repl-run-mark'."
  (setq upto (min upto (point-max)))
  ;; Everything before the last run is "stale": No.
  (when (< from racket--repl-run-mark)
    (funcall proc from racket--repl-run-mark nil)
    (setq from racket--repl-run-mark))
  (let ((prompt-end (or (racket--repl-prompt-mark-end) (point-max))))
    (while (< from upto)
      (cond
       ;; If we're at/after the end of the last, live prompt, then
       ;; everything remaining is input, yes, and we're done.
       ((<= prompt-end from)
        (funcall proc from upto t)
        (setq from upto))
       ;; Keep getting chunks at racket-output prop change boundaries,
       ;; until we reach the earlier of prompt-end or point-max.
       (t
        (let ((in (memq (get-text-property from 'field) '(value input)))
              (pos (min (or (next-single-property-change from 'field)
                            (point-max))
                        prompt-end)))
          (funcall proc from (min pos upto) in)
          (setq from pos)))))))

;;; Submit

(defalias 'racket-repl-eval-or-newline-and-indent #'racket-repl-submit)

(defvar-local racket-repl-submit-function nil)

(defun racket-repl-submit ()
  "Submit interaction or input.

When at a REPL prompt, submit as an interaction expression.
Otherwise send to current-input-port of user program."
  (interactive)
  (unless (racket--repl-session-id)
    (user-error "no REPL session"))
  (let ((prompt-end (racket--repl-prompt-mark-end)))
    (if (and prompt-end (< prompt-end (point-max)))
        (let* ((input (buffer-substring-no-properties prompt-end (point-max)))
               (input+ret (concat input "\n")))
          (when (if racket-repl-submit-function
                    (funcall racket-repl-submit-function input+ret)
                  (racket--repl-complete-sexp-p))
            (racket--repl-add-to-input-history input)
            (goto-char (point-max))
            (insert ?\n)
            (add-text-properties prompt-end (point-max)
                                 (list 'read-only t
                                       'rear-nonsticky t))
            (racket--repl-delete-prompt-mark nil)
            (racket--cmd/async (racket--repl-session-id) `(repl-submit ,input+ret))))
      (end-of-line)
      (when (< racket--repl-output-mark (point))
        (let ((input (buffer-substring-no-properties racket--repl-output-mark (point))))
          ;; Intentionally do NOT `racket--repl-add-to-input-history'.
          (insert ?\n)
          (add-text-properties racket--repl-output-mark (point)
                               (list 'read-only t
                                     'rear-nonsticky t))
          (set-marker racket--repl-output-mark (point))
          (racket--cmd/async (racket--repl-session-id)
                             `(repl-input ,(concat input "\n"))))))))

(defun racket--repl-complete-sexp-p ()
  "Is there at least one complete sexp at REPL prompt?"
  (condition-case _
      (let* ((beg (racket--repl-prompt-mark-end))
             (end (save-excursion
                    (goto-char beg)
                    (while (< (point) (point-max))
                      ;; This will scan-error unless complete sexp, or
                      ;; all whitespace.
                      (forward-list 1))
                    (point))))
        (not (or (equal beg end)        ;nothing
                 (string-match-p        ;something but all whitespace
                  (rx bos
                      (1+ (or (syntax whitespace)
                              (syntax comment-start)
                              (syntax comment-end)))
                      eos)
                  (buffer-substring beg end)))))
    (scan-error nil)))

(defun racket-repl-break ()
  "Send an interrupt break to the REPL."
  (interactive)
  (unless (racket--cmd-open-p) ;don't auto-start the back end
    (user-error "Back end is not running"))
  (racket--cmd/async (racket--repl-session-id) `(repl-break)))

(defun racket-repl-exit ()
  "Exit the REPL session.

Equivalent to entering \"(exit)\" at the REPL prompt, but works
even when the module language doesn't provide any binding for
\"exit\"."
  (interactive)
  ;; Avoid sending a command about exiting a REPL session that can't
  ;; exist because the back end isn't running. That's worse than a
  ;; no-op; that would auto-start the back end for no good reason now.
  (when (racket--cmd-open-p)
    (when (racket--repl-session-id)
      ;; Note: We don't `(setq racket--repl-session-id nil)` here
      ;; because (1) the repl buffer isn't necessarily current and
      ;; anyway (2) we want to allow our output handler function to
      ;; get the "exit" message from the back end; it will set nil,
      ;; then.
      (racket--cmd/async (racket--repl-session-id) `(repl-exit)))))

(declare-function racket-repl-buffer-name-unique "racket-repl-buffer-name" ())
(autoload        'racket-repl-buffer-name-unique "racket-repl-buffer-name")

(declare-function racket-mode "racket-mode" ())
(autoload        'racket-mode "racket-mode")

;;;###autoload
(defun racket-repl (&optional noselect)
  "Show a Racket REPL buffer in some window.

The intended use of Racket Mode's REPL is that you `find-file'
some specific file, then run it using a command like `racket-run'
or `racket-run-module-at-point'. The resulting REPL will
correspond to those definitions and match your expectations.

Therefore this `racket-repl' command -- which is intended as a
convenience for people who want to \"just get a quick scratch
REPL\" -- is actually implemented as running the file named in
the customization variable `racket-repl-command-file'. When that
file doesn't exist, it is created to contain just \"#lang
racket/base\". You may edit the file to use a different lang,
require other modules, or whatever."
  (interactive "P")
  ;; Create file if it doesn't exist
  (unless (file-exists-p racket-repl-command-file)
    (let ((dir (file-name-directory racket-repl-command-file)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (write-region ";; Used by M-x racket-repl; you may edit\n#lang racket/base\n"
                  nil racket-repl-command-file))
  ;; Visit the file without selecting it, and run it.
  (let ((racket-repl-buffer-name-function #'racket-repl-buffer-name-unique))
    (with-current-buffer (find-file-noselect racket-repl-command-file)
      (unless (racket--edit-mode-p)
        (racket-mode)) ;ensure: see #713
      (racket--repl-run
       (list racket-repl-command-file)
       nil
       nil
       (lambda ()
         (display-buffer racket-repl-buffer-name)
         (unless noselect
           (select-window (get-buffer-window racket-repl-buffer-name t))))))))

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

The command varies based on how many \\[universal-argument]
prefix arguments you supply.
\\<racket-mode-map>

- \\[racket-run-and-switch-to-repl]

  Follows the `racket-error-context' setting.

- \\[universal-argument] \\[racket-run-and-switch-to-repl]

  Uses errortrace for improved stack traces, as if
  `racket-error-context' were set to \"high\".

  This lets you keep `racket-error-context' set to a faster
  value like \"low\" or \"medium\", then conveniently re-run
  when you need a better strack trace.

- \\[universal-argument] \\[universal-argument] \\[racket-run-and-switch-to-repl]

  Instruments code for step debugging. See `racket-debug-mode'
  and the variable `racket-debuggable-files'.

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

To visit error locations, move point there and press RET or mouse
click. Or, use the standard `next-error' and `previous-error'
commands from either the edit or REPL buffer."
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

\\<racket-mode-map>
This is equivalent to \\[universal-argument] \\[racket-run].

Defined as a function so it can be a menu target."
  (interactive)
  (racket-run '(4)))

(defun racket-run-with-debugging ()
  "Run with `racket-error-context' temporarily set to \"debug\".

\\<racket-mode-map>
This is equivalent to \\[universal-argument] \\[universal-argument] \\[racket-run].

Defined as a function so it can be a menu target."
  (interactive)
  (racket-run '(16)))

(defun racket-run-and-switch-to-repl (&optional prefix)
  "This is `racket-run' followed by selecting the REPL buffer window.

This is similar to how Dr Racket behaves.

\\<racket-mode-map>
To make it even more similar, you may add `racket-repl-clear' to
the variable `racket-before-run-hook'."
  (interactive "P")
  (racket--repl-run (list (racket--buffer-file-name))
                    racket-submodules-to-run
                    (pcase prefix
                      (`(4)  'high)
                      (`(16) 'debug)
                      (_     racket-error-context))
                    #'racket-edit-switch-to-repl))

(defun racket-test (&optional prefix)
  "Run the \"test\" submodule.

Put your tests in a \"test\" submodule. For example:

#+BEGIN_SRC racket
    (module+ test
      (require rackunit)
      (check-true #t))
#+END_SRC

Any rackunit test failure messages show the location. You may use
`next-error' to jump to the location of each failing test.

With \\[universal-argument] uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting.

With \\[universal-argument] \\[universal-argument] also runs the
tests with coverage instrumentation and highlights uncovered code
using `font-lock-warning-face'.

See also:
- `racket-fold-all-tests'
- `racket-unfold-all-tests'
"
  (interactive "P")
  (let ((mod-path (list (racket--buffer-file-name) 'test))
        (buf (current-buffer)))
    ;; Originally this function's single optional argument was a
    ;; `coverage-p` boolean. For backward compatibility in case anyone
    ;; has Emacs Lisp calling this function non-interactively, we keep
    ;; supporting t and nil values.
    (pcase prefix
      (`()  (racket--repl-run mod-path))
      (`(4) (racket--repl-run mod-path nil 'high))
      ((or '(16) 't)
       (message "Running test submodule with coverage instrumentation...")
       (racket--repl-run
        mod-path
        nil
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
                    (goto-char beg0)))))))))))))

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
    \\='(\"-f\" \"bar\")
    (list \"-f\" \"bar\")
#+END_SRC
")

(defvar racket--repl-before-run-hook nil
  "Thunks to do before each `racket--repl-run'.

Here \"before\" means that the `racket-repl-mode' buffer might not
exist yet.

This hook is for internal use by Racket Mode. An equivalent hook
for end user customization is `racket-before-run-hook'.")

(defvar racket--repl-after-run-hook nil
  "Thunks to do after each `racket--repl-run'.

This hook is for internal use by Racket Mode. An equivalent hook
for end user customization is `racket-after-run-hook'.

Here \"after\" means that the run has completed and e.g. the REPL
is waiting at another prompt.")

;; Don't (require 'racket-hash-lang). Mutual dependency. Instead:
(declare-function racket--configure-repl-buffer-from-edit-buffer "racket-hash-lang" (edit-buf repl-buf))
(autoload        'racket--configure-repl-buffer-from-edit-buffer "racket-hash-lang")

(defun racket--repl-run (&optional what extra-submods context-level callback)
  "Do an initial or subsequent run.

WHAT must be `racket--what-to-run-p', where nil defaults to
`racket--what-to-run'.

EXTRA-SUBMODS should be a list of symbols, names of extra
submodules to run, e.g. (test main). This is intended for use by
`racket-run', which more closely emulates DrRacket, as opposed to
`racket-run-module-at-point'.

CONTEXT-LEVEL should be a valid value for the variable
`racket-error-context', \"coverage\", or \"profile\". Or if nil,
defaults to the variable `racket-error-context'.

CALLBACK is used as the callback for `racket--cmd/async'; it may
be nil which is equivalent to #\\='ignore."
  (racket--assert-edit-mode)
  ;; Support running buffers created by `org-edit-src-code': see
  ;; issues #626, #630.
  (when (bound-and-true-p org-src-mode)
    (unless buffer-file-name
      ;; Give the buffer a temp file we can run. The correct thing to
      ;; use is `set-visited-file-name', which handles many things
      ;; besides setting `buffer-file-name'. Some we want, e.g.
      ;; setting the buffer-modified flag. Some we don't, e.g.
      ;; renaming the buffer, which we rename back to the original
      ;; because org-src does things with regexps on these buffer
      ;; names.
      (let ((orig-buffer-name (buffer-name)))
        (set-visited-file-name (make-temp-file "racket-org-edit-" nil ".rkt"))
        (rename-buffer orig-buffer-name))
      (setq what (list (racket--buffer-file-name)))
      ;; org-src adds to `write-contents-functions' a hook that
      ;; prevents `save-buffer' actually writing to file; instead it
      ;; copies contents back to the main org buffer. Accommodate that
      ;; by prepending our own hook, which actually writes to file. It
      ;; returns nil to mean other hooks should still be run, so this
      ;; doesn't interfere with org's hook.
      (add-hook 'write-contents-functions #'racket--write-contents nil t)))
  ;; Save buffer and validate WHAT to run.
  (unless (progn (racket--save-if-changed)
                 (racket--what-to-run-p what))
    (signal 'wrong-type-argument `(racket--what-to-run-p ,what)))
  ;; Handle the restart-watch-directories feature; #602
  (when-let (changes (racket--back-end-watch-read/reset))
    (when (y-or-n-p (format "Changed: %S -- restart Racket Mode back end %S? "
                            changes
                            (racket-back-end-name)))
      (message "")
      (racket-start-back-end)))

  (pcase-let*
      ((context-level (or context-level racket-error-context))
       (what (or what (racket--what-to-run)))
       (`(,what ,debug-files)
        (pcase what
          (`(,file . ,subs)
           (list (cons (racket-file-name-front-to-back file) subs)
                 (when (eq context-level 'debug)
                   (racket--debuggable-files file))))
          (`()
           (list nil nil))))
       (cmd (list 'run
                  what
                  extra-submods
                  racket-memory-limit
                  racket-pretty-print
                  (window-width)
                  (racket--char-pixel-width)
                  context-level
                  racket-user-command-line-arguments
                  debug-files))
       (edit-buffer (current-buffer))
       (after (lambda (_ignore)
                (with-current-buffer edit-buffer
                  (run-hooks 'racket--repl-after-run-hook
                             'racket-after-run-hook)
                  (when callback
                    (funcall callback))))))
    (racket--repl-ensure-buffer-and-session
     edit-buffer
     (lambda (_repl-buffer)
       (with-current-buffer edit-buffer
         (run-hooks 'racket--repl-before-run-hook
                    'racket-before-run-hook))
       (racket--cmd/async (racket--repl-session-id) cmd after)))))

(defun racket--write-contents ()
  (write-region nil nil buffer-file-name)
  nil)

(defun racket--char-pixel-width ()
  (with-temp-buffer
    (insert "M")
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (car (window-text-pixel-size nil (line-beginning-position) (point))))))

(defun racket--repl-ensure-buffer-and-session (edit-buffer continue)
  "Ensure a `racket-repl-mode' buffer exists with a live session.

Create the buffer if necessary, enabling `racket-repl-mode'.

Start the session if necessary.

When EDIT-BUFFER is not nil, use it to call
`racket--configure-repl-buffer-from-edit-buffer' after the repl
buffer is fully initialized (and if the repl session isn't
started, before starting it).

Calls CONTINUE with one argument, the repl buffer.

This displays the buffer but does not change the selected window."
  (let ((repl-buf (or (get-buffer racket-repl-buffer-name)
                      (with-current-buffer (get-buffer-create racket-repl-buffer-name)
                        (racket-repl-mode)
                        (add-hook 'kill-buffer-hook #'racket-repl-exit nil t)
                        (current-buffer)))))
    (display-buffer repl-buf)
    (with-current-buffer repl-buf
      (if racket--repl-session-id
          (progn
            (when edit-buffer
              (racket--configure-repl-buffer-from-edit-buffer edit-buffer repl-buf))
            (funcall continue repl-buf))
        (setq racket--repl-session-id (cl-incf racket--repl-next-session-id))
        (when noninteractive
          (princ (format "{racket--repl-start}: picked next session id %S\n"
                         racket--repl-session-id)))
        (goto-char (point-max))
        (racket--repl-delete-prompt-mark t)
        (setq racket--repl-run-mark (point-marker))
        (setq racket--repl-output-mark (point-marker))
        (set-marker-insertion-type racket--repl-output-mark nil)
        (when edit-buffer
          (racket--configure-repl-buffer-from-edit-buffer edit-buffer repl-buf))
        (unless (racket--cmd-open-p)
          (racket--repl-insert-output 'message "Starting back end..."))
        (racket--cmd/async nil
                           `(repl-start ,racket--repl-session-id)
                           (lambda (_id)
                             (funcall continue repl-buf)))))))

;;; Switch between associcated edit and REPL buffers

(defun racket-edit-switch-to-repl ()
  "Select REPL buffer associated with the edit buffer.

When no such buffer exists yet, do nothing but say so and suggest
using a run command."
  (interactive)
  (racket--assert-edit-mode)
  (pcase (get-buffer racket-repl-buffer-name)
    ((and repl-buf (pred buffer-live-p))
     (display-buffer repl-buf)
     (select-window (get-buffer-window repl-buf t)))
    (_ (user-error
        (format "No REPL buffer exists for %s; use a run command"
                (buffer-name))))))

(defun racket-repl-file-name ()
  "Return the file running in the REPL, or nil.

The result can be nil if the REPL is not started."
  (when (racket--repl-session-id)
    (racket--cmd/await (racket--repl-session-id) `(path))))

(defun racket--in-repl-or-its-file-p ()
  "Is current-buffer `racket-repl-mode' or buffer for file active in it?"
  (or (eq (current-buffer)
          (get-buffer racket-repl-buffer-name))
      (let ((buf-file  (racket--buffer-file-name))
            (repl-file (racket-repl-file-name)))
        (and buf-file repl-file (string-equal buf-file repl-file)))))

(defun racket-repl-switch-to-edit ()
  "Select edit buffer of the file running in the REPL.

If no buffer is visting the file, `find-file' it in `other-window'."
  (interactive)
  (pcase (racket-repl-file-name)
    ((and (pred stringp) path)
     (pcase (find-buffer-visiting path)
       ((and (pred bufferp) buffer) (pop-to-buffer buffer t))
       (_ (other-window 1)
          (find-file path))))
    (_ (pcase (racket--most-recent-edit-buffer)
         ((and (pred bufferp) buffer) (pop-to-buffer buffer t))
         (_ (user-error "There are no racket-mode buffers"))))))

(defun racket--most-recent-edit-buffer ()
  (cl-some (lambda (b)
             (with-current-buffer b
               (and (racket--edit-mode-p) b)))
           (buffer-list)))

;;; send to REPL

(defun racket--send-region-to-repl (start end &optional echo-p)
  "Internal function to send the region to the Racket REPL.

Requires the REPL already to be started, e.g. from a run command.

Before sending the region, calls `racket--repl-forget-errors'.
Also inserts a ?\n at the process mark so that output goes on a
fresh line, not on the same line as the prompt.

Finally, displays the REPL buffer in some window, so the user may
see the results."
  (unless (and start end)
    (error "start and end must not be nil"))
  (unless (racket--repl-session-id)
    (user-error "No REPL session available; run the file first"))
  ;; Capture source buffer in case something changes; see e.g. #407.
  (let ((source-buffer (current-buffer)))
    (racket--repl-forget-errors)
    (with-racket-repl-buffer
      (save-excursion
        (racket--repl-delete-prompt-mark nil)
        (goto-char (point-max))
        (insert ?\n)
        (when echo-p
          (insert (with-current-buffer source-buffer
                    (buffer-substring start end)))
          (insert (propertize "\n=>\n"
                              'font-lock-face 'racket-repl-message)))
        (add-text-properties racket--repl-output-mark (point)
                             (list 'field 'send
                                   'read-only t))
        (set-marker racket--repl-output-mark (point))))
    (racket--cmd/async (racket--repl-session-id)
                       `(repl-submit ,(with-current-buffer source-buffer
                                        (buffer-substring-no-properties start end))))
    (display-buffer racket-repl-buffer-name)))

(defun racket-send-region (start end)
  "Send the current region (if any) to the Racket REPL."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No region"))
  (racket--assert-edit-mode)
  (racket--send-region-to-repl start end))

(defun racket-send-definition ()
  "Send the current definition to the Racket REPL."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (racket--send-region-to-repl (point) end))))

(defun racket-send-last-sexp (&optional prefix)
  "Send the expression before point to the Racket REPL.

The expression may be either an at-expression or an s-expression.

When the expression is a sexp comment, the sexp itself is sent,
without the #; prefix.

\\<racket-mode-map>
With a prefix argument (e.g. \\[universal-argument] \\[racket-send-last-sexp]), the sexp is copied
into the REPL, followed by a \"=>\" line, to distinguish it
from the zero or more values to which it evaluates."
  (interactive "P")
  (racket--assert-sexp-edit-mode)
  (racket--send-region-to-repl (racket--start-of-previous-expression)
                               (point)
                               prefix))

(defun racket-eval-last-sexp ()
  "Eval the expression before point asynchronously.

The eventual results are presented using the variable
`racket-show-functions'.

The expression may be either an at-expression or an s-expression."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (unless (racket--repl-session-id)
    (user-error "No REPL session available; run the file first"))
  (let ((beg (racket--start-of-previous-expression))
        (end (point)))
   (racket--cmd/async
    (racket--repl-session-id)
    `(eval ,(buffer-substring-no-properties beg end))
    (lambda (v)
      (racket-show (format "%s" v) end t)))))

(defun racket--start-of-previous-expression ()
  "Handles both s-expressions and at-expressions."
  (save-excursion
    (cl-flet* ((back () (and (< (point-min) (point))
                             (ignore-errors (backward-sexp) t)))
               (back-to (ch) (and (back)
                                  (eq (char-after (point)) ch)))
               (back-to* (&rest chs) (let ((pt (point)))
                                       (or (seq-every-p #'back-to chs)
                                           (progn (goto-char pt) nil)))))
      (or (back-to* ?\{ ?\[ ?@) ;@~a["foo"]{bar}
          (back-to*     ?\{ ?@) ;@~a{abc}
          (back-to*     ?\[ ?@) ;@+[1 2]
          (back)                ;@(+ 1 2) @1 or any s-expression
          (user-error "No previous s-expression or at-expression"))
      (if (looking-at-p "#;")
          (+ (point) 2)
        (point)))))

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
images in `racket-image-cache-dir'."
  (interactive)
  (dolist (file (butlast (racket-repl--list-image-cache)
                         racket-images-keep-last))
    (delete-file file)))

(defun racket--repl-insert-image (file)
  (let ((beg (point)))
    (if (and racket-images-inline (display-images-p))
        (insert-image
         (apply #'create-image
                file
                (and (image-type-available-p 'imagemagick)
                     racket-imagemagick-props
                     'imagemagick)
                nil                     ;data-p
                (append
                 '(:scale 1.0)          ;#529
                 (and (image-type-available-p 'imagemagick)
                      racket-imagemagick-props))))
      (insert (propertize (format "[file://%s]" file)
                          'font-lock-face 'italic)))
    (add-text-properties beg (point)
                         (list 'keymap racket-image-map
                               'racket-image file
                               'help-echo "RET or Mouse-2 to view image"))
    (setq racket-image-cache-dir (file-name-directory file))
    (racket-repl--clean-image-cache)))

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
  (racket--cmd/async (racket--repl-session-id)
                     '(syms)
                     (lambda (syms)
                       (setq racket--repl-namespace-symbols syms))))

(add-hook 'racket--repl-after-run-hook   #'racket--repl-refresh-namespace-symbols)

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
           (racket--completion-table racket--repl-namespace-symbols)
           :predicate #'identity
           :exclusive 'no
           :company-doc-buffer #'racket--repl-company-doc-buffer
           :company-location #'racket--repl-company-location))))

(defun racket--repl-company-doc-buffer (str)
  (racket--company-doc-buffer 'namespace str))

(defun racket--repl-company-location (str)
  (pcase (racket--cmd/await (racket--repl-session-id)
                            `(def-in-namespace ,str))
    (`(,path ,line ,_) (cons path line))))

;;; eldoc

(defun racket-repl-eldoc-point (callback &rest _more)
  "Call eldoc CALLBACK about the identifier at point.
A value for the variable `eldoc-documentation-functions'. Use
information from back end \"type\" command."
  (when (racket--cmd-open-p)
    (racket--eldoc-type callback (point))))

(defun racket-repl-eldoc-sexp-app (callback &rest _more)
  "Call eldoc CALLBACK about sexp application around point.
A value for the variable `eldoc-documentation-functions'. Use
information from back end \"type\" command."
  (when (and (racket--cmd-open-p)
             (> (point) (point-min)))
    ;; Preserve point during the dynamic extent of the eldoc calls,
    ;; because things like eldoc-box may dismiss the UI if they notice
    ;; point has moved.
    (when-let (pos (condition-case _
                       (save-excursion
                         (backward-up-list)
                         (forward-char 1)
                         (point))
                     (scan-error nil)))
      (racket--eldoc-type callback pos))))

(defun racket--eldoc-type (callback pos)
  "Obtain a \"type\" summary string from the back end.
This might be a bluebox, or a function signature discovered from
the surface syntax, or Typed Racket type information."
  (condition-case _
      (let* ((end (save-excursion (progn (goto-char pos) (forward-sexp) (point))))
             (thing (buffer-substring-no-properties pos end)))
        (when thing
          (when-let (str (racket--cmd/await
                          (racket--repl-session-id)
                          `(type namespace ,thing)))
            (racket--eldoc-do-callback callback
                                       thing
                                       (if (string-match-p "\n" str)
                                           (concat "\n" str)
                                         str)))))
    (scan-error nil)))

(defun racket-repl-eldoc-function ()
  "A value for the obsolete variable `eldoc-documentation-function'.

Obsolete: Newer versions of Emacs instead use the variable
`eldoc-documentation-functions', plural."
  nil)

;;; describe

(defun racket-repl-describe (&optional prefix)
  "Describe the identifier at point.

The command varies based on how many \\[universal-argument] prefix arguments you supply.
\\<racket-repl-mode-map>

- \\[racket-repl-describe]

  Uses the symbol at point. If no such symbol exists, you are
  prompted enter the identifier, but in this case it only
  considers definitions or imports at the file's module level --
  not local bindings nor definitions in submodules.

  - If the identifier has installed Racket documentation, then a
    simplified version of the HTML is presented in the buffer,
    including the \"blue box\", documentation prose, and
    examples.

  - Otherwise, if the identifier is a function, then its
    signature is displayed, for example \"\(name arg-1-name
    arg-2-name\)\".

- \\[universal-argument] \\[racket-repl-describe]

  Always prompts you to enter a symbol, defaulting to the symbol
  at point if any.

- \\[universal-argument] \\[universal-argument] \\[racket-repl-describe]

  This is an alias for `racket-describe-search', which uses
  installed documentation in a `racket-describe-mode' buffer
  instead of an external web browser.

The intent is to give a quick reminder or introduction to
something, regardless of whether it has installed documentation
-- and to do so within Emacs, without switching to a web browser."
  (interactive "P")
  (if (equal prefix '(16))
      (racket-describe-search)
    (pcase (racket--symbol-at-point-or-prompt prefix "Describe: "
                                              racket--repl-namespace-symbols)
      ((and (pred stringp) str)
       (racket--do-describe 'namespace (racket--repl-session-id) str)))))

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
         (list
          (xref-make str
                     (xref-make-file-location (racket-file-name-back-to-front path)
                                              line col))))))
     (`relative
      (let ((path (racket--rkt-or-ss-path
                   (expand-file-name (substring-no-properties str 1 -1)))))
        (list
         (xref-make str
                    (xref-make-file-location (racket-file-name-back-to-front path)
                                             1 0))))))
   (pcase (racket--cmd/await racket--repl-session-id `(def namespace ,str))
     (`(,path ,line ,col)
      (list
       (xref-make str
                  (xref-make-file-location (racket-file-name-back-to-front path)
                                           line col))))
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
\\<racket-repl-mode-map>

- \\[racket-repl-documentation]

  Uses the symbol at point. Tries to find documentation for an
  identifer defined in the current namespace.

  If no such identifer exists, opens the Search Manuals page. In
  this case, the variable `racket-documentation-search-location'
  determines whether the search is done locally as with `raco
  doc`, or visits a URL.

- \\[universal-argument] \\[racket-repl-documentation]

  Prompts you to enter a symbol, defaulting to the symbol at
  point if any.

- \\[universal-argument] \\[universal-argument] \\[racket-repl-documentation]

  Prompts you to enter anything, defaulting to the symbol at
  point if any.

  Proceeds directly to the Search Manuals page. Use this if you
  would like to see documentation for all identifiers named
  \"define\", for example."
  (interactive "P")
  (racket--doc prefix 'namespace racket--repl-namespace-symbols))

;;; racket-repl-mode definition per se

(defvar racket-repl-mode-map
  (racket--easy-keymap-define
   '(("C-m"             racket-repl-submit)
     ("C-j"             newline-and-indent)
     ("TAB"             indent-for-tab-command)
     ("C-M-u"           racket-backward-up-list)
     ("C-M-q"           prog-indent-sexp)
     ("M-p"             racket-repl-previous-input)
     ("M-n"             racket-repl-next-input)
     ("C-c C-u"         racket-repl-clear-input)
     ("C-c C-p"         racket-repl-previous-prompt-or-run)
     ("C-c C-n"         racket-repl-next-prompt-or-run)
     ("C-c C-o"         racket-repl-delete-output)
     ("C-c C-e f"       racket-expand-file)
     ("C-c C-e x"       racket-expand-definition)
     ("C-c C-e e"       racket-expand-last-sexp)
     ("C-c C-e r"       racket-expand-region)
     ("M-C-y"           racket-insert-lambda)
     ("C-c C-d"         racket-repl-documentation)
     ("C-c C-."         racket-repl-describe)
     ("C-c C-s"         racket-describe-search)
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

(defvar-local racket--repl-fontify-region-function #'font-lock-default-fontify-buffer)
(defun racket--repl-fontify-region (beg end loudly)
  "Limit to input and value spans."
  (racket--repl-call-with-value-and-input-ranges
   beg end
   (lambda (beg end v)
     (when v
       (funcall racket--repl-fontify-region-function beg end loudly))))
  (put-text-property beg end 'fontified t)
  `(jit-lock-bounds ,beg . ,end))

(define-derived-mode racket-repl-mode fundamental-mode "Racket-REPL"
  "Major mode for Racket REPL.

You may use `xref-find-definitions' \\[xref-find-definitions] and
`xref-pop-marker-stack' \\[xref-pop-marker-stack]:
`racket-repl-mode' adds a backend to the variable
`xref-backend-functions'. This backend uses information about
identifier bindings and modules from the REPL's namespace.

\\{racket-repl-mode-map}"
  ;; Here we set some values that will definitely be used when the
  ;; buffer is created by the `racket-repl' command. Otherwise,
  ;; `racket--hash-lang-configure-repl-buffer-from-edit-buffer' will
  ;; refresh these upon each run command via
  ;; `racket--repl-before-run-hook', drawing values from the
  ;; `racket-mode' or `racket-hash-lang-mode' edit buffer to also use
  ;; in the repl.
  (setq-local font-lock-fontify-region-function #'racket--repl-fontify-region)
  (font-lock-set-defaults)
  (setq-local window-point-insertion-type t)
  (setq-local indent-line-function #'racket-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local completion-at-point-functions (list #'racket-repl-complete-at-point))
  (when (boundp 'eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions
              #'racket-repl-eldoc-sexp-app
              nil t)
    (add-hook 'eldoc-documentation-functions
              #'racket-repl-eldoc-point
              nil t))
  (setq-local next-error-function #'racket-repl-next-error)
  (racket-repl-read-history)
  (add-hook 'kill-buffer-hook #'racket-repl-write-history nil t)
  (add-hook 'kill-emacs-hook #'racket-repl-write-all-histories nil t)
  (add-hook 'xref-backend-functions #'racket-repl-xref-backend-function nil t)
  (add-to-list 'semantic-symref-filepattern-alist
               '(racket-repl-mode "*.rkt" "*.rktd" "*.rktl")))

(defun racket-repl-write-all-histories ()
  "Call `racket-repl-write-history' for all `racket-repl-mode' buffers.
A suitable value for the hook `kill-emacs-hook'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'racket-repl-mode)
        (racket-repl-write-history)))))

(defun racket--buffer-name-slug ()
  "Change `buffer-name' to a string that is a valid filename."
  ;; 2. But not leading or trailing ?-
  (replace-regexp-in-string
   (rx (or (seq bos (+ ?-))
           (seq (+ ?-) eos)))
   ""
   ;; 1. Replace runs of anything that is not alnum with a single ?-.
   (replace-regexp-in-string
    (rx (+ (not (any alnum))))
    "-"
    (buffer-name))))

;;; Clearing the REPL

(defun racket-repl-clear ()
  "Delete all text in the REPL.

A suitable value for the hook `racket-before-run-hook' if you
want the REPL buffer to be cleared before each run, much like
with Dr Racket. To do so you can use `customize', or, add to your
Emacs init file something like:

  (add-hook \\='racket-before-run-hook #\\='racket-repl-clear)

See also the command `racket-repl-clear-leaving-last-prompt'."
  (racket--do-repl-clear nil))

(defun racket-repl-clear-leaving-last-prompt ()
  "Delete all text in the REPL, except for the last prompt."
  (interactive)
  (racket--do-repl-clear t))

(defun racket--do-repl-clear (leave-last-prompt-p)
  (cond ((eq major-mode 'racket-repl-mode)
         (racket--delete-all-buffer-text leave-last-prompt-p))
        ((racket--edit-mode-p)
         (when (get-buffer racket-repl-buffer-name)
           (with-current-buffer racket-repl-buffer-name
             (racket--delete-all-buffer-text leave-last-prompt-p))))
        (t
         (user-error "Current buffer is not a Racket edit or REPL buffer"))))

(defun racket--delete-all-buffer-text (leave-last-prompt-p)
  (widen)
  (let ((end (if leave-last-prompt-p
                 (save-excursion
                   (goto-char (point-max))
                   (racket-repl-previous-prompt)
                   (racket-repl-next-prompt)
                   (forward-line 0)   ;BOL ignoring fields
                   (point))
               (point-max)))
        (inhibit-read-only t))
    (delete-region (point-min) end)
    (goto-char (point-max))
    (dolist (win (get-buffer-window-list))
      (set-window-point win (point-max)))))

;;; Errors

(defvar racket-repl-error-location-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'racket-repl-goto-error-location)
    map))

;; Note about error locations: On the one hand, representing error
;; locations using markers has a benefit: The user can edit the file
;; to fix the error or test failure, without disturbing the ability to
;; visit subsequent error locations. On the other hand, markers impose
;; some cost on edit operations; and anyway we can only create a
;; marker if a buffer already exists for the file. Our tactic to pay
;; the cost only when we get the benefit: Initially create the
;; locations using positions. When the user wants to visit a location,
;; "upgrade" our values for that file to use markers (but only since
;; the last run), visiting the file if necessary. And our
;; before-run-hook that resets next-error also "downgrades" /all/ locs
;; from markers back to positions.

(defun racket--format-error-location (raw-loc)
  ;; Initially racket-error-loc is (list file beg-pos end-pos).
  (pcase raw-loc
    (`(,str ,file ,_line ,_col ,pos ,span)
     (propertize str
                 'racket-error-loc (list file pos (+ pos span))
                 'rear-nonsticky t
                 'font-lock-face 'racket-repl-error-location
                 'keymap racket-repl-error-location-map))
    (_ (propertize "location N/A" 'font-lock-face 'italic))))

(defun racket--repl-upgrade-error-locations (file)
  ;; Change all racket-error-locs for FILE, since the last run, which
  ;; use positions, instead to use markers, loading FILE in a buffer
  ;; if necessary.
  (let ((buf (or (get-file-buffer file)
                 (let ((find-file-suppress-same-file-warnings t))
                   (find-file-noselect file))))
        (from (save-excursion
                (racket--repl-after-previous-field '(run))
                (point))))
    (racket--map-error-locations
     from
     (lambda (v)
       (pcase v
         ((and `(,this-file ,beg ,end) (guard (equal this-file file)))
          (ignore this-file) ;"unused lexical variable" on some Emacs
          (list (set-marker (make-marker) beg buf)
                (set-marker (make-marker) end buf)))
         (v v))))))

(defun racket--repl-downgrade-error-locations ()
  ;; Change all racket-error-locs in the buffer, which use markers,
  ;; instead to use positions, and make the old markers point nowhere.
  (racket--map-error-locations
   (point-min)
   (lambda (v)
     (pcase v
       (`(,beg ,end)
        (prog1 (list (buffer-file-name (marker-buffer beg))
                     (marker-position beg)
                     (marker-position end))
          (set-marker beg nil)
          (set-marker end nil)))
       (v v)))))

(defun racket--map-error-locations (start fun)
  ;; Apply FUN to racket-error-loc property spans after START to eob.
  (let ((inhibit-read-only t)
        (prop 'racket-error-loc))
    (while
        (when-let ((beg (next-single-property-change start prop))
                   (end (next-single-property-change beg   prop))
                   (val (get-text-property beg prop)))
          (put-text-property beg end prop (funcall fun val))
          (setq start end)))))

(defun racket-repl-goto-error-location ()
  "When racket-error-loc prop exists at point, `compilation-goto-locus'."
  (interactive)
  (pcase (get-text-property (point) 'racket-error-loc)
    ;; A racket-error-loc property using file plus position integers.
    (`(,file ,_beg ,_end)
     (racket--repl-upgrade-error-locations file)
     (racket-repl-goto-error-location))
    ;; A racket-error-loc property using markers pointing into the
    ;; buffer.
    (`(,beg ,end)
     (compilation-goto-locus (point-marker) beg end))))

(defvar-local racket--errors-reset t)
(defvar-local racket--errors-point-min nil)
(defun racket--repl-forget-errors ()
  "Forget existing errors in the REPL.
Although they remain clickable they will be ignored by
`next-error' and `previous-error'."
  (with-racket-repl-buffer
    (racket--repl-downgrade-error-locations)
    (setq racket--errors-reset t)
    (setq racket--errors-point-min (point-max))
    ;; Set this so `next-error-find-buffer' chooses us.
    (setq next-error-last-buffer (current-buffer))))
(add-hook 'racket--repl-before-run-hook #'racket--repl-forget-errors)

(defun racket-repl-next-error (count reset)
  "A value for the variable `next-error-function'."
  (let ((prop 'racket-error-loc))
    (cl-flet* ((get () (get-text-property (point) prop))
               (next () (next-single-property-change (point) prop))
               (prev () (previous-single-property-change (point) prop))
               (go-next () (goto-char (or (next) (point-max))))
               (go-prev () (goto-char (max (or (prev) racket--errors-point-min)
                                           racket--errors-point-min))))
      (when (or reset racket--errors-reset)
        (goto-char racket--errors-point-min))
      (setq racket--errors-reset nil)
      (if (< 0 count)
          (dotimes (_ count)
            (when (get) (go-next))
            (go-next)
            (unless (get) (go-next)))
        (dotimes (_ (- count))
          (when (get) (go-prev))
          (go-prev)
          (unless (get) (go-prev))))
      (unless (get)
        (user-error "No more errors"))
      (racket-repl-goto-error-location))))

;;; Nav

(defun racket--repl-after-previous-field (kinds)
  ;; If already desired kind of field, move before
  (when (and (not (bobp))
             (memq (field-at-pos (point)) kinds))
    (goto-char (field-beginning (point) t)))
  ;; While not desired kind of field, move before.
  (while (and (not (bobp))
              (not (memq (field-at-pos (point)) kinds)))
    (goto-char (field-beginning (point) t))))

(defun racket--repl-after-next-field (kinds)
  ;; If already desired kind of field, move after it.
  (when (and (not (eobp))
             (memq (field-at-pos (1+ (point))) kinds))
    (goto-char (field-end (point) t)))
  ;; While not desired kind of field, move after.
  (while (and (not (eobp))
              (not (memq (field-at-pos (1+ (point))) kinds)))
    (goto-char (field-end (point) t)))
  ;; When we've found the desired kind, move after it.
  (when (and (not (eobp))
             (memq (field-at-pos (1+ (point))) kinds))
    (goto-char (field-end (point) t))))

(defun racket-repl-previous-prompt ()
  "Move to the character after the previous prompt."
  (interactive)
  (racket--repl-after-previous-field '(prompt)))

(defun racket-repl-next-prompt ()
  "Move to the character after the next prompt."
  (interactive)
  (racket--repl-after-next-field '(prompt)))

(defun racket-repl-previous-prompt-or-run ()
  "Move to the character after the previous prompt or run."
  (interactive)
  (racket--repl-after-previous-field '(prompt run)))

(defun racket-repl-next-prompt-or-run ()
  "Move to the character after the next prompt or run."
  (interactive)
  (racket--repl-after-next-field '(prompt run)))

(defun racket-repl-delete-output ()
  "Delete output from REPL interaction.

When point is within a prompt or input, delete the output of the
previous interaction.

When point is within output, delete all of that congtiguous
output."
  (interactive)
  (let* ((pt (point))
         (output-fields '(value stdout stderr error deleted))
         (beg-of-output (progn
                          ;; Skip backward over non-output fields
                          (unless (memq (get-text-property (point) 'field) output-fields)
                            (while (and (not (bobp))
                                        (not (memq (field-at-pos (point)) output-fields)))
                              (goto-char (field-beginning (point) t))))
                          ;; Skip backward over output fields
                          (while (and (not (bobp))
                                      (memq (field-at-pos (point)) output-fields))
                            (goto-char (field-beginning (point) t)))
                          (point)))
         (end-of-output (progn
                          (while (and (not (eobp))
                                      (or (memq (field-at-pos (1+ (point))) output-fields)
                                          ;; After stdout/stderr there
                                          ;; might be a \n with nil
                                          ;; field property.
                                          (and (not (field-at-pos (1+ (point))))
                                               (eq ?\n (char-after)))))
                            (goto-char (field-end (point) t)))
                          (point))))
    (if (and (< beg-of-output end-of-output)
             (not (eq (field-at-pos (1+ beg-of-output)) 'deleted)))
        (let ((inhibit-read-only t))
          (delete-region beg-of-output end-of-output)
          (save-excursion
            (goto-char beg-of-output)
            (insert (propertize "(output deleted)\n"
                                'field 'deleted
                                'read-only t
                                'font-lock-face racket-repl-message))))
      (goto-char pt)
      (user-error "Can't find output to delete"))))

;;; Input history

;; TODO: Make defcustom
(defvar racket-repl-history-size 128)

(defvar-local racket--repl-input-ring nil)
(defvar-local racket--repl-input-ring-index nil)

(defun racket--repl-add-to-input-history (input)
  "To be called from `racket-repl-submit'."
  (unless (ring-p racket--repl-input-ring)
    (setq racket--repl-input-ring (make-ring racket-repl-history-size)))
  (when (or (ring-empty-p racket--repl-input-ring)
            (not (string-equal (ring-ref racket--repl-input-ring 0) input)))
    (ring-insert racket--repl-input-ring input))
  (setq racket--repl-input-ring-index nil))

(defun racket-repl-previous-input (arg)
  (interactive "*p")
  (unless (and (ring-p racket--repl-input-ring)
               (not (ring-empty-p racket--repl-input-ring)))
    (user-error "No history"))
  (unless (racket--repl-prompt-mark-end)
    (user-error "No prompt"))
  (setq racket--repl-input-ring-index
        (if racket--repl-input-ring-index
            (+ racket--repl-input-ring-index arg)
          (if (< 0 arg)
              (1- arg) ;0 is already previous item in ring
            arg)))
  (delete-region (racket--repl-prompt-mark-end) (point-max))
  (let ((input (ring-ref racket--repl-input-ring racket--repl-input-ring-index)))
    (insert input)))

(defun racket-repl-next-input (arg)
  (interactive "*p")
  (racket-repl-previous-input (- arg)))

(defun racket-repl-clear-input ()
  (interactive)
  (when-let (prompt-end (racket--repl-prompt-mark-end))
    (delete-region prompt-end (point-max)))
  (setq racket--repl-input-ring-index nil))

(defun racket--repl-history-filename ()
  (make-directory racket-repl-history-directory t)
  (expand-file-name (concat "input-history-" (racket--buffer-name-slug))
                    racket-repl-history-directory))

(defun racket-repl-write-history ()
  (when (and (ring-p racket--repl-input-ring)
             (not (ring-empty-p racket--repl-input-ring)))
    (let* ((items (ring-elements racket--repl-input-ring))
           (str   (format "%S" items)))
      (write-region str nil (racket--repl-history-filename) nil 'no-message))))

(defun racket-repl-read-history ()
  (let* ((file (racket--repl-history-filename))
         (items (with-temp-buffer
                  (ignore-errors
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (read (current-buffer))))))
    ;; Although `ring-convert-sequence-to-ring' looks handy, it
    ;; creates a ring without letting us set the size (capacity).
    (setq racket--repl-input-ring (make-ring racket-repl-history-size))
    (dolist (item items)
      (ring-insert-at-beginning racket--repl-input-ring item))))

(provide 'racket-repl)

;; racket-repl.el ends here

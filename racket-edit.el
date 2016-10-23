;;; racket-edit.el

;; Copyright (c) 2013-2016 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

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

;; racket-mode per se, i.e. the .rkt file buffers

(require 'cl-lib)
(require 'cl-macs)
(require 'racket-custom)
(require 'racket-common)
(require 'racket-complete)
(require 'racket-util)
(require 'hideshow)

(defun racket-run (&optional errortracep)
  "Save and evaluate the buffer in REPL, much like DrRacket's Run.

If point is within a submodule form, the REPL \"enters\" that
submodule (uses its language info and namespace).

When you run again, the file is evaluated from scratch -- the
custodian releases resources like threads and the evaluation
environment is reset to the contents of the file. In other words,
like DrRacket, this provides the predictability of a \"static\"
baseline, plus the ability to explore interatively using the
REPL.

See also `racket-run-and-switch-to-repl', which is even more like
DrRacket's Run because it selects the REPL window (gives it the
focus), too.

With a C-u prefix, uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting.

Output in the `*Racket REPL*` buffer that describes a file and
position is automatically \"linkified\". To visit, move point
there and press <kdb>RET</kbd>, mouse click, or use a
Compilation mode command such as \\[next-error] (next error).
Examples of such text include:

- Racket error messages.
- `rackunit` test failure location messages.
- `print`s of `#<path>` objects.

In the `*Racket REPL*` buffer you can issue some special
commands. Some of them are the foundation for Emacs commands.
Others are available only as a command in the REPL.

- `,help`: See these commands.

- `,top`: Reset the REPL to an empty module (i.e. a racket/base namespace).

- `,run <module>` : What `racket-run' uses.
  - `<module> = <file> | (<file> <submodule-id> ...)`
  - `<file> = file.rkt | /path/to/file.rkt | \"file.rkt\" | \"/path/to/file.rkt\"`

- `,exit`: Exit Racket. Handy in a `#lang` like r5rs where the
  `exit` procedure is not available. (Regardless of how Racket
  exits, the `*Racket REPL*` buffer is not killed and is reused
  if you `racket-run' again.)

- `,doc <symbol-or-string>`: Look for `<symbol-or-string>` in
  Racket's documentation. What `racket-doc' uses.

- `,cd`, `,pwd`: Change and show `current-directory`.

- `,log` controls the log output level, overall, as well as for
  specific named loggers created with `define-logger`.

    - `,log`: Show the current levels.

    - `,log <logger> <level>`: Set a logger to show at least level
      `none`, `fatal`, `error`, `warning`, `info`, or `debug`.

    - `,log <logger> <level>`: Set a logger to use the default
      level.

    - `,log <level>`: Set the default level for all other loggers
      not specified individually.
"
  (interactive "P")
  (racket--do-run (if errortracep
                      'high
                    racket-error-context)))

(defun racket-run-with-errortrace ()
  "Run with `racket-error-context' temporarily set to 'high.
This is just `racket-run' with a C-u prefix. Defined as a function so
it can be a menu target."
  (interactive)
  (racket-run t))

(defvar-local racket-user-command-line-arguments
  nil
  "List of command-line arguments to supply to your Racket program.

Accessible in your Racket program in the usual way -- the
parameter `current-command-line-arguments` and friends.

This is an Emacs buffer-local variable -- convenient to set as a
file local variable. For example at the end of your .rkt file:

    ;; Local Variables:
    ;; racket-user-command-line-arguments: (\"-f\" \"bar\")
    ;; End:

Set this way the value must be an unquoted list of strings such
as:

    (\"-f\" \"bar\")

but NOT:

    '(\"-f\" \"bar\")
    (list \"-f\" \"bar\")
")

(defun racket--do-run (context-level &optional what-to-run)
  "Helper function for `racket-run'-like commands.

Supplies CONTEXT-LEVEL to the back-end ,run command; see run.rkt.

If supplied, WHAT-TO-RUN should be a buffer filename, or a cons
of a file name to a list of submodule symbols. Otherwise, the
`racket--what-to-run' is used."
  (unless (eq major-mode 'racket-mode)
    (error "Current buffer is not a racket-mode buffer"))
  (when (or (buffer-modified-p)
            (and (racket--buffer-file-name)
                 (not (file-exists-p (racket--buffer-file-name)))))
    (save-buffer))
  (remove-overlays (point-min) (point-max) 'racket-uncovered-overlay)
  (racket--invalidate-completion-cache)
  (racket--invalidate-type-cache)
  (racket--repl-eval ",run %S %s %s %s %S\n"
                     (or what-to-run (racket--what-to-run))
                     racket-memory-limit
                     racket-pretty-print
                     context-level
                     racket-user-command-line-arguments))

(defun racket--what-to-run ()
  (cons (racket--buffer-file-name) (racket--submod-path)))

(defun racket--submod-path ()
  (and (racket--lang-p)
       (racket--modules-at-point)))

(defun racket--lang-p ()
  "Is #lang the first sexpr in the file?"
  (save-excursion
    (goto-char 0)
    (ignore-errors
      (forward-sexp)
      (backward-sexp)
      (looking-at (rx "#lang")))))

(defun racket--modules-at-point ()
  "List of module names that point is within, from outer to inner."
  (let ((xs nil))
    (condition-case ()
        (save-excursion
          (racket--escape-string-or-comment)
          (while t
            (when (looking-at (rx ?\(
                                  (or "module " "module* " "module+ ")
                                  (group (+ (or (syntax symbol)
                                                (syntax word))))))
              (add-to-list 'xs
                           (intern (match-string-no-properties 1))
                           t
                           #'ignore)) ;i.e. never equal, always add
            (backward-up-list)))
      (error (reverse xs)))))

(defun racket-run-and-switch-to-repl (&optional errortracep)
  "This is `racket-run' followed by `racket-switch-to-repl'.

With a C-u prefix, uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting."
  (interactive "P")
  (racket-run errortracep)
  (racket-repl))

(defun racket-racket ()
  "Do `racket <file>` in `*shell*` buffer."
  (interactive)
  (racket--shell (concat racket-racket-program
                         " "
                         (shell-quote-argument (racket--buffer-file-name)))))

(defun racket-test (&optional coverage)
  "Run the `test` submodule.

With prefix, runs with coverage instrumentation and highlights
uncovered code.

Put your tests in a `test` submodule. For example:

    (module+ test
      (require rackunit)
      (check-true #t))

rackunit test failure messages show the location. You may use
`next-error' to jump to the location of each failing test.

See also:
- `racket-fold-all-tests'
- `racket-unfold-all-tests'
"
  (interactive "P")
  (racket--do-run (if coverage 'coverage racket-error-context)
                  (list 'submod (racket--buffer-file-name) 'test))
  (when coverage
    (message "Running tests with coverage instrumentation enabled...")
    (while (not (racket--repl-command "prompt"))
      (sit-for 0.5))
    (message "Checking coverage results...")
    (let ((xs (racket--repl-command "get-uncovered")))
      (dolist (x xs)
        (let ((beg (car x))
              (end (cdr x)))
          (let ((o (make-overlay beg end)))
            (overlay-put o 'name 'racket-uncovered-overlay)
            (overlay-put o 'priority 100)
            (overlay-put o 'face font-lock-warning-face))))
      (if (not xs)
          (message "Coverage complete.")
        (message (format "Missing coverage in %s place(s)." (length xs)))
        (goto-char (car (car xs)))))))

(defun racket-raco-test ()
  "Do `raco test -x <file>` in `*shell*` buffer.
To run <file>'s `test` submodule."
  (interactive)
  (racket--shell (concat racket-raco-program
                         " test -x "
                         (shell-quote-argument (racket--buffer-file-name)))))

(defun racket--shell (cmd)
  (let ((w (selected-window)))
    (save-buffer)
    (let ((rw (get-buffer-window "*shell*" t)))
      (if rw
          (select-window rw)
        (other-window -1)))
    (message (concat cmd "..."))
    (shell)
    (pop-to-buffer-same-window "*shell*")
    (comint-send-string "*shell*" (concat cmd "\n"))
    (select-window w)
    (sit-for 3)
    (message nil)))


;;; visiting defs and mods

(defun racket-visit-definition (&optional prefix)
  "Visit definition of symbol at point.

Use \\[racket-unvisit] to return.

Note: Only finds symbols defined in the current namespace. You
may need to invoke `racket-run' on the current buffer, first.

Note: Only visits the definition of module level identifiers (i.e.
things for which Racket's `identifier-binding` function returns a
list, as opposed to `'lexical`).

Note: If the definition is from Racket's `#%kernel` module, it
will tell you so but won't visit the definition site."
  (interactive "P")
  (let ((sym (racket--identifier-at-point-or-prompt prefix
                                                    "Visit definition of: ")))
    (when sym
      (racket--do-visit-def-or-mod "def" sym))))

(defun racket--do-visit-def-or-mod (cmd sym)
  "CMD must be \"def\" or \"mod\". SYM must be `symbolp`."
  (pcase (racket--repl-command "%s %s" cmd sym)
    (`(,path ,line ,col)
     (racket--push-loc)
     (find-file path)
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char col)
     (message "Type M-, to return"))
    (`kernel
     (message "`%s' defined in #%%kernel -- source not available." sym))
    (_ (when (y-or-n-p "Not found. Run current buffer and try again? ")
         (racket-run)
         (racket--do-visit-def-or-mod cmd sym)))))

(defun racket-visit-module (&optional prefix)
  "Visit definition of module at point, e.g. net/url or \"file.rkt\".

Use \\[racket-unvisit] to return.

Note: Only works if you've `racket-run' the buffer so that its
namespace is active.

See also: `racket-find-collection'."
  (interactive "P")
  (let* ((v (thing-at-point 'filename)) ;matches both net/url and "file.rkt"
         (v (and v (substring-no-properties v)))
         (v (if (or prefix (not v))
                (read-from-minibuffer "Visit module: " (or v ""))
              v)))
    (racket--do-visit-def-or-mod "mod" v)))

(defun racket-doc (&optional prefix)
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
  (let ((sym (racket--identifier-at-point-or-prompt prefix
                                                    "Racket help for: ")))
    (when sym
      (racket--repl-command "doc %s" sym))))

(defvar racket--loc-stack '())

(defun racket--push-loc ()
  (push (cons (current-buffer) (point))
        racket--loc-stack))

(defun racket-unvisit ()
  "Return from previous `racket-visit-definition' or `racket-visit-module'."
  (interactive)
  (if racket--loc-stack
      (pcase (pop racket--loc-stack)
        (`(,buffer . ,pt)
         (pop-to-buffer-same-window buffer)
         (goto-char pt)))
    (message "Stack empty.")))


;;; code folding

;;;###autoload
(add-to-list 'hs-special-modes-alist
             '(racket-mode "(" ")" ";" nil nil))

(defun racket--for-all-tests (verb f)
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^(module[+*]? test" (point-max) t)
        (funcall f)
        (cl-incf n)
        (goto-char (match-end 0)))
      (message "%s %d test submodules" verb n))))

(defun racket-fold-all-tests ()
  "Fold (hide) all test submodules."
  (interactive)
  (racket--for-all-tests "Folded" 'hs-hide-block))

(defun racket-unfold-all-tests ()
  "Unfold (show) all test submodules."
  (interactive)
  (racket--for-all-tests "Unfolded" 'hs-show-block))


;;; macro expansion

(defun racket-expand-region (start end &optional prefix)
  "Like `racket-send-region', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "rP")
  (if (region-active-p)
      (progn
        (racket--repl-send-expand-command prefix)
        (racket--send-region-to-repl start end))
    (beep)
    (message "No region.")))

(defun racket-expand-definition (&optional prefix)
  "Like `racket-send-definition', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-definition))

(defun racket-expand-last-sexp (&optional prefix)
  "Like `racket-send-last-sexp', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-last-sexp))

(defun racket--repl-send-expand-command (prefix)
  (comint-send-string (racket--get-repl-buffer-process)
                      (if prefix ",exp!" ",exp ")))

(defun racket-expand-again ()
  "Macro expand again the previous expansion done by one of:
- `racket-expand-region'
- `racket-expand-definition'
- `racket-expand-last-sexp'
- `racket-expand-again'"
  (interactive)
  (comint-send-string (racket--get-repl-buffer-process) ",exp+\n"))


;;; requires

(defun racket-tidy-requires ()
  "Make a single top-level `require`, modules sorted, one per line.

All top-level `require` forms are combined into a single form.
Within that form:

- A single subform is used for each phase level, sorted in this
  order: for-syntax, for-template, for-label, for-meta, and
  plain (phase 0).

  - Within each level subform, the modules are sorted:

    - Collection path modules -- sorted alphabetically.

    - Subforms such as `only-in`.

    - Quoted relative requires -- sorted alphabetically.

At most one module is listed per line.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

See also: `racket-trim-requires' and `racket-base-requires'."
  (interactive)
  (let* ((reqs (racket--top-level-requires 'find))
         (new (and reqs
                   (racket--repl-command "requires/tidy %S" reqs))))
    (when (not (string-equal "" new))
      (goto-char (racket--top-level-requires 'kill))
      (insert (concat new "\n")))))

(defun racket-trim-requires ()
  "Like `racket-tidy-requires' but also deletes unused modules.

Note: This only works when the source file can be evaluated with
no errors.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

See also: `racket-base-requires'."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (let* ((reqs (racket--top-level-requires 'find))
         (new (and reqs
                   (racket--repl-command
                    "requires/trim \"%s\" %S"
                    (racket--buffer-file-name)
                    reqs))))
    (when (not new)
      (error "Can't do, source file has error"))
    (goto-char (racket--top-level-requires 'kill))
    (when (not (string-equal "" new))
      (insert (concat new "\n")))))

(defun racket-base-requires ()
  "Change from `#lang racket` to `#lang racket/base`.

Adds explicit requires for modules that are provided by `racket`
but not by `racket/base`.

This is a recommended optimization for Racket applications.
Avoiding loading all of `racket` can reduce load time and memory
footprint.

Also, as does `racket-trim-requires', this removes unneeded
modules and tidies everything into a single, sorted require form.

Note: This only works when the source file can be evaluated with
no errors.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

Note: Currently this only helps change `#lang racket` to
`#lang racket/base`. It does *not* help with other similar conversions,
such as changing `#lang typed/racket` to `#lang typed/racket/base`."
  (interactive)
  (when (racket--buffer-start-re "^#lang.*? racket/base$")
    (error "Already using #lang racket/base. Nothing to change."))
  (unless (racket--buffer-start-re "^#lang.*? racket$")
    (error "File does not use use #lang racket. Cannot change."))
  (when (buffer-modified-p) (save-buffer))
  (let* ((reqs (racket--top-level-requires 'find))
         (new (racket--repl-command
               "requires/base \"%s\" %S"
               (racket--buffer-file-name)
               reqs)))
    (when (not new)
      (error "Source file has error"))
    (goto-char (point-min))
    (re-search-forward "^#lang.*? racket$")
    (insert "/base")
    (goto-char (or (racket--top-level-requires 'kill)
                   (progn (insert "\n") (point))))
    (when (not (string= "" new))
      (insert (concat new "\n")))))

(defun racket--buffer-start-re (re)
  (save-excursion
    (ignore-errors
      (goto-char (point-min))
      (re-search-forward re)
      t)))

(defun racket--top-level-requires (what)
  "Identify all top-level requires and do WHAT.

When WHAT is 'find, returns the top-level require forms.

When WHAT is 'kill, kill the top-level requires, returning the
location of the first one."
  (save-excursion
    (goto-char (point-min))
    (let ((first-beg nil)
          (requires nil))
      (while (re-search-forward "^(require " nil t)
        (let* ((beg (progn (up-list -1)   (point)))
               (end (progn (forward-sexp) (point)))
               (str (buffer-substring-no-properties beg end))
               (sexpr (read str)))
          (unless first-beg (setq first-beg beg))
          (setq requires (cons sexpr requires))
          (when (eq 'kill what)
            (kill-sexp -1)
            (delete-blank-lines))))
      (if (eq 'kill what) first-beg requires))))


;;; racket-check-syntax

(defvar racket--highlight-overlays nil)

(defun racket--highlight (beg end defp)
  ;; Unless one of our highlight overlays already exists there...
  (let ((os (overlays-at beg)))
    (unless (cl-some (lambda (o) (member o racket--highlight-overlays)) os)
      (let ((o (make-overlay beg end)))
        (setq racket--highlight-overlays (cons o racket--highlight-overlays))
        (overlay-put o 'name 'racket-check-syntax-overlay)
        (overlay-put o 'priority 100)
        (overlay-put o 'face (if defp
                                 racket-check-syntax-def-face
                               racket-check-syntax-use-face))))))

(defun racket--unhighlight-all ()
  (while racket--highlight-overlays
    (delete-overlay (car racket--highlight-overlays))
    (setq racket--highlight-overlays (cdr racket--highlight-overlays))))

(defun racket--point-entered (old new)
  (pcase (get-text-property new 'help-echo)
    (s (message s)))
  (pcase (get-text-property new 'racket-check-syntax-def)
    ((and uses `((,beg ,end) . ,_))
     (pcase (get-text-property beg 'racket-check-syntax-use)
       (`(,beg ,end) (racket--highlight beg end t)))
     (dolist (use uses)
       (pcase use (`(,beg ,end) (racket--highlight beg end nil))))))
  (pcase (get-text-property new 'racket-check-syntax-use)
    (`(,beg ,end)
     (racket--highlight beg end t)
     (dolist (use (get-text-property beg 'racket-check-syntax-def))
       (pcase use (`(,beg ,end) (racket--highlight beg end nil)))))))

(defun racket--point-left (old new)
  (racket--unhighlight-all))

(defun racket-check-syntax-mode-quit ()
  (interactive)
  (racket-check-syntax-mode -1))

(defun racket-check-syntax-mode-goto-def ()
  "When point is on a use, go to its definition."
  (interactive)
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,end) (goto-char beg))))

(defun racket-check-syntax-mode-forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,end)
     (pcase (get-text-property beg 'racket-check-syntax-def)
       (uses (let* ((pt (point))
                    (ix-this (cl-loop for ix from 0 to (1- (length uses))
                                      for use = (nth ix uses)
                                      when (and (<= (car use) pt) (< pt (cadr use)))
                                      return ix))
                    (ix-next (+ ix-this amt))
                    (ix-next (if (> amt 0)
                                 (if (>= ix-next (length uses)) 0 ix-next)
                               (if (< ix-next 0) (1- (length uses)) ix-next)))
                    (next (nth ix-next uses)))
               (goto-char (car next))))))
    (_ (pcase (get-text-property (point) 'racket-check-syntax-def)
         (`((,beg ,end) . ,_) (goto-char beg))))))


(defun racket-check-syntax-mode-goto-next-use ()
  "When point is on a use, go to the next (sibling) use."
  (interactive)
  (racket-check-syntax-mode-forward-use 1))

(defun racket-check-syntax-mode-goto-prev-use ()
  "When point is on a use, go to the previous (sibling) use."
  (interactive)
  (racket-check-syntax-mode-forward-use -1))

(defun racket-check-syntax-mode-help ()
  (interactive)
  (describe-function #'racket-check-syntax-mode))

(defun racket-check-syntax-mode-rename ()
  (interactive)
  ;; If we're on a def, get its uses. If we're on a use, get its def.
  (let* ((pt (point))
         (uses (get-text-property pt 'racket-check-syntax-def))
         (def  (get-text-property pt 'racket-check-syntax-use)))
    ;; If we got one, get the other.
    (when (or uses def)
      (let* ((uses (or uses (get-text-property (car def)   'racket-check-syntax-def)))
             (def  (or def  (get-text-property (caar uses) 'racket-check-syntax-use)))
             (locs (cons def uses))
             (strs (mapcar (lambda (loc)
                             (apply #'buffer-substring-no-properties loc))
                           locs)))
        ;; Proceed only if all the strings are the same. (They won't
        ;; be for e.g. import bindings.)
        (when (cl-every (lambda (s) (equal (car strs) s))
                        (cdr strs))
          (let ((new (read-from-minibuffer (format "Rename %s to: " (car strs))))
                (marker-pairs
                 (mapcar (lambda (loc)
                           (let ((beg (make-marker))
                                 (end (make-marker)))
                             (set-marker beg (nth 0 loc) (current-buffer))
                             (set-marker end (nth 1 loc) (current-buffer))
                             (list beg end)))
                         locs))
                (point-marker (let ((m (make-marker)))
                                (set-marker m (point) (current-buffer)))))
            (racket-check-syntax-mode -1)
            (dolist (marker-pair marker-pairs)
              (let ((beg (marker-position (nth 0 marker-pair)))
                    (end (marker-position (nth 1 marker-pair))))
                (delete-region beg end)
                (goto-char beg)
                (insert new)))
            (goto-char (marker-position point-marker))
            (racket-check-syntax-mode 1)))))))

(defun racket-check-syntax-mode-goto-next-def ()
  (interactive)
  (let ((pos (next-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (next-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(defun racket-check-syntax-mode-goto-prev-def ()
  (interactive)
  (let ((pos (previous-single-property-change (point) 'racket-check-syntax-def)))
    (when pos
      (unless (get-text-property pos 'racket-check-syntax-def)
        (setq pos (previous-single-property-change pos 'racket-check-syntax-def)))
      (and pos (goto-char pos)))))

(define-minor-mode racket-check-syntax-mode
  "Analyze the buffer and annotate with information.

The buffer becomes read-only until you exit this minor mode.
However you may navigate the usual ways. When point is on a
definition or use, related items are highlighted and
information is displayed in the echo area. You may also use
special commands to navigate among the definition and its uses.

```
\\{racket-check-syntax-mode-map}
```
"
  :lighter " CheckSyntax"
  :keymap (racket--easy-keymap-define
           '(("q"               racket-check-syntax-mode-quit)
             ("h"               racket-check-syntax-mode-help)
             (("j" "TAB")       racket-check-syntax-mode-goto-next-def)
             (("k" "<backtab>") racket-check-syntax-mode-goto-prev-def)
             ("."               racket-check-syntax-mode-goto-def)
             ("n"               racket-check-syntax-mode-goto-next-use)
             ("p"               racket-check-syntax-mode-goto-prev-use)
             ("r"               racket-check-syntax-mode-rename)))
  (unless (eq major-mode 'racket-mode)
    (setq racket-check-syntax-mode nil)
    (user-error "racket-check-syntax-mode only works with racket-mode"))
  (racket--check-syntax-stop)
  (when racket-check-syntax-mode
    (racket--check-syntax-start)))

(defvar racket--check-syntax-start-timeout 30)

(defun racket--check-syntax-start ()
  (message "Analyzing...")
  (let ((xs (let ((racket-command-timeout racket--check-syntax-start-timeout))
              (racket--repl-command "check-syntax \"%s\""
                                    (buffer-file-name)))))
    (unless xs
      (racket-check-syntax-mode 0)
      (user-error "No bindings found"))
    (unless (listp xs)
      (racket-check-syntax-mode 0)
      (error "Requires a newer version of Racket."))
    (with-silent-modifications
      (dolist (x xs)
        (pcase x
          (`(,`info ,beg ,end ,str)
           (put-text-property beg end 'help-echo str))
          (`(,`def/uses ,def-beg ,def-end ,uses)
           (add-text-properties def-beg
                                def-end
                                (list 'racket-check-syntax-def uses
                                      'point-entered #'racket--point-entered
                                      'point-left #'racket--point-left))
           (dolist (use uses)
             (pcase-let* ((`(,use-beg ,use-end) use))
               (add-text-properties use-beg
                                    use-end
                                    (list 'racket-check-syntax-use (list def-beg
                                                                         def-end)
                                          'point-entered #'racket--point-entered
                                          'point-left #'racket--point-left)))))))
      (setq buffer-read-only t)
      (racket--point-entered (point-min) (point)) ;in case already in one
      (setq header-line-format
            "Check Syntax. Buffer is read-only. Press h for help, q to quit.")
      (racket-check-syntax-mode-goto-next-def))
    (message "")))

(defun racket--check-syntax-stop ()
  (setq header-line-format nil)
  (with-silent-modifications
    (remove-text-properties (point-min)
                            (point-max)
                            '(help-echo nil
                              racket-check-syntax-def nil
                              racket-check-syntax-use nil
                              point-entered
                              point-left))
    (racket--unhighlight-all)
    (setq buffer-read-only nil)))


;;; align

(defun racket-align ()
  "Align values in the same column.

Useful for binding forms like `let` and `parameterize`,
conditionals like `cond` and `match`, association lists, and any
series of couples like the arguments to `hash`.

Before choosing this command, put point on the first of a series
of \"couples\". A couple is:

- A list of two or more sexprs: `[sexpr val sexpr ...]`
- Two sexprs: `sexpr val`.

Each `val` moves to the same column and is `indent-sexp'-ed (in
case it is a multi-line form).

For example with point on the `[` before `a`:

    Before             After

    (let ([a 12]       (let ([a   12]
          [bar 23])          [bar 23])
      ....)              ....)

    '([a . 12]         '([a   . 12]
      [bar . 23])        [bar . 23])

    (cond [a? #t]      (cond [a?   #t]
          [b? (f x           [b?   (f x
                 y)]                  y)]
          [else #f])         [else #f])

Or with point on the `'` before `a`:

    (list 'a 12        (list 'a   12
          'bar 23)           'bar 23)

If more than one couple is on the same line, none are aligned,
because it is unclear where the value column should be. For
example the following form will not change; `racket-align' will
display an error message:

    (let ([a 0][b 1]
          [c 2])       error; unchanged
      ....)

When a couple's sexprs start on different lines, that couple is
ignored. Other, single-line couples in the series are aligned as
usual. For example:

    (let ([foo         (let ([foo
           0]                 0]
          [bar 1]            [bar 1]
          [x 2])             [x   2])
      ....)              ....)

See also: `racket-unalign'."
  (interactive)
  (save-excursion
    (let ((listp (eq ?\( (char-syntax (char-after))))
          (prev-line 0)
          (max-col 0))
      (racket--for-each-couple listp
                               (lambda ()
                                 (setq max-col (max max-col (current-column)))
                                 (let ((this-line (line-number-at-pos)))
                                   (when (= prev-line this-line)
                                     (user-error
                                      "Can't align if any couples are on same line"))
                                   (setq prev-line this-line))))
      (racket--for-each-couple listp
                               (lambda ()
                                 (indent-to max-col)
                                 (indent-sexp))))))

(defun racket-unalign ()
  "The opposite of `racket-align'.

Effectively does M-x `just-one-space' and `indent-sexp' for each
couple's value."
  (interactive)
  (save-excursion
    (let ((listp (eq ?\( (char-syntax (char-after)))))
      (racket--for-each-couple listp
                               (lambda ()
                                 (just-one-space)
                                 (indent-sexp))))))

(defun racket--for-each-couple (listp f)
  "Move point to each value sexp of a couple, and `funcall' F.

Only call F when the couple's sexprs are on the same line.

When LISTP is true, expects couples to be `[id val]`, else `id val`."
  (save-excursion
    (condition-case ()
        (while t
          (when listp
            (down-list))
          (forward-sexp)
          (let ((line (line-number-at-pos)))
            (forward-sexp)
            (backward-sexp)
            (when (= line (line-number-at-pos))
              ;; Defensive: Backup over any prefix or punctuation
              ;; chars just in case backward-sexp didn't (although it
              ;; should have if our syntax table is correct).
              (while (memq (char-syntax (char-before)) '(?\' ?\.))
                (goto-char (1- (point))))
              (funcall f)))
          ;; On to the next couple...
          (if listp
              (up-list)
            (forward-sexp)))
      (scan-error nil))))

(provide 'racket-edit)

;; racket-edit.el ends here

;;; racket-edit.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2018 by Greg Hendershott.
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
(require 'tooltip)

(defun racket-run (&optional prefix)
  "Save and evaluate the buffer in REPL.

With one C-u prefix, uses errortrace for improved stack traces.
Otherwise follows the `racket-error-context' setting.

With two C-u prefixes, instruments code for step debugging. See
`racket-debug-mode' and the variable `racket-debuggable-files'.

If point is within a Racket `module` form, the REPL \"enters\"
that submodule (uses its language info and namespace).

When you run again, the file is evaluated from scratch -- the
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
identifiers from module languages, `require`s, and definitions.
That way, things like completion and `racket-describe' are more
likely to work while you edit the file to fix the error. If not
even the \"skeleton\" evaluation succeeds, you'll have only
identifiers provided by racket/base, until you fix the error and
run again.

Output in the `*Racket REPL*` buffer that describes a file and
position is automatically \"linkified\". Examples of such text
include:

- Racket error messages.
- `rackunit` test failure location messages.
- `print`s of `#<path>` objects.

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
  "Run with `racket-error-context' temporarily set to 'high.
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
  "This is `racket-run' followed by `racket-switch-to-repl'."
  (interactive "P")
  (racket-run prefix)
  (racket-repl))

(defun racket-racket ()
  "Do `racket <file>` in `*shell*` buffer."
  (interactive)
  (racket--shell (concat (shell-quote-argument racket-program)
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
  (let ((mod-path (list 'submod (racket--buffer-file-name) 'test))
        (buf (current-buffer)))
    (if (not coverage)
        (racket--repl-run mod-path)
      (message "Running test submodule with coverage instrumentation...")
      (racket--repl-run
       mod-path
       'coverage
       (lambda (_n/a)
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

(defun racket-raco-test ()
  "Do `raco test -x <file>` in `*shell*` buffer.
To run <file>'s `test` submodule."
  (interactive)
  (racket--shell (concat (shell-quote-argument racket-program)
                         " -l raco test -x "
                         (shell-quote-argument (racket--buffer-file-name)))))

(defun racket--shell (cmd)
  (racket--save-if-changed)
  (let ((w (selected-window)))
    (pcase (get-buffer-window "*shell*" t)
      (`() (other-window -1))
      (win (select-window win)))
    (with-temp-message cmd
      (shell)
      (pop-to-buffer-same-window "*shell*")
      (comint-send-string "*shell*" (concat cmd "\n"))
      (select-window w)
      (sit-for 3))))


;;; visiting defs and mods

(defun racket-visit-definition (&optional prefix)
  "Visit definition of symbol at point.

Use \\[racket-unvisit] to return.

Please keep in mind the following limitations:

- Only finds symbols defined in the current namespace. You may
  need to `racket-run' the current buffer, first.

- Only visits the definition of module-level identifiers --
  things for which Racket's `identifier-binding` function returns
  information. This does _not_ include things such as
  local (nested) function definitions or `racket/class` member
  functions. To find those in the same file, you'll need to use a
  normal Emacs text search function like `isearch-forward'.

- If the definition is found in Racket's `#%kernel` module, it
  will tell you so but won't visit the definition site."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Visit definition of: ")
    (`nil nil)
    (str (if (and (eq major-mode 'racket-mode)
                  (not (equal (racket--repl-file-name+md5)
                              (cons (racket--buffer-file-name t) (md5 (current-buffer)))))
                  (y-or-n-p "Run current buffer first? "))
             (racket--repl-run nil nil
                               (lambda (_n/a)
                                 (racket--do-visit-def-or-mod 'def str)))
           (racket--do-visit-def-or-mod 'def str)))))

(defun racket-visit-module (&optional prefix)
  "Visit definition of module at point, e.g. net/url or \"file.rkt\".

Use \\[racket-unvisit] to return.

See also: `racket-find-collection'."
  (interactive "P")
  (let* ((v (racket--module-at-point))
         (v (if (or prefix (not v))
                (read-from-minibuffer "Visit module: " (or v ""))
              v)))
    ;; If the module name is quoted e.g. "file.rkt", just do
    ;; equivalent of `find-file-at-point'. Else ask back-end.
    (cond ((and (equal "\"" (substring v 0 1))
                (equal "\"" (substring v -1 nil)))
           (racket--push-loc)
           (find-file (expand-file-name (substring v 1 -1)))
           (message "Type M-, to return"))
          (t (racket--do-visit-def-or-mod 'mod v)))))

(defun racket--module-at-point ()
  "Treat point as a Racket module path name, possibly in a multi-in form."
  ;; `thing-at-point' 'filename matches both net/url and "file.rkt".
  ;; But. 1. Returns both without the quotes; use `syntax-ppss' to
  ;; detect latter (string). 2. Returns nil if on the opening quote;
  ;; use `forward-char' then.
  (save-excursion
    (when (eq ?\" (char-syntax (char-after))) ;2
      (forward-char))
    (pcase (racket--thing-at-point 'filename t)
      (`() `())
      (v
       (let* ((ppss       (syntax-ppss))
              (relative-p (and (racket--ppss-string-p ppss) t)) ;1
              (multi-in   (condition-case ()
                              (progn
                                (when relative-p
                                  (goto-char (racket--ppss-string/comment-start ppss)))
                                (backward-up-list 1)
                                (backward-sexp 2)
                                (when (looking-at-p "multi-in")
                                  (forward-sexp 2)
                                  (backward-sexp 1)
                                  (when (eq ?\" (char-syntax (char-after))) ;2
                                    (forward-char))
                                  (unless (eq relative-p
                                              (and (racket--ppss-string-p ppss) t)) ;1
                                    (user-error "multi-in mixes absolute and relative paths"))
                                  (racket--thing-at-point 'filename t)))
                            (scan-error nil))))
         (concat (if relative-p "\"" "") ;1
                 (if multi-in
                     (concat multi-in "/")
                   "")
                 v
                 (if relative-p "\"" ""))))))) ;1

(defun racket--do-visit-def-or-mod (cmd str)
  "CMD must be 'def or 'mod. STR must be `stringp`."
  (unless (memq major-mode '(racket-mode racket-repl-mode racket-describe-mode))
    (user-error "That doesn't work in %s" major-mode))
  (pcase (racket--cmd/await (list cmd str))
    (`(,path ,line ,col)
     (racket--push-loc)
     (find-file (funcall racket-path-from-racket-to-emacs-function path))
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char col)
     (message "Type M-, to return"))
    (`kernel
     (message "`%s' defined in #%%kernel -- source not available." str))
    (_
     (message "Not found."))))

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
  (pcase (racket--symbol-at-point-or-prompt prefix "Racket help for: ")
    (`nil nil)
    (str (racket--cmd/async `(doc ,str)))))


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
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
  (pcase (racket--top-level-requires 'find)
    (`nil (user-error "The file module has no requires; nothing to do"))
    (reqs (pcase (racket--cmd/await `(requires/tidy ,reqs))
            ("" nil)
            (new (goto-char (racket--top-level-requires 'kill))
                 (insert (concat new "\n")))))))

(defun racket-trim-requires ()
  "Like `racket-tidy-requires' but also deletes unnecessary requires.

Note: This only works when the source file can be evaluated with
no errors.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms. Furthermore, it is not smart about `module+` or
`module*` forms -- it may delete top level requires that are
actually needed by such submodules.

See also: `racket-base-requires'."
  (interactive)
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
  (when (racket--ok-with-module+*)
   (racket--save-if-changed)
   (pcase (racket--top-level-requires 'find)
     (`nil (user-error "The file module has no requires; nothing to do"))
     (reqs (pcase (racket--cmd/await `(requires/trim
                                       ,(racket--buffer-file-name)
                                       ,reqs))
             (`nil (user-error "Syntax error in source file"))
             (""   (goto-char (racket--top-level-requires 'kill)))
             (new  (goto-char (racket--top-level-requires 'kill))
                   (insert (concat new "\n"))))))))

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
`module` forms. Furthermore, it is not smart about `module+` or
`module*` forms -- it may delete top level requires that are
actually needed by such submodules.

Note: Currently this only helps change `#lang racket` to
`#lang racket/base`. It does *not* help with other similar conversions,
such as changing `#lang typed/racket` to `#lang typed/racket/base`."
  (interactive)
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
  (when (racket--buffer-start-re "^#lang.*? racket/base$")
    (user-error "Already using #lang racket/base. Nothing to change."))
  (unless (racket--buffer-start-re "^#lang.*? racket$")
    (user-error "File does not use use #lang racket. Cannot change."))
  (when (racket--ok-with-module+*)
    (racket--save-if-changed)
    (let ((reqs (racket--top-level-requires 'find)))
      (pcase (racket--cmd/await `(requires/base
                                  ,(racket--buffer-file-name)
                                  ,reqs))
        (`nil (user-error "Syntax error in source file"))
        (new (goto-char (point-min))
             (re-search-forward "^#lang.*? racket$")
             (insert "/base")
             (goto-char (or (racket--top-level-requires 'kill)
                            (progn (insert "\n\n") (point))))
             (unless (string= "" new)
               (insert (concat new "\n"))))))))

(defun racket--ok-with-module+* ()
  (save-excursion
    (goto-char (point-min))
    (or (not (re-search-forward (rx ?\( "module" (or "+" "*")) nil t))
        (prog1
            (y-or-n-p "Analysis will be unreliable due to module+ or module* forms -- proceed anyway? ")
          (message "")))))

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

(defun racket--non-empty-string-p (v)
  (and (stringp v)
       (not (string-match-p "\\`[ \t\n\r]*\\'" v)))) ;`string-blank-p'

(defun racket--point-entered (_old new)
  (pcase (get-text-property new 'help-echo)
    ((and s (pred racket--non-empty-string-p))
     (if (and (boundp 'tooltip-mode)
              tooltip-mode
              (fboundp 'window-absolute-pixel-position))
         (pcase (window-absolute-pixel-position new)
           (`(,left . ,top)
            (let ((tooltip-frame-parameters `((left . ,left)
                                              (top . ,top)
                                              ,@tooltip-frame-parameters)))
              (tooltip-show s))))
       (message "%s" s))))
  (pcase (get-text-property new 'racket-check-syntax-def)
    ((and uses `((,beg ,_end) . ,_))
     (pcase (get-text-property beg 'racket-check-syntax-use)
       (`(,beg ,end) (racket--highlight beg end t)))
     (dolist (use uses)
       (pcase use (`(,beg ,end) (racket--highlight beg end nil))))))
  (pcase (get-text-property new 'racket-check-syntax-use)
    (`(,beg ,end)
     (racket--highlight beg end t)
     (dolist (use (get-text-property beg 'racket-check-syntax-def))
       (pcase use (`(,beg ,end) (racket--highlight beg end nil)))))))

(defun racket--point-left (_old _new)
  (racket--unhighlight-all))

(defun racket-check-syntax-mode-quit ()
  (interactive)
  (racket-check-syntax-mode -1))

(defun racket-check-syntax-mode-goto-def ()
  "When point is on a use, go to its definition."
  (interactive)
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,_end) (goto-char beg))))

(defun racket-check-syntax-mode-forward-use (amt)
  "When point is on a use, go AMT uses forward. AMT may be negative.

Moving before/after the first/last use wraps around.

If point is instead on a definition, then go to its first use."
  (pcase (get-text-property (point) 'racket-check-syntax-use)
    (`(,beg ,_end)
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
         (`((,beg ,_end) . ,_) (goto-char beg))))))

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

(defun racket--check-syntax-start ()
  (let ((buf (current-buffer)))
    (racket--save-if-changed)
    (message "Running check-syntax analysis...")
    (racket--cmd/async-raw
     `(check-syntax ,(racket--buffer-file-name))
     (lambda (response)
       (with-current-buffer buf
        (pcase response
          (`(error ,m)
           (racket-check-syntax-mode -1)
           (error m))
          (`(ok ())
           (racket-check-syntax-mode -1)
           (user-error "No bindings found"))
          (`(ok ,xs)
           (message "Marking up buffer...")
           (racket--check-syntax-insert xs)
           (message ""))))))))

(defun racket--check-syntax-insert (xs)
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
                                    'point-left    #'racket--point-left))
         (dolist (use uses)
           (pcase-let* ((`(,use-beg ,use-end) use))
             (add-text-properties use-beg
                                  use-end
                                  (list 'racket-check-syntax-use (list def-beg
                                                                       def-end)
                                        'point-entered #'racket--point-entered
                                        'point-left    #'racket--point-left)))))))
    (setq buffer-read-only t)
    (setq header-line-format
          "Check Syntax. Buffer is read-only. Press h for help, q to quit.")
    ;; Make 'point-entered and 'point-left work in Emacs 25+. Note
    ;; that this is somewhat of a hack -- I spent a lot of time trying
    ;; to Do the Right Thing using the new cursor-sensor-mode, but
    ;; could not get it to work satisfactorily. See:
    ;; http://emacs.stackexchange.com/questions/29813/point-motion-strategy-for-emacs-25-and-older
    (setq-local inhibit-point-motion-hooks nil)
    ;; Go to next definition, as an affordance/hint what this does:
    (racket-check-syntax-mode-goto-next-def)))

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

Each `val` moves to the same column and is
`prog-indent-sexp'-ed (in case it is a multi-line form).

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
                                 (prog-indent-sexp))))))

(defun racket-unalign ()
  "The opposite of `racket-align'.

Effectively does M-x `just-one-space' and `prog-indent-sexp' for
each couple's value."
  (interactive)
  (save-excursion
    (let ((listp (eq ?\( (char-syntax (char-after)))))
      (racket--for-each-couple listp
                               (lambda ()
                                 (just-one-space)
                                 (prog-indent-sexp))))))

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

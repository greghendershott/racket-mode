;;; racket-edit.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;; racket-mode per se, i.e. the mode for .rkt file buffers

(require 'cl-lib)
(require 'cl-macs)
(require 'comint)
(require 'racket-custom)
(require 'racket-cmd)
(require 'racket-common)
(require 'racket-complete)
(require 'racket-repl)
(require 'racket-util)
(require 'hideshow)
(require 'xref)

;;; Show/hide test submodules

(defun racket--fold-tests (fold-p)
  ;; For this to work in `racket-hash-lang-mode' for all hash-langs,
  ;; (a) we'd need to learn the test submodule spans from analysis of
  ;; fully-expanded code (as we can do on the `pdb` branch with a
  ;; sufficiently new Racket). And then, (b) we'd need to do the
  ;; hiding ourselves, without `hs-minor-mode', which AFAICT demands
  ;; regexps for block starts and ends. We'd want a "positional" not
  ;; regexp flavor, which AFAIK we'd need to implement ourselves.
  ;;
  ;; TL;DR: For now require `racket-sexp-edit-mode'.
  (unless hs-minor-mode
    (hs-minor-mode))
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^(module[+*]? test" (point-max) t)
        (if fold-p (hs-hide-block) (hs-show-block))
        (cl-incf n)
        (goto-char (match-end 0)))
      (message "%s %d test submodules" (if fold-p "Folded" "Unfolded") n))))

(defun racket-fold-all-tests ()
  "Fold (hide) all test submodules."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (racket--fold-tests t))

(defun racket-unfold-all-tests ()
  "Unfold (show) all test submodules."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (racket--fold-tests nil))

;;; requires

(defun racket-tidy-requires ()
  "Make a single \"require\" form, modules sorted, one per line.

The scope of this command is the innermost module around point,
including the outermost module for a file using a \"#lang\" line.
All require forms within that module are combined into a single
form. Within that form:

- A single subform is used for each phase level, sorted in this
  order: for-syntax, for-template, for-label, for-meta, and
  plain (phase 0).

  - Within each level subform, the modules are sorted:

    - Collection path modules -- sorted alphabetically.

    - Subforms such as only-in.

    - Quoted relative requires -- sorted alphabetically.

At most one required module is listed per line.

See also: `racket-trim-requires' and `racket-base-requires'."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (racket--tidy-requires '() #'ignore))

(defun racket--tidy-requires (add callback)
  (pcase (append (racket--module-requires 'find) add)
    (`() (user-error "The module has no requires; nothing to do"))
    (reqs (racket--cmd/async
           nil
           `(requires/tidy ,reqs)
           (lambda (result)
             (pcase result
               ("" nil)
               (new
                (pcase (racket--module-requires 'kill)
                  (`()
                   (goto-char (racket--inside-innermost-module))
                   (forward-line 1))
                  (pos (goto-char pos)))
                (let ((pt (point)))
                  (insert new)
                  (when (eq (char-before pt) ?\n)
                    (newline))
                  (indent-region pt (1+ (point)))
                  (goto-char pt))))
             (funcall callback result))))))

(defun racket-trim-requires ()
  "Like `racket-tidy-requires' but also deletes unnecessary requires.

Note: This only works when the source file can be fully expanded
with no errors.

Note: This only works for requires at the top level of a source
file using #lang. It does NOT work for require forms inside
module forms. Furthermore, it is not smart about module+ or
module* forms -- it might delete top level requires that are
actually needed by such submodules.

See also: `racket-base-requires'."
  (interactive)
  (racket--assert-edit-mode)
  (when (racket--submodule-y-or-n-p)
   (racket--save-if-changed)
   (pcase (racket--module-requires 'find t)
     (`nil (user-error "The file module has no requires; nothing to do"))
     (reqs (racket--cmd/async
            nil
            `(requires/trim
              ,(racket--buffer-file-name)
              ,reqs)
            (lambda (result)
              (pcase result
                (`nil (user-error "Syntax error in source file"))
                (""   (goto-char (racket--module-requires 'kill t)))
                (new  (goto-char (racket--module-requires 'kill t))
                      (insert (concat new "\n"))))))))))

(defun racket-base-requires ()
  "Change from \"#lang racket\" to \"#lang racket/base\".

Adds explicit requires for imports that are provided by
\"racket\" but not by \"racket/base\".

This is a recommended optimization for Racket applications.
Avoiding loading all of \"racket\" can reduce load time and
memory footprint.

Also, as does `racket-trim-requires', this removes unneeded
modules and tidies everything into a single, sorted require form.

Note: This only works when the source file can be fully expanded
with no errors.

Note: This only works for requires at the top level of a source
file using #lang. It does NOT work for require forms inside
module forms. Furthermore, it is not smart about module+ or
module* forms -- it might delete top level requires that are
actually needed by such submodules.

Note: Currently this only helps change \"#lang racket\" to
\"#lang racket/base\". It does not help with other similar
conversions, such as changing \"#lang typed/racket\" to \"#lang
typed/racket/base\"."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (when (racket--buffer-start-re "^#lang.*? racket/base$")
    (user-error "Already using #lang racket/base. Nothing to change."))
  (unless (racket--buffer-start-re "^#lang.*? racket$")
    (user-error "File does not use use #lang racket. Cannot change."))
  (when (racket--submodule-y-or-n-p)
    (racket--save-if-changed)
    (let ((reqs (racket--module-requires 'find t)))
      (racket--cmd/async
       nil
       `(requires/base
         ,(racket--buffer-file-name)
         ,reqs)
       (lambda (result)
         (pcase result
           (`nil (user-error "Syntax error in source file"))
           (new (goto-char (point-min))
                (re-search-forward "^#lang.*? racket$")
                (insert "/base")
                (goto-char (or (racket--module-requires 'kill t)
                               (progn (insert "\n\n") (point))))
                (unless (string= "" new)
                  (insert (concat new "\n"))))))))))

(defun racket--submodule-y-or-n-p ()
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

(defun racket--module-requires (what &optional outermost-p)
  "Identify all require forms and do WHAT.

When WHAT is \"find\", return the require forms.

When WHAT is \"kill\", kill the require forms and return the
position where the first one had started.

OUTERMOST-P says which module's requires: true means the
outermost file module, nil means the innermost module around
point."
  (save-excursion
    (goto-char (if outermost-p
                   (point-min)
                 (racket--inside-innermost-module)))
    (let ((first-beg nil)
          (requires nil))
      (while
          (condition-case _
              (let ((end (progn (forward-sexp  1) (point)))
                    (beg (progn (forward-sexp -1) (point))))
                (unless (equal end (point-max))
                  (when (prog1 (racket--looking-at-require-form)
                          (goto-char end))
                    (unless first-beg (setq first-beg beg))
                    (push (read (buffer-substring-no-properties beg end))
                          requires)
                    (when (eq 'kill what)
                      (delete-region beg end)
                      (delete-blank-lines)))
                  t))
            (scan-error nil)))
      (if (eq 'kill what) first-beg requires))))

(defun racket--inside-innermost-module ()
  "Position of the start of the inside of the innermost module
around point. This could be \"(point-min)\" if point is within no
module form, meaning the outermost, file module."
  (save-excursion
    (racket--escape-string-or-comment)
    (condition-case _
        (progn
          (while (not (racket--looking-at-module-form))
            (backward-up-list))
          (down-list)
          (point))
      (scan-error (point-min)))))

(defun racket--looking-at-require-form ()
  ;; Assumes you navigated to point using a method that ignores
  ;; strings and comments, preferably `forward-sexp'.
  (and (eq ?\( (char-syntax (char-after)))
       (save-excursion
         (down-list 1)
         (looking-at-p "require"))))

(defun racket-add-require-for-identifier ()
  "Add a require for an identifier.

Useful when you know the name of an export but don't remember
from what module it is exported.

At the prompt:
\\<minibuffer-local-map>

Use \\[next-history-element] to load the identifier at point.
You may also need to \\[move-end-of-line] to see candidates.

Or type anything.

After you choose:

The identifier you chose is inserted at point if not already
there.

A \"require\" form is inserted, followed by doing a
`racket-tidy-requires'.

When more than one module supplies an identifer with the same
name, the first is used -- for example \"racket/base\" instead of
\"racket\".

Caveat: This works in terms of identifiers that are documented.
The mechanism is similar to that used for Racket's \"Search
Manuals\" feature. Today there exists no system-wide database of
identifiers that are exported but not documented."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (when-let (result (racket--describe-search-completing-read))
    (pcase-let* ((`(,term ,_path ,_anchor ,lib) result)
                 (req `(require ,(intern lib))))
      (unless (equal (racket--thing-at-point 'symbol) term)
        (insert term))
      (let ((pt  (copy-marker (point))))
        (racket--tidy-requires
         (list req)
         (lambda (result)
           (goto-char pt)
           (when result
             (message "Added \"%s\" and did racket-tidy-requires" req))))))))

;;; align

(defun racket-align ()
  "Align values in the same column.

Useful for binding forms like \"let\" and \"parameterize\",
conditionals like \"cond\" and \"match\", association lists, and
any series of couples like the arguments to \"hash\".

Before choosing this command, put point on the first of a series
of \"couples\". A couple is:

- A list of two or more sexprs: \"[sexpr val sexpr ...]\".
- Two sexprs: \"sexpr val\".

Each \"val\" moves to the same column and is
`prog-indent-sexp'-ed (in case it is a multi-line form).

For example with point on the \"[\" before \"a\":

#+BEGIN_SRC racket
    Before             After

    (let ([a 12]       (let ([a   12]
          [bar 23])          [bar 23])
      ....)              ....)

    ([a . 12]          ([a   . 12]
     [bar . 23])        [bar . 23])

    (cond [a? #t]      (cond [a?   #t]
          [b? (f x           [b?   (f x
                 y)]                  y)]
          [else #f])         [else #f])
#+END_SRC

Or with point on the quote before \"a\":

#+BEGIN_SRC racket
    (list a 12        (list a   12
          bar 23)           bar 23)
#+END_SRC

If more than one couple is on the same line, none are aligned,
because it is unclear where the value column should be. For
example the following form will not change; `racket-align' will
display an error message:

#+BEGIN_SRC racket
    (let ([a 0][b 1]
          [c 2])       error; unchanged
      ....)
#+END_SRC

When a couple's sexprs start on different lines, that couple is
ignored. Other, single-line couples in the series are aligned as
usual. For example:

#+BEGIN_SRC racket
    (let ([foo         (let ([foo
           0]                 0]
          [bar 1]            [bar 1]
          [x 2])             [x   2])
      ....)              ....)
#+END_SRC

See also: `racket-unalign'."
  (interactive)
  (racket--assert-sexp-edit-mode)
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
  (racket--assert-sexp-edit-mode)
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
    (condition-case _
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

;;; Completion

(defconst racket--completion-candidates
  (seq-sort #'string-lessp
            (seq-reduce (lambda (accum xs)
                          (append accum xs))
                        (list racket-type-list
                              racket-keywords
                              racket-builtins-1-of-2
                              racket-builtins-2-of-2)
                        nil)))

(defun racket-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

Completion candidates are drawn from the same symbols used for
font-lock. This is a static list. If you want dynamic, smarter
completion candidates, enable the minor mode `racket-xp-mode'."
  (racket--call-with-completion-prefix-positions
   (lambda (beg end)
     (list beg
           end
           (racket--completion-table racket--completion-candidates)
           :predicate #'identity
           :exclusive 'no))))

;;; lispy

;; <https://github.com/abo-abo/lispy/blob/master/le-racket.el> expects
;; this in 'racket-edit
(define-obsolete-function-alias 'racket-lispy-visit-symbol-definition
  #'xref-find-definitions "2020-11"
  "Function called by lispy.el's `lispy-goto-symbol' for Racket.")

(provide 'racket-edit)

;; racket-edit.el ends here

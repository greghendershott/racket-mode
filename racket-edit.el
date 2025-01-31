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
  "Make a single, sorted \"require\" form for each module.

Use a single require-spec for each phase-level, sorted in this
order: for-syntax, for-template, for-label, for-meta, and
plain (phase 0).

Within each phase-level, sort require-specs by module name.

Format at most one module per line.

Simplify gratuitous require-specs. For example reduce (only-in m)
to m and elide (combine-in).

See also: `racket-trim-requires' and `racket-base-requires'."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (racket--tidy-requires))

(defun racket--tidy-requires (&optional callback)
  "Helper for both the `racket-tidy-requires' and
`racket-add-require-for-identifier' commands."
  (racket--save-if-changed)
  (racket--cmd/async
   nil
   `(requires/tidy ,(racket--buffer-file-name))
   (lambda (changes)
     (racket--require-changes changes)
     (when callback
       (funcall callback changes)))))

(defun racket-trim-requires ()
  "Like `racket-tidy-requires' but also delete unnecessary requires.

Use macro-debugger/analysis/check-requires to analyze.

The analysis:

- Needs the `macro-debugger-lib` package.

- Only works when the source file can be fully expanded with no
errors.

- Only works for requires at the top level of a source file using
#lang -- not for requires inside submodule forms. Furthermore,
the analysis is not smart about module+ or module* forms -- it
might delete outer requires that are actually needed by such
submodules.

See also: `racket-base-requires'."
  (interactive)
  (racket--assert-edit-mode)
  (when (racket--submodule-y-or-n-p)
   (racket--save-if-changed)
   (racket--cmd/async
    nil
    `(requires/trim ,(racket--buffer-file-name))
    #'racket--require-changes)))

(defun racket-base-requires ()
  "Change from \"#lang racket\" to \"#lang racket/base\".

Using \"racket/base\" is a recommended optimization for Racket
applications. Loading all of \"racket\" is slower and uses more
memory.

Add explicit requires for imports that are provided by \"racket\"
but not by \"racket/base\".

Also do the equivalent of `racket-trim-requires' and
`racket-tidy-require'. See those commands for additional notes
and caveats.

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
    (racket--cmd/async
     nil
     `(requires/base ,(racket--buffer-file-name))
     (lambda (changes)
       (racket--require-changes changes)
       (goto-char (point-min))
       (re-search-forward "^#lang.*? racket$")
       (insert "/base")))))

(defun racket--require-changes (changes)
  "Process response from back end tidy/trim/base commands.

Each change is either a deletion, a replacement, or an insertion.

The changes are sorted from greater to smaller positions -- so
that by working backwards through the buffer, we need not worry
about shifting positions of later items.

The biggest wrinkle here is that, for esthetics, we want to
remove surrounding whitepsace when deleting, or add when
inserting something brand-new. Otherwise, for replacing, it
suffices to make the change and re-indent."
  (save-match-data
    (dolist (change changes)
      (pcase change
        (`(delete ,pos ,span)
         (delete-region pos (+ pos span))
         (save-excursion
           (goto-char pos)
           (if (save-excursion
                 (forward-line 0)
                 (looking-at "[ \t]*)"))
               (delete-indentation) ;i.e. join-line
             (delete-blank-lines))))
        (`(replace ,pos ,span ,str)
         (delete-region pos (+ pos span))
         (save-excursion
           (goto-char pos)
           (insert str)
           (indent-region pos (point))))
        (`(insert ,pos ,str)
         (save-excursion
           (goto-char pos)
           (newline-and-indent)
           (insert str)
           (newline-and-indent)
           (indent-region pos (point))))))))

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
          (forward-line 1)
          (re-search-forward "[ \t]*" nil t)
          (point))
      (scan-error
       (goto-char (point-min))
       (or (re-search-forward "^#lang .+?\n$" nil t)
           (point-min))))))

(defun racket-add-require-for-identifier ()
  "Add a require for an identifier.

Useful when you know the name of an export but don't remember
from what module it is exported.

After you choose an identifier, this command will:

- Insert the identifier at point if not already there.

- Insert a \"require\" form and do `racket-tidy-requires'.

Caveat: This works only for identifiers that are documented. The
mechanism is similar to that used for Racket's \"Search Manuals\"
feature. Today there exists no system-wide database of
identifiers that are exported but not documented."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (when-let (result (racket--describe-search-completing-read))
    (pcase-let* ((`(,term ,_path ,_anchor ,lib) result)
                 (req (format "(require %s)" lib))
                 (pt (copy-marker (point) t)))
      (unless (equal (racket--thing-at-point 'symbol) term)
        (insert term))
      (goto-char (racket--inside-innermost-module))
      (newline-and-indent)
      (insert req)
      (newline-and-indent)
      (racket--tidy-requires
       (lambda (result)
         (goto-char pt)
         (when result
           (message "Added %S and did racket-tidy-requires" req)))))))

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

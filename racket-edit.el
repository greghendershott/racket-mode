;;; racket-edit.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
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

;; racket-mode per se, i.e. the mode for .rkt file buffers

(require 'cl-lib)
(require 'cl-macs)
(require 'comint)
(require 'ido)
(require 'racket-custom)
(require 'racket-cmd)
(require 'racket-common)
(require 'racket-complete)
(require 'racket-repl)
(require 'racket-util)
(require 'hideshow)
(require 'xref)

(defun racket-racket ()
  "Do \"racket <file>\" in a shell buffer."
  (interactive)
  (racket--shell (concat (shell-quote-argument racket-program)
                         " "
                         (shell-quote-argument (racket--buffer-file-name)))))

(defun racket-raco-test ()
  "Do \"raco test -x <file>\" in a shell buffer to run the \"test\" submodule."
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
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
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
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
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
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
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

When WHAT is 'find, return the require forms.

When WHAT is 'kill, kill the require forms and return the
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
          (condition-case nil
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
    (condition-case ()
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
         (looking-at "require"))))

(defun racket-add-require-for-identifier ()
  "Add a require for the identifier at point.

When more than one module supplies an identifer with the same
name, they are listed for you to choose one. The list is sorted
alphabetically, except modules starting with \"racket/\" and
\"typed/racket/\" are sorted before others. While at the prompt,
as a convenience you can press C-h to see the \"Search Manuals\"
page for locally installed packages -- effectively like
doing \"raco doc\" at the command line.

A \"require\" form is inserted into the buffer, followed by doing
a `racket-tidy-requires'.

Caveat: This works in terms of identifers that are documented.
The mechanism is similar to that used for Racket's \"Search
Manuals\" feature. Today there exists no system-wide database of
identifiers that are exported but not documented."
  (interactive)
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
  (let ((sym-at-point (thing-at-point 'symbol t)))
    (unless sym-at-point
      (user-error "There does not seem to be an identifier at point"))
    (racket--cmd/async
     nil
     `(requires/find ,sym-at-point)
     (lambda (result)
       (let ((lib
              (pcase result
                (`()
                 (message "\"%s\" is not a documented export of any installed library"
                          sym-at-point)
                 nil)
                (`(,lib)
                 lib)
                (libs
                 (let* ((map (prog1 (make-sparse-keymap)))
                        (_   (set-keymap-parent map ido-completion-map))
                        (_   (define-key map "\C-h"
                               (lambda ()
                                 (interactive)
                                 (racket--search-doc-locally sym-at-point))))
                        (overriding-local-map map))
                   (ido-completing-read
                    (format "\"%s\" is provided by multiple libraries, choose one (C-h to search manuals): "
                            sym-at-point)
                    libs))))))
         (when lib
           (let ((pt  (copy-marker (point)))
                 (req `(require ,(intern lib))))
             (racket--tidy-requires
              (list req)
              (lambda (result)
                (goto-char pt)
                (when result
                  (message "Added \"%s\" and did racket-tidy-requires" req)))))))))))

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

    '([a . 12]         '([a   . 12]
      [bar . 23])        [bar . 23])

    (cond [a? #t]      (cond [a?   #t]
          [b? (f x           [b?   (f x
                 y)]                  y)]
          [else #f])         [else #f])
#+END_SRC

Or with point on the quote before \"a\":

#+BEGIN_SRC racket
    (list 'a 12        (list 'a   12
          'bar 23)           'bar 23)
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

;;; Completion

(defvar racket--completion-candidates (list racket-type-list
                                            racket-keywords
                                            racket-builtins-1-of-2
                                            racket-builtins-2-of-2))

(defun racket--completion-candidates-for-prefix (prefix)
  (cl-reduce (lambda (results strs)
               (append results (all-completions prefix strs)))
             racket--completion-candidates
             :initial-value ()))

(defun racket-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

Completion candidates are drawn from the same symbols used for
font-lock. This is a static list. If you want dynamic, smarter
completion candidates, enable the minor mode `racket-xp-mode'."
  (racket--call-with-completion-prefix-positions
   (lambda (beg end)
     (list beg
           end
           (completion-table-dynamic
            #'racket--completion-candidates-for-prefix)
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

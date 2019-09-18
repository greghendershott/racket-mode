;;; racket-parens.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2019 by Greg Hendershott.
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

;; Things related to parens, paredit, electric-pair-mode

(require 'racket-custom)
(require 'racket-ppss)
(require 'racket-util)

;;; racket--self-insert

(defun racket--self-insert (event)
  "Simulate a `self-insert-command' of EVENT.

Using this intead of `insert' allows self-insert hooks to run,
which is important for things like `'electric-pair-mode'.

A command using this should probably set its 'delete-selection
property to t so that `delete-selection-mode' works:

  (put 'racket-command 'delete-selection t)

If necessary the value of the property can be a function, for
example `racket--electric-pair-mode-not-active'."
  (let ((last-command-event event))     ;set this for hooks
    (self-insert-command (prefix-numeric-value nil))))

(defun racket--electric-pair-mode-not-active ()
  "A suitable value for the 'delete-selection property of
commands that insert parens: Inserted text should replace the
selection unless a mode like `electric-pair-mode' is enabled, in
which case the selection is to be wrapped in parens."
  (not (and (boundp 'electric-pair-mode)
            electric-pair-mode)))


;;; Automatically insert matching \?) \?] or \?}

(defconst racket--matching-parens
  '(( ?\( . ?\) )
    ( ?\[ . ?\] )
    ( ?\{ . ?\} )))

(defun racket-insert-closing (&optional prefix)
  "Insert a matching closing delimiter.

With a prefix, insert the typed character as-is.

If you want to use this, in your Emacs init file you can bind
\")\", \"]\", and \"}\" keys to `racket-insert-closing'.

This is handy if you're not yet using something like
`paredit-mode', `smartparens-mode', `parinfer-mode', or simply
`electric-pair-mode' added in Emacs 24.5."
  (interactive "P")
  (let* ((do-it (not (or prefix
                         (and (string= "#\\"
                                       (buffer-substring-no-properties
                                        (- (point) 2) (point) )))
                         (racket--ppss-string-p (syntax-ppss)))))
         (open-char  (and do-it        (racket--open-paren #'backward-up-list)))
         (close-pair (and open-char    (assq open-char racket--matching-parens)))
         (close-char (and close-pair   (cdr close-pair))))
    (racket--self-insert (or close-char last-command-event))))

(put 'racket-insert-closing 'delete-selection
     #'racket--electric-pair-mode-not-active)


;;; Smart open bracket

(defconst racket--smart-open-bracket-data
  (eval-when-compile
    `(;; cond-like
      (0 0 ,(rx (seq "("
                     (or "augment"
                         "augment-final"
                         "augride"
                         "cond"
                         "field"
                         "inherit"
                         "inherit-field"
                         "inherit/super"
                         "inherit/inner"
                         "init"
                         "init-field"
                         "match-lambda"
                         "match-lambda*"
                         "match-lambda**"
                         "overment"
                         "override"
                         "override-final"
                         "public"
                         "pubment"
                         "public-final"
                         "rename-inner"
                         "rename-super"
                         "super-new")
                     (or space line-end))))
      ;; case-like
      (2 0 ,(rx (seq "("
                     (or "case"
                         "new"
                         "match"
                         "match*"
                         "syntax-parse"
                         "syntax-rules")
                     (or space line-end))))
      ;; syntax-case
      (3 0 ,(rx (seq "("
                     (or "syntax-case")
                     (or space line-end))))
      ;; syntax-case*
      (4 0 ,(rx (seq "("
                     (or "syntax-case*")
                     (or space line-end))))
      ;; let-like
      ;;
      ;; In addition to the obvious suspects with 'let' in the name,
      ;; handles forms like 'parameterize', 'with-handlers', 'for',
      ;; and 'for/fold' accumulator bindings.
      (0 1 ,(rx (seq (or "for"
                         "for/list"
                         "for/vector"
                         "for/hash"
                         "for/hasheq"
                         "for/hasheqv"
                         "for/and"
                         "for/or"
                         "for/lists"
                         "for/first"
                         "for/last"
                         "for/fold"
                         "for/flvector"
                         "for/extflvector"
                         "for/set"
                         "for/sum"
                         "for/product"
                         "for*"
                         "for*/list"
                         "for*/vector"
                         "for*/hash"
                         "for*/hasheq"
                         "for*/hasheqv"
                         "for*/and"
                         "for*/or"
                         "for*/lists"
                         "for*/first"
                         "for*/last"
                         "for*/fold"
                         "for*/flvector"
                         "for*/extflvector"
                         "for*/set"
                         "for*/sum"
                         "for*/product"
                         "fluid-let"
                         "let"
                         "let*"
                         "let*-values"
                         "let-struct"
                         "let-syntax"
                         "let-syntaxes"
                         "let-values"
                         "let/cc"
                         "let/ec"
                         "letrec"
                         "letrec-syntax"
                         "letrec-syntaxes"
                         "letrec-syntaxes+values"
                         "letrec-values"
                         "match-let"
                         "match-let*"
                         "match-let-values"
                         "match-let*-values"
                         "match-letrec"
                         "parameterize"
                         "parameterize*"
                         "with-handlers"
                         "with-handlers*"
                         "with-syntax"
                         "with-syntax*")
                     (or space line-end))))
      ;; for/fold bindings
      ;;
      ;; Note: Previous item handles the first, accumulators subform.
      (0 2 ,(rx (seq (or "for/fold"
                         "for*/fold")
                     (or space line-end))))
      ;; named-let bindings
      ;;
      (0 2 ,(rx (seq "let" (1+ whitespace) (1+ (not (in "()[]{}\",'`;#|\" "))))))))
  "A list of lists. Each sub list is arguments to supply to
  `racket--smart-open-bracket-helper'.")

(defun racket--smart-open-bracket-helper (pre-backward-sexps
                                          post-backward-sexps
                                          regexp)
  "Is point is a subform (of a known form REGEXP) that should open with '['.

Returns '[' or nil."

  (and (save-excursion
         (ignore-errors
           (backward-sexp pre-backward-sexps) t))
       (save-excursion
         (ignore-errors
           (backward-up-list)
           (backward-sexp post-backward-sexps)
           (when (looking-at-p regexp)
             ?\[)))))

(defun racket-smart-open-bracket (&optional prefix)
  "Automatically insert a `(` or a `[` as appropriate.

Behaves like the \"Automatically adjust opening square brackets\"
feature in Dr. Racket:

By default, inserts a `(`. Inserts a `[` in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

  - `class` declaration syntax, such as `init` and `inherit`.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To use, bind the `[` key to `racket-smart-open-bracket' in
`racket-mode-map' and/or `racket-repl-mode-map'.

To force insert `[`, use `quoted-insert'.

Combined with `racket-insert-closing' this means that you can
press the unshifted `[` and `]` keys to get whatever delimiters
follow the Racket conventions for these forms. When something
like `electric-pair-mode' or `paredit-mode' is active, you need
not even press `]`."
  (interactive "P")
  (let ((ch (or (and (not racket-smart-open-bracket-enable)
                     ?\[)
                (and (save-excursion
                       (let ((pt (point)))
                         (beginning-of-defun)
                         (let ((state (parse-partial-sexp (point) pt)))
                           (or (racket--ppss-string-p state)
                               (racket--ppss-comment-p state)))))
                     ?\[)
                (cl-some (lambda (xs)
                           (apply #'racket--smart-open-bracket-helper xs))
                         racket--smart-open-bracket-data)
                (racket--open-paren #'backward-sexp)
                ?\()))
    (if (fboundp 'racket--paredit-aware-open)
        (racket--paredit-aware-open prefix ch)
      (racket--self-insert ch))))

(put 'racket-smart-open-bracket 'delete-selection
     #'racket--electric-pair-mode-not-active)

(eval-after-load 'paredit
  '(progn
     (declare-function paredit-open-round  'paredit)
     (declare-function paredit-open-square 'paredit)
     (declare-function paredit-open-curly  'paredit)
     (defun racket--paredit-aware-open (prefix ch)
       "A paredit-aware helper for `racket-smart-open-bracket'.

When `paredit-mode' is active, use its functions, such as
`paredit-open-round'. Note: This function isn't defined unless
paredit is loaded, so check for this function's existence using
`fboundp'."
       (let ((paredit-active (and (boundp 'paredit-mode) paredit-mode)))
         (cond ((not paredit-active) (racket--self-insert ch))
               ((eq ch ?\()          (paredit-open-round prefix))
               ((eq ch ?\[)          (paredit-open-square prefix))
               ((eq ch ?\{)          (paredit-open-curly prefix))
               (t                    (racket--self-insert ch)))))))

;;; paredit and reader literals

(defun racket--reader-literal-paredit-space-for-delimiter-predicate (endp _delimiter)
  "`paredit-mode' shouldn't insert space beteween # and open delimiters.

Examples: #() #2() #fl() #hasheq  etc.

This function is a suitable element for the list variable
`paredit-space-for-delimiter-predicates'."
  (if (and (racket--mode-edits-racket-p)
           (not endp))
      (not (looking-back (rx ?# (* (or (syntax word) (syntax symbol))))
                         nil))
    t))

(eval-after-load 'paredit
  '(add-hook 'paredit-space-for-delimiter-predicates
             #'racket--reader-literal-paredit-space-for-delimiter-predicate))

;;; paredit and at-expressions

(defun racket--at-expression-paredit-space-for-delimiter-predicate (endp delimiter)
  "`paredit-mode' shouldn't insert space before [ or { in Racket at-expressions.

This function is a suitable element for the list variable
`paredit-space-for-delimiter-predicates'."
  (if (and (racket--mode-edits-racket-p)
           (not endp))
      (not (or
            ;; @foo[ @foo{
            (and (memq delimiter '(?\[ ?\{))
                 (looking-back (rx ?@ (* (or (syntax word) (syntax symbol))))
                               nil))
            ;; @foo[]{
            (and (eq delimiter ?\{)
                 (looking-back (rx ?@ (* (or (syntax word) (syntax symbol)))
                                   ?\[
                                   (* (or (syntax word) (syntax symbol)))
                                   ?\])
                               nil))))
    t))

(eval-after-load 'paredit
  '(add-hook 'paredit-space-for-delimiter-predicates
             #'racket--at-expression-paredit-space-for-delimiter-predicate))


;;; Cycle paren shapes

(defconst racket--paren-shapes
  '( (?\( ?\[ ?\] )
     (?\[ ?\{ ?\} )
     (?\{ ?\( ?\) ))
  "This is not user-configurable because we expect them have to
  have actual ?\( and ?\) char syntax.")

(defun racket-cycle-paren-shapes ()
  "Cycle the sexpr among () [] {}."
  (interactive)
  (save-excursion
    (unless (eq ?\( (char-syntax (char-after)))
      (backward-up-list))
    (pcase (assq (char-after) racket--paren-shapes)
      (`(,_ ,open ,close)
       (delete-char 1)
       (insert open)
       (backward-char 1)
       (forward-sexp 1)
       (backward-delete-char 1)
       (insert close))
      (_
       (user-error "Don't know that paren shape")))))

(defun racket--open-paren (back-func)
  "Use BACK-FUNC to find an opening ( [ or { if any.
BACK-FUNC should be something like #'backward-sexp or #'backward-up-list."
  (save-excursion
    (ignore-errors
      (funcall back-func)
      (let ((ch (char-after)))
        (and (eq ?\( (char-syntax ch))
             ch)))))

(provide 'racket-parens)

;; racket-parens.el ends here

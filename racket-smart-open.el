;;; racket-smart-open.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; racket-smart-open-bracket-mode

(require 'racket-custom)
(require 'racket-parens)
(require 'racket-ppss)
(require 'racket-util)

;;;###autoload
(define-minor-mode racket-smart-open-bracket-mode
  "Minor mode to let you always type `[`' to insert `(` or `[` automatically.

Behaves like the \"Automatically adjust opening square brackets\"
feature in Dr. Racket.

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

To force insert `[`, use `quoted-insert'.

Combined with `racket-insert-closing' this means that you can
press the unshifted `[` and `]` keys to get whatever delimiters
follow the Racket conventions for these forms. When something
like `electric-pair-mode' or `paredit-mode' is active, you need
not even press `]`.

Tip: When also using `paredit-mode', enable that first so that
the binding for the `[`' key in the map for
`racket-smart-open-bracket-mode' has higher priority. See also
the variable `minor-mode-map-alist'.

Tip: When using this with `racket-hash-lang-mode', you may want
to use `racket-hash-lang-module-language-hook' to enable it IFF
the module langugage is something like \"racket\"."
  :lighter " RacketSmartOpen"
  :keymap (racket--easy-keymap-define '(("[" racket-smart-open-bracket)))
  (racket--assert-edit-or-repl-mode
   (lambda () (setq racket-smart-open-bracket-mode nil))))

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
                         "for/foldr"
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
                         "for*/foldr"
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
                         "for*/fold"
                         "for/foldr"
                         "for*/foldr")
                     (or space line-end))))
      ;; named-let bindings
      ;;
      (0 2 ,(rx (seq "let" (1+ whitespace) (1+ (not (in "()[]{}\",'`;#|\" "))))))))
  "A list of lists. Each sub list is arguments to supply to
  `racket--smart-open-bracket-helper'.")

(defun racket--smart-open-bracket-helper (pre-backward-sexps
                                          post-backward-sexps
                                          regexp)
  "Is point at a subform of a known form REGEXP that should open with '['.

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

See `racket-smart-open-bracket-mode'."
  (interactive "P")
  (let ((ch (or (and (save-excursion
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
     (declare-function paredit-open-round  "ext:paredit" (&optional N) t)
     (declare-function paredit-open-square "ext:paredit" (&optional N) t)
     (declare-function paredit-open-curly  "ext:paredit" (&optional N) t)
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

(provide 'racket-smart-open)

;; racket-smart-open.el ends her

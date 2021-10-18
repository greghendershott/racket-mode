;;; racket-font-lock.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2021 by Greg Hendershott.

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

(require 'cl-lib)
(require 'racket-custom)
(require 'racket-keywords-and-builtins)
(require 'racket-ppss)
(require 'racket-util)

;; Define 4 levels of font-lock, as documented in 23.6.5 "Levels of
;; Font Lock". User may control using `font-lock-maximum-decoration'.

;; Note: font-lock iterates by matcher, doing an re-search-forward
;; over the entire region. As a result, it's faster to consolidate
;; matchers that will yield the same result (unless they need to be
;; tried in a certain order).

;; Note: This relies on our character syntax already having been
;; applied. For example a Racket identifier like `|name with spaces|`
;; will already have word/symbol syntax on everything including the
;; pipe and space chars.

(defconst racket-font-lock-keywords-0
  (eval-when-compile
    `(;; #shebang
      (,(rx bol "#!" (+ nonl) eol) . font-lock-comment-face)

      ;; #lang
      (,(rx (group (group "#lang")
                   (1+ " ")
                   (group (1+ not-newline))))
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t))

      ;; #; sexp comments
      ;;
      ;; We don't put any comment syntax on these -- that way things
      ;; like indent and nav work within the sexp. They are solely
      ;; font-locked as comments, here.
      (,#'racket--font-lock-sexp-comments)

      ;; #<< here strings
      ;;
      ;; We only handle the opening #<<ID here. The remainder is
      ;; handled in `racket-font-lock-syntatic-face-function'.
      (,(rx (group "#<<" (+? (not (any blank ?\n)))) ?\n)
       (1 racket-here-string-face nil t))
    ))
  "Strings, comments, #lang.")

(defconst racket-font-lock-keywords-1
  (eval-when-compile
    `(;; Keyword arguments
      (,(rx "#:" (1+ (or (syntax word) (syntax symbol) (syntax punctuation))))
       . racket-keyword-argument-face)
      ;; Character literals
      (,(rx (seq "#\\" (1+ (or (syntax word) (syntax symbol) (syntax punctuation)))))
       . font-lock-constant-face)

      ;; Regular expression literals
      (,(rx (group (or "#rx" "#px")) ?\")
       1 font-lock-constant-face)

      ;; Misc reader literals
      (,(rx
         symbol-start
         (or "#t" "#T" "#true"
             "#f" "#F" "#false"
             (seq (any "-+")
                  (or (regexp "[iI][nN][fF]")
                      (regexp "[nN][aA][nN]"))
                  "." (any "0fFtT")))
         symbol-end)
       . font-lock-constant-face)

      ;; Numeric literals
      (,(rx
         symbol-start
         (or
          ;; #d #e #i or no hash prefix
          (seq (? "#" (any "dDeEiI"))
               (? (any "-+"))
               (1+ digit)
               (? (any "./") (1+ digit))
               (? (any "eEfF")
                  (? (any "-+"))
                  (1+ digit))
               (? (any "-+")
                  (1+ digit)
                  (? (any "./") (1+ digit))
                  (? (any "eEfF")
                     (? (any "-+"))
                     (1+ digit))
                  (any "iI")))
          ;; #x
          (seq "#" (any "xX")
               (? (any "-+"))
               (1+ hex-digit)
               (? (any "./") (1+ hex-digit))
               (? (any "-+")
                  (1+ hex-digit)
                  (? (any "./") (1+ hex-digit))
                  (any "iI")))
          ;; #b
          (seq "#" (any "bB")
               (? (any "-+"))
               (1+ (any "01"))
               (? (any "./") (1+ (any "01")))
               (? (any "eEfF")
                  (? (any "-+"))
                  (1+ (any "01")))
               (? (any "-+")
                  (1+ (any "01"))
                  (? (any "./") (1+ (any "01")))
                  (? (any "eEfF")
                     (? (any "-+"))
                     (1+ (any "01")))
                  (any "iI")))
          ;; #o
          (seq "#" (any "oO")
               (? (any "-+"))
               (1+ (any "0-7"))
               (? (any "./") (1+ (any "0-7")))
               (? (any "eEfF")
                  (? (any "-+"))
                  (1+ (any "0-7")))
               (? (any "-+")
                  (1+ (any "0-7"))
                  (? (any "./") (1+ (any "0-7")))
                  (? (any "eEfF")
                     (? (any "-+"))
                     (1+ (any "0-7")))
                  (any "iI")))
          ;; extflonum
          (or
           ;; #d or no hash prefix
           (seq (? "#" (any "dD"))
                (? (any "-+"))
                (1+ digit)
                (? (any "./") (1+ digit))
                (any "tT")
                (? (any "-+"))
                (1+ digit))
           ;; #x
           (seq "#" (any "xX")
                (? (any "-+"))
                (1+ hex-digit)
                (? (any "./") (1+ hex-digit))
                (any "tT")
                (? (any "-+"))
                (1+ hex-digit))
           ;; #b
           (seq "#" (any "bB")
                (? (any "-+"))
                (1+ (any "01"))
                (? (any "./") (1+ (any "01")))
                (any "tT")
                (? (any "-+"))
                (1+ (any "01")))
           ;; #o
           (seq "#" (any "oO")
                (? (any "-+"))
                (1+ (any "0-7"))
                (? (any "./") (1+ (any "0-7")))
                (any "tT")
                (? (any "-+"))
                (1+ (any "0-7")))))
         symbol-end)
       . font-lock-constant-face)

      ;; (quasi)syntax reader shorthand for symbols only
      (,(rx ?#
            (or ?` ?')
            (or
             (seq ?| (+ any) ?|)
             (seq (1+ (or (syntax word) (syntax symbol) (syntax punctuation))))))
       . racket-reader-syntax-quoted-symbol-face)

      ;; (quasi)quote reader shorthand for symbols only
      (,(rx (or ?` ?')
            (or
             (seq ?| (+ any) ?|)
             (seq (1+ (or (syntax word) (syntax symbol) (syntax punctuation))))))
       . racket-reader-quoted-symbol-face)))
  "Symbols, constants, regular expressions")

(defconst racket-font-lock-keywords-2
  (eval-when-compile
    `(;; def* -- variables
      (,(rx (syntax open-parenthesis)
            "def" (0+ (or (syntax word) (syntax symbol) (syntax punctuation)))
            (1+ space)
            (group (1+ (or (syntax word) (syntax symbol) (syntax punctuation)))))
       1 font-lock-variable-name-face)
      (,(rx (syntax open-parenthesis)
            "define-values"
            (1+ space)
            (syntax open-parenthesis)
            (group (1+ (or (syntax word) (syntax symbol) (syntax punctuation) space)))
            (syntax close-parenthesis))
       1 font-lock-variable-name-face)

      ;; def* -- functions
      (,(rx (syntax open-parenthesis)
            "def" (0+ (or (syntax word) (syntax symbol) (syntax punctuation)))
            (1+ space)
            (1+ (syntax open-parenthesis)) ;1+ b/c curried define
            (group (1+ (or (syntax word) (syntax symbol) (syntax punctuation)))))
       1 font-lock-function-name-face)

      ;; let identifiers
      (,#'racket--font-lock-let-identifiers)

      ;; module and module*
      (,(rx (syntax open-parenthesis)
            (group "module" (? "*"))
            (1+ space)
            (group (1+ (or (syntax word) (syntax symbol) (syntax punctuation))))
            (1+ space)
            (group (1+ (or (syntax word) (syntax symbol) (syntax punctuation)))))
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t)
       (3 font-lock-variable-name-face nil t))
      ;; module+
      (,(rx (syntax open-parenthesis)
            (group "module+")
            (1+ space)
            (group (1+ (or (syntax word) (syntax symbol) (syntax punctuation)))))
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t))))
  "Parens, modules, function/variable identifiers, syntax-")

(defconst racket-font-lock-keywords-3
  (eval-when-compile
    `((,(regexp-opt racket-keywords 'symbols) . font-lock-keyword-face)
      (,(regexp-opt racket-builtins-1-of-2 'symbols) . font-lock-builtin-face)
      (,(regexp-opt racket-builtins-2-of-2 'symbols) . font-lock-builtin-face)
      (,(regexp-opt racket-type-list 'symbols) . font-lock-type-face)

      ;; pretty lambda (deprecated)
      (,(rx (syntax open-parenthesis)
            (? (or "case-" "match-" "opt-"))
            (group "lambda")
            (or word-end symbol-end))
       1
       (ignore
        (when racket-pretty-lambda
          (compose-region (match-beginning 1)
                          (match-end       1)
                          racket-lambda-char)))
       nil t)))
  "Function/variable identifiers, Typed Racket types.

Note: To the extent you use #lang racket or #typed/racket, this
may be handy. But Racket is also a tool to make #lang's, and this
doesn't really fit that.")

(defconst racket-font-lock-keywords-level-0
  (append racket-font-lock-keywords-0))

(defconst racket-font-lock-keywords-level-1
  (append racket-font-lock-keywords-0
          racket-font-lock-keywords-1))

(defconst racket-font-lock-keywords-level-2
  (append racket-font-lock-keywords-0
          racket-font-lock-keywords-1
          racket-font-lock-keywords-2))

(defconst racket-font-lock-keywords-level-3
  (append racket-font-lock-keywords-0
          racket-font-lock-keywords-1
          racket-font-lock-keywords-2
          racket-font-lock-keywords-3))

(defconst racket-font-lock-keywords
  racket-font-lock-keywords-level-3)

(defun racket-font-lock-syntactic-face-function (state)
  (let ((q (racket--ppss-string-p state)))
    (if q
        (let ((startpos (racket--ppss-string/comment-start state)))
          (if (eq (char-after startpos) ?|)
              nil ;a |...| symbol
            (if (characterp q)
                font-lock-string-face
              racket-here-string-face)))
      font-lock-comment-face)))

;;; sexp comments

(defun racket--font-lock-sexp-comments (limit)
  "Font-lock sexp comments.

Note that our syntax table intentionally does not mark these as
comments. As a result, indent and nav work within the sexp.
Instead we merely font-lock them to look like comments.

See https://docs.racket-lang.org/srfi/srfi-std/srfi-62.html for a
discussion of s-expression comments. We try to handle nesting
like \"#; #; 1 2\". For more examples see the issue 432 section
of example/example.rkt."
  (while (re-search-forward (rx "#;") limit t)
    (if (racket--string-or-comment-p (match-beginning 0))
        (goto-char (match-end 0))       ;issues #388, #408
      (let ((first-prefix-begin (match-beginning 0)))
        (racket--region-set-face (match-beginning 0) (match-end 0)
                                 'font-lock-comment-delimiter-face t)
        ;; Font-lock and count any additional successive prefixes
        (goto-char (match-end 0))
        (forward-comment (buffer-size))
        (let ((num-prefixes 1))
          (save-match-data
            (while (looking-at (rx "#;"))
              (cl-incf num-prefixes)
              (racket--region-set-face (match-beginning 0) (match-end 0)
                                       'font-lock-comment-delimiter-face t)
              (goto-char (match-end 0))
              (forward-comment (buffer-size))))
          ;; Font-lock as many successive sexprs as prefixes
          (dotimes (_ num-prefixes)
            (let ((beg (point)))
              (forward-sexp 1)
              (racket--region-set-face beg (point)
                                       'font-lock-comment-face t)
              (forward-comment (buffer-size)))))
        ;; Cover everything from the beginning of the first prefix to
        ;; the end of the last sexp with font-lock-multiline; #443.
        (put-text-property first-prefix-begin (point)
                           'font-lock-multiline t))))
  nil)

(defun racket--string-or-comment-p (pos)
  (let ((state (syntax-ppss pos)))
    (or (racket--ppss-string-p  state)
        (racket--ppss-comment-p state))))

;;; let forms

(defun racket--font-lock-let-identifiers (limit)
  "In let forms give identifiers `font-lock-variable-name-face'.

This handles both let and let-values style forms (bindings with
with single identifiers or identifier lists).

Note: This works only when the let form has a closing paren.
\(Otherwise, when you type an incomplete let form before existing
code, this would mistakenly treat the existing code as part of
the let form.) The font-lock will kick in after you type the
closing paren. Or if you use electric-pair-mode, paredit, or
similar, it will already be there."
  (while (re-search-forward
          (rx (syntax open-parenthesis)
              (* (syntax whitespace))
              (group-n 1 "let"
                       (* (or (syntax word) (syntax symbol) (syntax punctuation)))))
          limit
          t)
    (ignore-errors
      (when (and (not (member (match-string-no-properties 1) '("let/ec" "let/cc")))
                 (racket--inside-complete-sexp))
        ;; Resume search before this let's bindings list, so we can
        ;; check rhs of bindings for more lets.
        (save-excursion
          ;; Check for named let
          (when (looking-at-p (rx (+ space) (+ (or (syntax word)
                                                   (syntax symbol)
                                                   (syntax punctuation)))))
            (forward-sexp 1)
            (backward-sexp 1)
            (racket--sexp-set-face font-lock-function-name-face))
          ;; Set font-lock-multiline property on entire identifier
          ;; list. Avoids need for font-lock-extend-region function.
          (put-text-property (point)
                             (save-excursion (forward-sexp 1) (point))
                             'font-lock-multiline t)
          (down-list 1) ;to the open paren of the first binding form
          (while (ignore-errors
                   (down-list 1) ;to the id or list of id's
                   (if (not (looking-at-p "[([{]"))
                       (racket--sexp-set-face font-lock-variable-name-face)
                     ;; list of ids, e.g. let-values
                     (down-list 1)    ;to first id
                     (cl-loop
                      do (racket--sexp-set-face font-lock-variable-name-face)
                      while (ignore-errors (forward-sexp 1) (backward-sexp 1) t))
                     (backward-up-list))
                   (backward-up-list) ;to open paren of this binding form
                   (forward-sexp 1)   ;to open paren of next binding form
                   t))))))
  nil)

;;; misc

(defun racket--inside-complete-sexp ()
  "Return whether point is inside a complete sexp."
  (condition-case ()
      (save-excursion (backward-up-list) (forward-sexp 1) t)
    (error nil)))

(defun racket--sexp-set-face (face &optional forcep)
  "Set 'face prop to FACE, rear-nonsticky, for the sexp starting at point.
Unless FORCEP is t, does so only if not already set in the
region.

Moves point to the end of the sexp."
  (racket--region-set-face (point)
                           (progn (forward-sexp 1) (point))
                           face
                           forcep))

(defun racket--region-set-face (beg end face &optional forcep)
  "Set 'face prop to FACE, rear-nonsticky, in the region BEG..END.
Unless FORCEP is t, does so only if not already set in the
region."
  (when (or forcep (not (text-property-not-all beg end 'face nil)))
    (add-text-properties beg end
                         `(face ,face
                                ;;rear-nonsticky (face)
                                ))))


(provide 'racket-font-lock)

;; racket-font-lock.el ends here

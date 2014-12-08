;;; racket-font-lock.el

;; Copyright (c) 2013-2014 by Greg Hendershott.

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

(require 'racket-keywords-and-builtins)

(defconst racket-keyword-argument-face 'racket-keyword-argument-face)
(defface racket-keyword-argument-face
  '((((background dark))
     (:foreground "IndianRed"))
    (((background light))
     (:foreground "Red3")))
  "Face for #:keyword arguments."
  :tag "Keyword argument face"
  :group 'racket)

(defconst racket-selfeval-face 'racket-selfeval-face)
(defface racket-selfeval-face
  '((t
     (:foreground "SeaGreen")))
  "Face for self-evaluating expressions like numbers, symbols, strings."
  :tag "Self-eval face"
  :group 'racket)

(defconst racket-paren-face 'racket-paren-face)
(defface racket-paren-face
  (let ((fg (face-foreground 'default)))
    `((t (:foreground ,fg))))
  "Face for parentheses () [] {}."
  :tag "Paren face"
  :group 'racket)

(defcustom racket-mode-pretty-lambda nil
  "Display lambda keywords using λ."
  :tag "Pretty lambda"
  :type 'boolean
  :group 'racket
  :safe 'booleanp)

(defconst racket-font-lock-keywords
  (eval-when-compile
    `(
      ;; #lang
      ("\\(\\(#lang\\)[ ]+\\([^\n]+\\)\\)"
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t))

      ;; keyword argument
      ("#:[^ )]+"                 . racket-keyword-argument-face)

      ;; symbol
      ("'\\sw+"                   . racket-selfeval-face)
      ("'|\\(\\sw\\| \\)+|"       . racket-selfeval-face)

      ;; #rx #px
      ("\\(#[pr]x\\)\"" (1 racket-selfeval-face))

      ;; literal char
      ("\\_<#\\\\\\([][-`~!@#$%&*()_+=^{}\;:'\"<>,.?/|\\\\]\\|\\sw+\\>\\)"
       . racket-selfeval-face)

      ;; paren
      ("[][(){}]"                 . racket-paren-face)

      (,(regexp-opt racket-type-list 'symbols) . font-lock-type-face)
      (,(regexp-opt racket-builtins 'symbols) . font-lock-builtin-face)
      (,(regexp-opt racket-keywords 'symbols) . font-lock-keyword-face)

      ;; def* -- variables
      ("(\\(def[^ ]*[ ]+\\([^( ]+\\)\\)"       2 font-lock-variable-name-face)
      ("(\\(define-values[ ]*(\\([^(]+\\))\\)" 2 font-lock-variable-name-face)

      ;; def* -- functions
      ("(\\(def[^ ]*[ ]*(\\([^ )]+\\)\\)" 2 font-lock-function-name-face)

      ;; module and module*
      ("(\\(module[*]?\\)[ ]+\\([^ ]+\\)[ ]+\\([^ ]+\\)"
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t)
       (3 font-lock-variable-name-face nil t))
      ;; module+
      ("(\\(module[+]\\)[ ]+\\([^ ]+\\)"
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t))

      ;; pretty lambda
      ("[[(]\\(case-\\|match-\\|opt-\\)?\\(lambda\\)\\>"
       2
       (if racket-mode-pretty-lambda
           (progn (compose-region (match-beginning 2)
                                  (match-end       2)
                                  racket-lambda-char)
                  nil)
         font-lock-keyword-face)
       nil t)

      ;; #t #f
      (,(regexp-opt '("#t" "#f") 'symbols) . racket-selfeval-face)

      ;; From my Pygments lexer (maybe can simplify b/c unlike Pygments
      ;; we're not lexing for types like int vs. float).
      ;;
      ;; Numeric literals including Racket reader hash prefixes.
      ;; Caveat: None of these regexps attempt to exclude identifiers
      ;; that start with a number, such as a variable named
      ;; "100-Continue".

      ;; #d (or no hash prefix)
      ("\\_<\\(#d\\)?[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<\\(#d\\)?[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<\\(#d\\)?[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<\\(#d\\)?[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#d[^ ]*\\_>". font-lock-warning-face)

      ;; #x
      ("\\_<#x[-+]?[0-9a-fA-F]+\\.[0-9a-fA-F]+\\_>" . racket-selfeval-face)
      ;; the exponent variation (e.g. #x1e1) is N/A
      ("\\_<#x[-+]?[0-9a-fA-F]+/[0-9a-fA-F]+\\_>" . racket-selfeval-face)
      ("\\_<#x[-+]?[0-9a-fA-F]+\\_>" . racket-selfeval-face)
      ("\\_<#x[^ ]*\\_>" . font-lock-warning-face)

      ;; #b
      ("\\_<#b[-+]?[01]+\\.[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[01]+e[-+]?[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[-+]?[01]/[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[-+]?[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[^ ]*\\_>" . font-lock-warnng-face)

      ;; #e
      ("\\_<#e[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[^ ]*\\_>" . font-lock-warning-face)

      ;; #i
      ("\\_<#i[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[^ ]*\\_>" . font-lock-warning-face)

      ;; #o
      ("\\_<#o[-+]?[0-7]+\\.[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[0-7]+e[-+]?[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[-+]?[0-7]+/[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[-+]?[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[^ ]*\\_>" . font-lock-warning-face)

      ;; numeric constants
      (,(regexp-opt '("+inf.0" "-inf.0" "+nan.0") 'symbols)
       . racket-selfeval-face)

      ))
    "Font lock keywords for Racket mode")

(provide 'racket-font-lock)

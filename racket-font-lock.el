;;; racket-font-lock.el

;; Copyright (c) 2013-2015 by Greg Hendershott.

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

(require 'racket-custom)
(require 'racket-keywords-and-builtins)

(defconst racket-font-lock-keywords
  (eval-when-compile
    `(
      ;; #lang
      (,(rx (group (group "#lang")
                   (1+ " ")
                   (group (1+ (not (any "\n"))))))
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t))

      ;; keyword argument
      (,(rx "#:" (1+ (not (any "^ "))))
       . racket-keyword-argument-face)

      ;; symbol
      (,(rx "'" (1+ (or (syntax symbol) (syntax word))))
       . racket-selfeval-face)
      ;; The '|symbol with spaces case is handed in syntax-propertize

      ;; #rx #px
      (,(rx (group (or "#rx" "#px")) ?\")
       1 racket-selfeval-face)

      ;; literal char
      (,(rx "#\\" (1+ (or (syntax symbol) (syntax word))))
       . racket-selfeval-face)

      ;; paren
      (,(rx (any "[](){}")) . racket-paren-face)

      (,(regexp-opt racket-type-list 'symbols) . font-lock-type-face)
      (,(regexp-opt racket-builtins 'symbols) . font-lock-builtin-face)
      (,(regexp-opt racket-keywords 'symbols) . font-lock-keyword-face)

      ;; def* -- variables
      (,(rx "(def" (0+ (not (any " "))) (1+ " ")
            (group (1+ (not (any "( ")))))
       1 font-lock-variable-name-face)
      (,(rx "(define-values" (0+ " ") "(" (group (1+ (not (any "(")))) ")")
       1 font-lock-variable-name-face)

      ;; def* -- functions
      (,(rx "(def" (0+ (not (any " "))) (1+ " ")
            "(" (group (1+ (not (any " )")))))
       1 font-lock-function-name-face)

      ;; module and module*
      (,(rx "("
            (group "module" (? "*"))
            (1+ " ")
            (group (1+ (not (any " "))))
            (1+ " ")
            (group (1+ (not (any " ")))))
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t)
       (3 font-lock-variable-name-face nil t))
      ;; module+
      (,(rx "("
            (group "module+")
            (1+ " ")
            (group (1+ (not (any " ")))))
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t))

      ;; pretty lambda
      (,(rx (syntax open-parenthesis)
            (? (or "case-" "match-" "opt-"))
            (group "lambda"))
       1
       (if racket-pretty-lambda
           (progn (compose-region (match-beginning 1)
                                  (match-end       1)
                                  racket-lambda-char)
                  nil)
         font-lock-keyword-face)
       nil t)

      ;; #t #f
      (,(rx (or "#t" "#f")) . racket-selfeval-face)

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
      ("\\_<#b[^ ]*\\_>" . font-lock-warning-face)

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

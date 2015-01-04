;;; racket-mode.el --- Major mode for Racket language.

;; Copyright (c) 2013-2014 by Greg Hendershott.

;; Package: racket-mode
;; Package-Requires: ((emacs "24.3"))
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

;;; Commentary:

;; Goals:
;; - Focus on Racket (not various Schemes).
;; - Fontify all Racket keywords, builtins, and so on.
;; - Fontify variations of define for functions and variables.
;; - Indent Racket forms (even `for/fold` and `for*/fold`).
;; - Follow DrRacket concepts where applicable.
;; - Compatible with Emacs 24.3+.
;;
;; Details: https://github.com/greghendershott/racket-mode

;;; Code:

(defconst racket-mode-copyright
  "Copyright (c) 2013-2014 by Greg Hendershott. Portions Copyright (c) Free Software Foundation and Copyright (c) 2002-2012 Neil Van Dyke.")

(defconst racket-mode-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  See
http://www.gnu.org/licenses/ for details.")

(defconst racket-mode-version "0.4")

(require 'racket-edit)
(require 'racket-repl)
(require 'easymenu)

(defvar racket-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m lisp-mode-shared-map)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("<f5>"      racket-run)
            ("M-C-<f5>"  racket-racket)
            ("C-<f5>"    racket-test)
            ("M-C-x"     racket-send-definition)
            ("C-x C-e"   racket-send-last-sexp)
            ("C-c C-r"   racket-send-region)
            ("C-c C-e x" racket-expand-definition)
            ("C-c C-e e" racket-expand-last-sexp)
            ("C-c C-e r" racket-expand-region)
            ("C-c C-e a" racket-expand-again)
            ("RET"       racket-newline-and-indent)
            ("TAB"       racket-indent-or-complete)
            ("M-C-u"     racket-backward-up-list)
            (")"         racket-insert-closing-paren)
            ("]"         racket-insert-closing-bracket)
            ("}"         racket-insert-closing-brace)
            ("C-c C-p"   racket-cycle-paren-shapes)
            ("M-C-y"     racket-insert-lambda)
            ("C-c C-d"   racket-doc)
            ("C-c C-."   racket-describe)
            ("M-."       racket-visit-definition)
            ("M-C-."     racket-visit-module)
            ("M-,"       racket-unvisit)
            ("C-c C-f"   racket-fold-all-tests)
            ("C-c C-U"   racket-unfold-all-tests)))
    m)
  "Keymap for Racket mode. Inherits from `lisp-mode-shared-map'.")

(easy-menu-define racket-mode-menu racket-mode-map
  "Menu for Racket mode."
  '("Racket"
    ("Run"
     ["in REPL" racket-run]
     ["via `racket`" racket-racket])
    ("Tests"
     ["in REPL" racket-test]
     ["via `raco test`" racket-raco-test]
     "---"
     ["Fold All" racket-fold-all-tests]
     ["Unfold All" racket-unfold-all-tests])
    ("Eval"
     ["Region" racket-send-region :active (region-active-p)]
     ["Definition" racket-send-definition]
     ["Last S-Expression" racket-send-last-sexp])
    ("Macro Expand"
     ["Region" racket-expand-region  :active (region-active-p)]
     ["Definition" racket-expand-definition]
     ["Last S-Expression" racket-expand-last-sexp]
     "---"
     ["Again" racket-expand-again])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes]
    "---"
    ["Visit Definition" racket-visit-definition]
    ["Visit Module" racket-visit-module]
    ["Return from Visit" racket-unvisit]
    "---"
    ["Next Error or Link" next-error]
    ["Previous Error" previous-error]
    "---"
    ["Tidy Requires" racket-tidy-requires]
    ["Trim Requires" racket-trim-requires]
    ["Use #lang racket/base" racket-base-requires]
    "---"
    ["Racket documentation" racket-doc]
    ["Describe" racket-describe]
    ["Customize..." customize-mode]))

(defvar racket-imenu-generic-expression
  '((nil
     "^(define\\s-+(?\\(\\sw+\\)" 1)
    ("Struct"
     "^(struct\\s-+\\(\\sw+\\)" 1)
    ("Syntax"
     "^(define-syntax\\s-+(?\\(\\sw+\\)" 1))
  "Imenu generic expression for racket mode.  See `imenu-generic-expression'.")

(defun racket--variables-imenu ()
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
       racket-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
       '(("+-*/.<>=?!$%_&~^:" . "w"))))

;;;###autoload
(define-derived-mode racket-mode prog-mode
  "Racket"
  "Major mode for editing Racket.
\\{racket-mode-map}"
  (racket--variables-for-both-modes)
  (racket--variables-imenu)
  (racket--company-setup)
  (hs-minor-mode t))

;;;###autoload
(setq auto-mode-alist
      (append '(("\\.rkt\\'" . racket-mode)
                ("\\.rktd\\'" . racket-mode))
              auto-mode-alist))

(provide 'racket-mode)

;;; racket-mode.el ends here

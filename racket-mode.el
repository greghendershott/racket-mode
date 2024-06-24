;;; racket-mode.el --- Racket editing, REPL, and more  -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.

;; Package: racket-mode
;; Package-Requires: ((emacs "25.1"))
;; Author: Greg Hendershott <racket-mode-author@greghendershott.com>
;; Maintainer: Greg Hendershott
;; URL: https://www.racket-mode.com/
;; Version: 1

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Goals:
;; - Focus on Racket lang.
;; - Follow DrRacket concepts where applicable.
;; - Thorough font-lock and indent.
;; - Compatible with Emacs 25.1+ and Racket 6.12+.
;;
;; Details: https://github.com/greghendershott/racket-mode

;;; Code:

(require 'racket-doc)
(require 'racket-edit)
(require 'racket-xp)
(require 'racket-custom)
(require 'racket-smart-open)
(require 'racket-imenu)
(require 'racket-package)
(require 'racket-profile)
(require 'racket-logger)
(require 'racket-shell)
(require 'racket-stepper)
(require 'racket-repl)
(require 'racket-repl-buffer-name)
(require 'racket-collection)
(require 'racket-lisp-mode)
(require 'racket-bug-report)
(require 'racket-util)
(require 'easymenu)

(defvar racket-mode-map
  (racket--easy-keymap-define
   '((("C-c C-c"
       "C-c C-k")   racket-run-module-at-point)
     ("C-c C-z"     racket-edit-switch-to-repl)
     ("<f5>"        racket-run-and-switch-to-repl)
     ("M-C-<f5>"    racket-racket)
     ("C-<f5>"      racket-test)
     ("C-c C-t"     racket-test)
     ("C-c C-l"     racket-logger)
     ("C-c C-o"     racket-profile)
     ("M-C-x"       racket-send-definition)
     ("C-x C-e"     racket-send-last-sexp)
     ("C-c C-r"     racket-send-region)
     ("C-c C-e f"   racket-expand-file)
     ("C-c C-e x"   racket-expand-definition)
     ("C-c C-e e"   racket-expand-last-sexp)
     ("C-c C-e r"   racket-expand-region)
     ("C-c C-x C-f" racket-open-require-path)
     ("TAB"         indent-for-tab-command)
     ("M-C-u"       racket-backward-up-list)
     ("C-c C-p"     racket-cycle-paren-shapes)
     ("M-C-y"       racket-insert-lambda)
     ("C-c C-d"     racket-documentation-search)
     (("C-c C-s"
       "C-c C-.")   racket-describe-search)
     ("C-c C-f"     racket-fold-all-tests)
     ("C-c C-u"     racket-unfold-all-tests)
     ((")" "]" "}") racket-insert-closing)))
  "Keymap for Racket mode.")

(easy-menu-define racket-mode-menu racket-mode-map
  "Menu for `racket-mode'."
  '("Racket"
    ("Run"
     ["in REPL" racket-run]
     ["in REPL and switch to REPL" racket-run-and-switch-to-repl]
     ["in *shell* using `racket`" racket-racket])
    ("Tests"
     ["in REPL" racket-test]
     ["in *shell* using `raco test`" racket-raco-test]
     "---"
     ["Fold All" racket-fold-all-tests]
     ["Unfold All" racket-unfold-all-tests])
    ("Eval"
     ["Region" racket-send-region :active (region-active-p)]
     ["Definition" racket-send-definition]
     ["Last S-Expression" racket-send-last-sexp])
    ("Macro Expand"
     ["File" racket-expand-file]
     ["Region" racket-expand-region  :active (region-active-p)]
     ["Definition" racket-expand-definition]
     ["Last S-Expression" racket-expand-last-sexp])
    ["Switch to REPL" racket-repl]
    ("Tools"
     ["Profile" racket-profile]
     ["Error Trace" racket-run-with-errortrace]
     ["Step Debug" racket-run-with-debugging]
     ["Toggle XP Mode" racket-xp-mode])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes]
    ["Align" racket-align]
    ["Unalign" racket-unalign]
    "---"
    ["Visit Module" xref-find-definitions]
    ["Return from Visit" xref-pop-marker-stack]
    "---"
    ["Open Require Path" racket-open-require-path]
    ["Find Collection" racket-find-collection]
    "---"
    ["Next Error or Link" next-error]
    ["Previous Error" previous-error]
    "---"
    ["Tidy Requires" racket-tidy-requires]
    ["Trim Requires" racket-trim-requires]
    ["Use #lang racket/base" racket-base-requires]
    "---"
    ["List Racket Packages" list-racket-packages]
    ["Start Faster" racket-mode-start-faster]
    ["Customize..." customize-mode]))

(declare-function racket-call-racket-repl-buffer-name-function "racket-repl-buffer-name" ())
(autoload        'racket-call-racket-repl-buffer-name-function "racket-repl-buffer-name")

;;;###autoload
(define-derived-mode racket-mode prog-mode
  "Racket"
  "Major mode for editing Racket source files.

\\{racket-mode-map}"
  ;;; Syntax
  (set-syntax-table racket-mode-syntax-table)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local syntax-propertize-function #'racket-syntax-propertize-function)
  (syntax-propertize (point-max)) ;for e.g. paredit: see issue #222
  ;; -----------------------------------------------------------------
  ;; REPL
  (racket-call-racket-repl-buffer-name-function)
  (add-hook 'kill-buffer-hook
            #'racket-mode-maybe-offer-to-kill-repl-buffer
            nil t)
  ;; -----------------------------------------------------------------
  ;; Font-lock
  (setq-local font-lock-defaults
              (list racket-font-lock-keywords ;keywords
                    nil                       ;keywords-only?
                    nil                       ;case-fold?
                    nil                       ;syntax-alist
                    nil                       ;syntax-begin
                    ;; Additional variables:
                    (cons 'font-lock-mark-block-function #'mark-defun)
                    (cons 'parse-sexp-lookup-properties t)
                    (cons 'font-lock-multiline t)
                    (cons 'font-lock-syntactic-face-function
                          #'racket-font-lock-syntactic-face-function)
                    (list 'font-lock-extend-region-functions
                          #'font-lock-extend-region-wholelines
                          #'font-lock-extend-region-multiline)))
  ;; -----------------------------------------------------------------
  ;; Comments. Mostly borrowed from lisp-mode and/or scheme-mode
  (setq-local comment-start ";")
  (setq-local comment-add 1)        ;default to `;;' in comment-region
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-column 40)
  (setq-local comment-multi-line t) ;for auto-fill-mode and #||# comments
  ;; Font lock mode uses this only when it knows a comment is starting:
  (setq-local font-lock-comment-start-skip ";+ *")
  ;; -----------------------------------------------------------------
  ;; Indent
  (setq-local indent-line-function #'racket-indent-line)
  (setq-local indent-tabs-mode nil)
  ;; -----------------------------------------------------------------
  ;; Code folding
  (add-to-list 'hs-special-modes-alist
               '(racket-mode "(" ")" ";" nil nil))
  ;; -----------------------------------------------------------------
  ;;; Misc
  (setq-local local-abbrev-table racket-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function #'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local beginning-of-defun-function #'racket--beginning-of-defun-function)
  (setq-local imenu-create-index-function #'racket-imenu-create-index-function)
  (setq-local completion-at-point-functions (list #'racket-complete-at-point))
  (setq-local eldoc-documentation-function nil)
  (add-hook 'xref-backend-functions
            #'racket-mode-xref-backend-function
            nil t)
  (setq racket-submodules-at-point-function #'racket-submodules-at-point-text-sexp))

;;;###autoload
(progn
  ;; Use simple regexps for auto-mode-alist as they may be given to
  ;; grep (e.g. by default implementation of `xref-find-references').
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  (add-to-list 'auto-mode-alist '("\\.rktd\\'" . racket-mode))
  (add-to-list 'auto-mode-alist '("\\.rktl\\'" . racket-mode))
  ;; "Fancier" regexp OK here:
  (modify-coding-system-alist 'file "\\.rkt[dl]?\\'"  'utf-8)
  (add-to-list 'interpreter-mode-alist '("racket" . racket-mode)))

;;;###autoload
(defun racket-mode-start-faster ()
  "Compile Racket Mode's .rkt files for faster startup.

Racket Mode is implemented as an Emacs Lisp \"front end\" that
talks to a Racket process \"back end\". Because Racket Mode is
delivered as an Emacs package instead of a Racket package,
installing it does not do the `raco setup` that is normally done
for Racket packages.

This command will do a `raco make` of Racket Mode's .rkt files,
creating bytecode files in `compiled/` subdirectories. As a
result, when a command must start the Racket process, it will
start somewhat faster.

On many computers, the resulting speed up is negligible, and
might not be worth the complication.

If you run this command, ever, you will need to run it again
after:

- Installing an updated version of Racket Mode. Otherwise, you
  might lose some of the speed-up.

- Installing a new version of Racket and/or changing the value of
  the variable `racket-program'. Otherwise, you might get an
  error message due to the bytecode being different versions.

To revert to compiling on startup, use
`racket-mode-start-slower'. "
  (interactive)
  (let* ((racket  (executable-find racket-program))
         (rkts0   (expand-file-name "*.rkt" racket--rkt-source-dir) )
         (rkts1   (expand-file-name "commands/*.rkt" racket--rkt-source-dir))
         (command (format "%s -l raco make -v %s %s"
                          (shell-quote-wildcard-pattern racket)
                          (shell-quote-wildcard-pattern rkts0)
                          (shell-quote-wildcard-pattern rkts1)))
         (prompt (format "Do `%s` " command)))
    (when (y-or-n-p prompt)
      (racket-stop-back-end)
      (async-shell-command command))))

(defun racket-mode-start-slower ()
  "Delete the \"compiled\" directories made by `racket-mode-start-faster'."
  (interactive)
  (let* ((dir0 (expand-file-name "compiled"          racket--rkt-source-dir) )
         (dir1 (expand-file-name "commands/compiled" racket--rkt-source-dir))
         (prompt (format "Delete %s and %s" dir0 dir1)))
    (when (y-or-n-p prompt)
      (racket-stop-back-end)
      (ignore-errors (delete-directory dir0 t))
      (ignore-errors (delete-directory dir1 t)))))

(defun racket-documentation-search ()
  "Search documentation.

This command is useful in several situations:

- You are not using `racket-xp-mode' for a `racket-mode' edit
  buffer, so `racket-xp-documentation' is not available.

- There is no `racket-repl-mode' buffer with a live namespace, so
  `racket-repl-documentation' is not available or helpful.

- You want to search for definitions provided by all modules --
  for example, the \"define\" syntax provided by racket/base, by
  typed/racket/base, and by other modules, as well definitions or
  topics that merely include \"define\".

This command does not try to go directly to the help topic for a
definition provided by any specific module. Instead it goes to
the Racket \"Search Manuals\" page."
  (interactive)
  (racket--doc '(16) nil nil))

;;; xref

;; Note that this backend will be ignored when `racket-xp-mode' minor
;; mode is active. This backend is a weak effort to do /something/ in
;; plain `racket-mode' edit buffers, without using the Racket Mode
;; back end process.
;;
;; Currently, aside from being able to visit relative require files,
;; it just suggests using `racket-xp-mode' to find definitions.
;;
;; As for finding references: We just use the default
;; `xref-backend-references' which greps within a project.
;; `racket-xp-mode' is better only for intra-file references found by
;; check-syntax; otherwise it defers to the same default, too.

(defun racket-mode-xref-backend-function ()
  'racket-mode-xref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql racket-mode-xref)))
  (or (racket--module-path-name-at-point)
      (thing-at-point 'symbol)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql racket-mode-xref)))
  (completion-table-dynamic #'ignore))

(cl-defmethod xref-backend-definitions ((_backend (eql racket-mode-xref)) str)
  (or (pcase (get-text-property 0 'racket-module-path str)
        (`relative
         (let ((path (racket--rkt-or-ss-path
                      (expand-file-name (substring-no-properties str 1 -1)))))
           (list (xref-make str (xref-make-file-location path 1 0))))))
      (list (xref-make str
                       (xref-make-bogus-location
                        "Cannot find definitions in plain `racket-mode'; see `racket-xp-mode'")))))

;; Use the default `xref-backend-references', which greps within a project.

;;; Commands that predate `racket-xp-mode'

(defun racket-doc ()
  "This command is obsolete.

Instead please use `racket-documentation-search',
`racket-xp-documentation' or `racket-repl-documentation'. See:
<https://github.com/greghendershott/racket-mode/issues/439>"
  (interactive)
  (describe-function 'racket-doc))

(defun racket-describe ()
  "Instead please use `racket-xp-describe' or `racket-repl-describe'.
See: <https://github.com/greghendershott/racket-mode/issues/439>"
  (interactive)
  (describe-function 'racket-describe))

;; See also `racket-visit-definition' alias in racket-visit.el

(provide 'racket-mode)

;;; racket-mode.el ends here

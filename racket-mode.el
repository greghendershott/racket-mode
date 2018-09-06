;;; racket-mode.el --- Major mode for Racket language.

;; Copyright (c) 2013-2018 by Greg Hendershott.

;; Package: racket-mode
;; Package-Requires: ((emacs "24.3") (faceup "0.0.2") (s "1.9.0"))
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
;; - Focus on Racket lang.
;; - Follow DrRacket concepts where applicable.
;; - Thorough font-lock and indent.
;; - Compatible with Emacs 24.3+ and Racket 6.0+.
;;
;; Details: https://github.com/greghendershott/racket-mode

;;; Code:

(require 'racket-edit)
(require 'racket-imenu)
(require 'racket-profile)
(require 'racket-logger)
(require 'racket-stepper)
(require 'racket-repl)
(require 'racket-collection)
(require 'racket-bug-report)
(require 'racket-util)
(require 'easymenu)

(defvar racket-mode-map
  (racket--easy-keymap-define
   '((("C-c C-c"
       "C-c C-k")   racket-run)
     ("C-c C-z"     racket-repl)
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
     ("["           racket-smart-open-bracket)
     (")"           racket-insert-closing)
     ("]"           racket-insert-closing)
     ("}"           racket-insert-closing)
     ("C-c C-p"     racket-cycle-paren-shapes)
     ("M-C-y"       racket-insert-lambda)
     ("C-c C-d"     racket-doc)
     ("C-c C-."     racket-describe)
     ("M-."         racket-visit-definition)
     ("M-C-."       racket-visit-module)
     ("M-,"         racket-unvisit)
     ("C-c C-f"     racket-fold-all-tests)
     ("C-c C-u"     racket-unfold-all-tests)))
  "Keymap for Racket mode.")

(easy-menu-define racket-mode-menu racket-mode-map
  "Menu for Racket mode."
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
     ["Check Syntax" racket-check-syntax-mode]
     ["Error Trace" racket-run-with-errortrace]
     ["Step Debug" racket-run-with-debugging])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    ["Cycle Paren Shapes" racket-cycle-paren-shapes]
    ["Align" racket-align]
    ["Unalign" racket-unalign]
    "---"
    ["Visit Definition" racket-visit-definition]
    ["Visit Module" racket-visit-module]
    ["Return from Visit" racket-unvisit]
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
    ["Racket Documentation" racket-doc]
    ["Describe" racket-describe]
    ["Start Faster" racket-mode-optimize-startup]
    ["Customize..." customize-mode]))

(defun racket--variables-imenu ()
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-create-index-function #'racket--imenu-create-index-function))

;;;###autoload
(define-derived-mode racket-mode prog-mode
  "Racket"
  "Major mode for editing Racket.
\\{racket-mode-map}"
  (racket--common-variables)
  (racket--variables-imenu)
  (hs-minor-mode t))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.rkt[dl]?\\'" . racket-mode))
  (modify-coding-system-alist 'file "\\.rkt[dl]?\\'"  'utf-8)
  (add-to-list 'interpreter-mode-alist '("racket" . racket-mode)))

;;;###autoload
(defun racket-mode-start-faster ()
  "Compile racket-mode's .rkt files for faster startup.

racket-mode is implemented as an Emacs Lisp \"front end\" that
talks to a Racket process \"back end\". Because racket-mode is
delivered as an Emacs package instead of a Racket package,
installing it does _not_ do the `raco setup` that is normally
done for Racket packages.

This command will do a `raco make` of racket-mode's .rkt files,
creating bytecode files in `compiled/` subdirectories. As a
result, when a `racket-run' or `racket-repl' command must start
the Racket process, it will start faster.

If you run this command, _ever_, you should run it _again_ after:

- Installing an updated version of racket-mode. Otherwise, you
  might lose some of the speed-up.

- Installing a new version of Racket and/or changing the value of
  the variable `racket-program'. Otherwise, you might get an
  error message due to the bytecode being different versions."
  (interactive)
  (dolist (dir (list racket--rkt-source-dir
                     (concat racket--rkt-source-dir "/commands/")))
    (let* ((command (format "%s -l raco make -v %s"
                            racket-program
                            (expand-file-name "*.rkt" dir)))
           (prompt (format "Do `%s` " command)))
      (when (y-or-n-p prompt)
        (async-shell-command command)))))

(provide 'racket-mode)

;;; racket-mode.el ends here

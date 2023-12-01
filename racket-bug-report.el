;;; racket-bug-report.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2023 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-macs)
(require 'cus-edit)
(require 'package)
(require 'seq)
(require 'racket-back-end)
(require 'racket-cmd)
(require 'racket-custom)

;;;###autoload
(defun racket-bug-report ()
  "Fill a buffer with data to make a Racket Mode bug report."
  (interactive)
  (unless (string-match-p "^racket-" (symbol-name major-mode))
    (user-error "Please run from a Racket Mode buffer in which you're having a problem"))
  (let ((help-window-select t)
        (print-length nil) ;for `pp'
        (print-level nil)) ;for `pp'
    (cl-flet ((p (label value)
                 (princ (format "<dt>%s</dt>" label))
                 (princ "<dd><pre>")
                 (pp value)
                 (princ "</pre></dd>\n"))
              (section (label thunk)
                       (princ (format "<h2>%s</h2>\n" label))
                       (princ "<dl>\n")
                       (funcall thunk)
                       (princ "</dl>\n"))
              (symbol-less-p (a b) (string-lessp (symbol-name a) (symbol-name b))))
      (with-help-window "*racket-mode bug report*"
        (princ "Please copy all of the following lines and paste them into your bug report\n")
        (princ "at <https://github.com/greghendershott/racket-mode/issues/>.\n\n")

        (princ "<details>\n")
        (section "Package"
                 (lambda ()
                   (p "Metadata"
                      (let ((v (assq 'racket-mode package-alist)))
                        (and v (cdr v))))))
        (section "General values"
                 (lambda ()
                   (dolist (sym '(emacs-version
                                  major-mode
                                  system-type
                                  x-gtk-use-system-tooltips
                                  after-change-functions
                                  completion-at-point-functions
                                  eldoc-documentation-function
                                  font-lock-defaults
                                  xref-backend-functions))
                     (ignore-errors
                       (p sym (symbol-value sym))))
                   (dolist (fun (list #'display-graphic-p))
                     (ignore-errors
                       (p fun (funcall fun))))))
        (section "Racket Mode values"
                 (lambda ()
                   (p 'racket--cmd-open-p (racket--cmd-open-p))
                   (dolist (sym
                            (sort (append (racket--bug-report-customs)
                                          '(racket-mode-hook
                                            racket-hash-lang-mode-hook
                                            racket-hash-lang-module-language-hook
                                            racket-repl-mode-hook
                                            racket-back-end-configurations
                                            racket--el-source-dir
                                            racket--rkt-source-dir))
                                  #'symbol-less-p))
                     (p sym (symbol-value sym)))))
        (section "Minor modes"
                 (lambda ()
                   (let* ((minor-modes (seq-uniq
                                        (append minor-mode-list
                                                (mapcar #'car minor-mode-alist))))
                          (minor-modes (sort minor-modes #'symbol-less-p))
                          (enabled (seq-filter (lambda (sym)
                                                 (when (ignore-errors (symbol-value sym))
                                                   sym))
                                               minor-modes))
                          (disabled (seq-filter (lambda (sym)
                                                 (unless (ignore-errors (symbol-value sym))
                                                   sym))
                                               minor-modes)))
                     (p 'enabled  (mapcar #'list enabled)) ;so pp line-breaks
                     (princ "<details><summary>Disabled minor modes</summary>\n")
                     (p 'disabled (mapcar #'list disabled))
                     (princ "</details>\n"))))
        (princ "</details>\n\nSteps to reproduce: ")))
    (forward-line 2)))

(defun racket--bug-report-customs ()
  (let ((syms nil))
    (cl-labels ((item (v) (pcase v
                            (`(,sym custom-variable) (push sym syms))
                            (`(,sym custom-group)    (group sym))))
                (group (sym) (dolist (v (custom-group-members sym nil))
                               (item v))))
      (group 'racket)
      syms)))

(provide 'racket-bug-report)

;;; racket-bug-report.el ends here

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
  "Fill a buffer with details for a Racket Mode bug report."
  (interactive)
  (unless (string-match-p "^racket-" (symbol-name major-mode))
    (user-error "Please run from a Racket Mode buffer in which you're having a problem"))
  (let ((original-buffer (current-buffer))
        (help-window-select t)
        (print-length nil) ;for `pp'
        (print-level nil)) ;for `pp'
    (cl-flet* ((-section (label thunk)
                 (princ (format "<h2>%s</h2>\n" label))
                 (princ "<dl>\n")
                 (funcall thunk)
                 (princ "</dl>\n"))
               (show (label value)
                 (princ (format "<dt>%s</dt>" label))
                 (princ "<dd><pre>")
                 (pp value)
                 (princ "</pre></dd>\n"))
               (show-vars (syms)
                 (with-current-buffer original-buffer
                   (dolist (sym syms)
                     (ignore-errors (show sym (symbol-value sym))))))
               (symbol-less-p (a b)
                 (string-lessp (symbol-name a) (symbol-name b))))
      (cl-macrolet ((section (title &rest body)
                             `(-section ,title (lambda () ,@body))))
        (with-help-window "*racket-mode bug report*"
          (princ "Please copy all of the following lines and paste them into your bug report\n")
          (princ "at <https://github.com/greghendershott/racket-mode/issues/>.\n\n")

          (princ "<details>\n")
          (section "Package"
                   (show "metadata"
                         (let ((v (assq 'racket-mode package-alist)))
                           (and v (cdr v))))
                   (show-vars '(package-archives
                                racket--el-source-dir
                                racket--rkt-source-dir)))
          (section "System values"
                   (show-vars '(emacs-version
                                major-mode
                                system-type
                                x-gtk-use-system-tooltips))
                   (show 'display-graphic-p (display-graphic-p)))
          (section "Buffer values"
                   (show-vars '(after-change-functions
                                before-change-functions
                                completion-at-point-functions
                                eldoc-documentation-function
                                eldoc-documentation-strategy
                                eldoc-documentation-functions
                                font-lock-defaults
                                pre-command-hook
                                post-command-hook
                                post-self-insert-hook
                                xref-backend-functions)))
          (section "Racket Mode values"
                   (show 'racket--cmd-open-p (racket--cmd-open-p))
                   (show-vars
                    (sort
                     (seq-uniq
                      (append
                       (racket--bug-report-customs)
                       '(racket-mode-hook
                         racket-hash-lang-mode-hook
                         racket-hash-lang-module-language-hook
                         racket-repl-mode-hook
                         racket-back-end-configurations)))
                     #'symbol-less-p)))
          (section "Minor modes"
                   (let* ((minor-modes (seq-uniq
                                        (append minor-mode-list
                                                (mapcar #'car minor-mode-alist))))
                          (minor-modes (sort minor-modes #'symbol-less-p))
                          (enabled (with-current-buffer original-buffer
                                     (seq-filter (lambda (sym)
                                                   (when (ignore-errors (symbol-value sym))
                                                     sym))
                                                 minor-modes)))
                          (disabled (with-current-buffer original-buffer
                                      (seq-filter (lambda (sym)
                                                    (unless (ignore-errors (symbol-value sym))
                                                      sym))
                                                  minor-modes))))
                     (show 'enabled  (mapcar #'list enabled)) ;so pp line-breaks
                     (princ "<details><summary>Disabled minor modes</summary>\n")
                     (show 'disabled (mapcar #'list disabled))
                     (princ "</details>\n")))
          (princ "</details>\n\nSteps to reproduce: "))))
    (forward-line 2)))

(defun racket--bug-report-customs ()
  (let ((syms nil))
    (cl-labels ((item (v)
                  (pcase v
                    (`(,sym custom-variable) (push sym syms))
                    (`(,sym custom-group)    (group sym))))
                (group (sym)
                  (dolist (v (custom-group-members sym nil))
                    (item v))))
      (group 'racket)
      (group 'racket-xp)
      (group 'racket-repl)
      (group 'racket-hash-lang)
      (group 'racket-other)
      syms)))

(provide 'racket-bug-report)

;;; racket-bug-report.el ends here

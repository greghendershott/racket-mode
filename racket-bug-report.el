;;; racket-bug-report.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
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

(require 'cl-lib)
(require 'package)
(require 'racket-custom)

;;;###autoload
(defun racket-bug-report ()
  "Fill a buffer with data to make a Racket Mode bug report."
  (interactive)
  (let ((help-window-select t)
        (print-length nil) ;for `pp'
        (print-level nil)) ;for `pp'
    (with-help-window "*racket-mode bug report*"
      (princ "Please copy all of the following lines and paste them into your bug report\n")
      (princ "at <https://github.com/greghendershott/racket-mode/issues/>.\n\n")

      (princ "<details>\n")
      (princ "<pre>\n")
      (pp (cons '(alist-get 'racket-mode package-alist)
                (let ((v (assq 'racket-mode package-alist)))
                  (and v (cdr v)))))
      (cl-labels ((id-val (id) (list id
                                     (condition-case () (symbol-value id)
                                       (error 'UNDEFINED)))))
        (pp `(,@(mapcar #'id-val
                        `(emacs-version
                          system-type
                          x-gtk-use-system-tooltips
                          major-mode
                          racket--el-source-dir
                          racket--rkt-source-dir
                          racket-program
                          racket-command-timeout
                          racket-path-from-emacs-to-racket-function
                          racket-path-from-racket-to-emacs-function
                          racket-browse-url-function
                          racket-documentation-search-location
                          racket-xp-after-change-refresh-delay
                          racket-xp-mode-lighter
                          racket-xp-highlight-unused-regexp
                          racket-repl-buffer-name-function
                          racket-submodules-to-run
                          racket-memory-limit
                          racket-error-context
                          racket-repl-history-directory
                          racket-history-filter-regexp
                          racket-images-inline
                          racket-imagemagick-props
                          racket-images-keep-last
                          racket-images-system-viewer
                          racket-pretty-print
                          racket-use-repl-submit-predicate
                          racket-pretty-print
                          racket-indent-curly-as-sequence
                          racket-indent-sequence-depth
                          racket-pretty-lambda
                          racket-smart-open-bracket-enable
                          racket-module-forms
                          racket-logger-config
                          racket-show-functions))))
        ;; Show lists of enabled and disabled minor modes, each sorted by name.
        (let* ((minor-modes (cl-remove-duplicates
                             (append minor-mode-list
                                     (mapcar #'car minor-mode-alist))))
               (modes/values (mapcar #'id-val minor-modes))
               (sorted (sort modes/values
                             (lambda (a b)
                               (string-lessp (format "%s" (car a))
                                             (format "%s" (car b)))))))
          (cl-labels ((f (x) (list (car x)))) ;car as a list so pp line-wraps
            (pp `(enabled-minor-modes  ,@(mapcar #'f (cl-remove-if-not #'cadr sorted))))
            (pp `(disabled-minor-modes ,@(mapcar #'f (cl-remove-if     #'cadr sorted)))))))
      (princ "</pre>\n")
      (princ "</details>\n"))
    (forward-line 2)))

(provide 'racket-bug-report)

;;; racket-bug-report.el ends here

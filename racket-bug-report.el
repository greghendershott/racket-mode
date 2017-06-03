;;; racket-bug-report.el

;; Copyright (c) 2013-2016 by Greg Hendershott.
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
(require 'racket-util)

(defconst racket--source-dir
  (file-name-directory (or load-file-name (racket--buffer-file-name))))

;;;###autoload
(defun racket-bug-report ()
  "Fill a buffer with data to make a racket-mode bug report."
  (interactive)
  (unless (memq major-mode '(racket-mode racket-repl-mode))
    (user-error "Please run this from a racket-mode or racket-repl-mode buffer."))
  (with-help-window "*racket-mode bug report*"
    (princ "TIP: If you get an `invalid function' error, be aware that Emacs package\n")
    (princ "updates don't necessarily fully update Emacs' state.  In some cases, you\n")
    (princ "might even need to:\n\n")
    (princ  "  1. Uninstall racket-mode\n")
    (princ  "  2. Exit and restart Emacs\n")
    (princ  "  3. Install racket-mode\n\n\n")
    (princ "When you submit a bug report at:\n\n")
    (princ "  https://github.com/greghendershott/racket-mode/issues/new\n\n")
    (princ "Please copy and paste ALL OF THE FOLLOWING LINES from\n")
    (princ "`<details>' through `</details>':\n\n\n")
    (princ "<details>\n")
    (princ "```\n")
    (cl-labels ((id-val (id) (list id
                                   (condition-case () (symbol-value id)
                                     (error 'UNDEFINED)))))
      (let ((emacs-uptime (emacs-uptime)))
        (pp `(,@(mapcar #'id-val
                        `(emacs-version
                          emacs-uptime
                          system-type
                          major-mode
                          racket--source-dir
                          racket-program
                          racket-memory-limit
                          racket-error-context
                          racket-history-filter-regexp
                          racket-images-inline
                          racket-images-keep-last
                          racket-images-system-viewer
                          racket-pretty-print
                          racket-indent-curly-as-sequence
                          racket-indent-sequence-depth
                          racket-pretty-lambda
                          racket-smart-open-bracket-enable)))))
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
    (princ "```\n")
    (princ "</details>\n")))

(provide 'racket-bug-report)

;;; racket-bug-report.el ends here

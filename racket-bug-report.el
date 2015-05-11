;;; racket-bug-report.el

;; Copyright (c) 2013-2015 by Greg Hendershott.
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

(defconst racket--source-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

;;;###autoload
(defun racket-bug-report ()
  "Fill a buffer with data to make a racket-mode bug report."
  (interactive)
  (unless (memq major-mode '(racket-mode racket-repl-mode))
    (user-error "Please run this from a racket-mode or racket-repl-mode buffer."))
  (with-help-window "*racket-mode bug report*"
    (princ "Please copy and paste this into your bug report at:\n\n")
    (princ "  https://github.com/greghendershott/racket-mode/issues/new\n\n")
    (princ "```\n")
    (pp `(,@(mapcar (lambda (id)
                      (list id
                            (condition-case ()
                                (symbol-value id)
                              (error 'VOID-VARIABLE))))
                    `(emacs-version
                      system-type
                      major-mode
                      racket--source-dir
                      racket-racket-program
                      racket-raco-program
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
                      racket-smart-open-bracket-enable
                      racket-use-company-mode
                      ,@(mapcar #'car minor-mode-alist)))))
    (princ "```\n")))

(provide 'racket-bug-report)

;; Local Variables:
;; coding: utf-8
;; comment-column: 40
;; indent-tabs-mode: nil
;; require-final-newline: t
;; show-trailing-whitespace: t
;; End:

;;; racket-bug-report.el ends here

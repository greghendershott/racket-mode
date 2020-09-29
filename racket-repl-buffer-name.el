;;; racket-repl-buffer-name.el -*- lexical-binding: t; -*-

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

(require 'racket-custom)
(require 'racket-repl)
(require 'racket-util)

;;;###autoload
(defun racket-repl-buffer-name-shared ()
  "All `racket-mode' edit buffers share one `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'."
  (interactive)
  (setq-default racket-repl-buffer-name "*Racket REPL*"))

;;;###autoload
(defun racket-repl-buffer-name-unique ()
  "Each `racket-mode' edit buffer gets its own `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'."
  (interactive)
  (let ((name (format "*Racket REPL <%s>*" (racket--buffer-file-name))))
    (setq-local racket-repl-buffer-name name)))

;;;###autoload
(defun racket-repl-buffer-name-project ()
  "All `racket-mode' buffers in a project share a `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'.

The \"project\" is determined by `racket-project-root'."
  (interactive)
  (setq-local racket-repl-buffer-name
              (format "*Racket REPL <%s>*"
                      (racket-project-root (racket--buffer-file-name)))))

(defun racket-mode-maybe-offer-to-kill-repl-buffer ()
  "Maybe offer to kill a `racket-repl-mode' buffer.

A value for `kill-buffer-hook'.

Offer to kill a `racket-repl-mode' buffer when killing the last
`racket-mode' buffer using it. Although is not necessary to do
so, a user might want to do some \"cleanup\" -- especially if
they're using a `racket-repl-buffer-name-function' such as
`racket-repl-buffer-name-unique'."
  (when (eq major-mode 'racket-mode)
    (pcase (get-buffer racket-repl-buffer-name)
      ((and (pred bufferp) repl-buffer)
       (let ((n (1-
                 (length
                  (racket--buffers-using-repl racket-repl-buffer-name)))))
         (if (zerop n)
             (when (y-or-n-p
                    (format "No other buffers using %s -- also kill it? "
                            racket-repl-buffer-name))
               ;; They already said yes. Avoid another prompt about
               ;; killing the buffer's process.
               (pcase (get-buffer-process repl-buffer)
                 ((and (pred processp) repl-process)
                  (set-process-query-on-exit-flag repl-process nil)))
               (kill-buffer repl-buffer))
           (message "%s other buffer%s still using %s"
                    n
                    (if (= n 1) "" "s")
                    racket-repl-buffer-name)))))))

(defun racket--buffers-using-repl (repl-buffer-name)
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (eq major-mode 'racket-mode)
                       (equal racket-repl-buffer-name repl-buffer-name))))
              (buffer-list)))

(provide 'racket-repl-buffer-name)

;; racket-repl-buffer-name.el ends here

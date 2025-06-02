;;; racket-repl-buffer-name.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2025 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-back-end)
(require 'racket-custom)
(require 'racket-repl)
(require 'racket-util)
(require 'tramp)

;;;###autoload
(defun racket-call-racket-repl-buffer-name-function ()
  "Unless it already has a value, set the buffer-local value of
the variable `racket-repl-buffer-name' according to the user's
customization."
  (unless racket-repl-buffer-name ;#655
    (funcall (or (and (functionp racket-repl-buffer-name-function)
                      racket-repl-buffer-name-function)
                 #'racket-repl-buffer-name-shared))))

;;;###autoload
(defun racket-repl-buffer-name-shared ()
  "Share one `racket-repl-mode' buffer per back end.

A value for the variable `racket-repl-buffer-name-function'."
  (interactive)
  (setq-local racket-repl-buffer-name
              (format "*Racket REPL <%s>*"
                      (racket-back-end-name))))

;;;###autoload
(defun racket-repl-buffer-name-unique ()
  "Each `racket-mode' edit buffer gets its own `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'."
  (interactive)
  (let ((name (format "*Racket REPL <%s>*" (racket--buffer-file-name))))
    (setq-local racket-repl-buffer-name name)))

;;;###autoload
(defun racket-repl-buffer-name-project ()
  "Share one `racket-repl-mode' buffer per back end and per project.

A value for the variable `racket-repl-buffer-name-function'.

The \"project\" is determined by `racket-project-root'."
  (interactive)
  (setq-local racket-repl-buffer-name
              (format "*Racket REPL <%s %s>*"
                      (racket-back-end-name)
                      (racket--file-name-sans-remote-method
                       (racket-project-root (racket--buffer-file-name))))))

(defun racket-mode-maybe-offer-to-kill-repl-buffer ()
  "Maybe offer to kill a `racket-repl-mode' buffer.

Intended to be a buffer-local value for `kill-buffer-hook' in
`racket-mode' or `racket-hash-lang-mode' edit buffers.

Offer to kill an `racket-repl-mode' buffer when killing the last
edit buffer using it. Although is not necessary to do so, a user
might want to do some \"cleanup\" -- especially if they're using
a `racket-repl-buffer-name-function' such as
`racket-repl-buffer-name-unique'."
  (when (racket--edit-mode-p)
    (pcase (get-buffer racket-repl-buffer-name)
      ((and (pred bufferp) (pred buffer-live-p) repl-buffer)
       (let ((n (1-
                 (length
                  (racket--edit-buffers-using-repl racket-repl-buffer-name)))))
         (if (zerop n)
             (when (y-or-n-p
                    (format "No other buffers using %s -- also kill it? "
                            racket-repl-buffer-name))
               (kill-buffer repl-buffer))
           (message "%s other buffer%s still using %s"
                    n
                    (if (= n 1) "" "s")
                    racket-repl-buffer-name)))))))

(defun racket--edit-buffers-using-repl (repl-buffer-name)
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (racket--edit-mode-p)
                       (equal racket-repl-buffer-name repl-buffer-name))))
              (buffer-list)))

(provide 'racket-repl-buffer-name)

;; racket-repl-buffer-name.el ends here

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

;;; Some values suitable for `racket-repl-buffer-name-function',
;;; which set the variable `racket-repl-buffer-name'.

(require 'cl-extra)
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
  (let ((name (concat "*Racket REPL: " (racket--buffer-file-name) "*")))
    (setq-local racket-repl-buffer-name name)))

;;;###autoload
(defun racket-repl-buffer-name-project ()
  "Files belonging to a projectile project share a `racket-repl-mode' buffer.

A value for the variable `racket-repl-buffer-name-function'.

If no projectile project is found, then files in the same
directory share a REPL."
  (interactive)
  (let* ((dir  (file-name-directory (racket--buffer-file-name)))
         (root (or (and (fboundp 'projectile-project-root)
                        (projectile-project-root dir))
                   dir))
         (name (concat "*Racket REPL: " root "*")))
    (setq-local racket-repl-buffer-name name)))

(defun racket-mode-maybe-offer-to-kill-repl-buffer ()
  "A `kill-buffer-hook' function.

Offer to kill a `racket-repl-mode' buffer when killing the last
`racket-mode' buffer using it. Although is not necessary to do
so, a user might want to do some \"cleanup\" -- especially if
they're using a `racket-repl-buffer-name-function' such as
`racket-repl-buffer-name-unique'."
  (when (eq major-mode 'racket-mode)
    (let ((our-buffer                  (current-buffer))
          (our-racket-repl-buffer-name racket-repl-buffer-name))
      (unless (cl-some (lambda (buffer)
                         (with-current-buffer buffer
                           (and (eq major-mode 'racket-mode)
                                (not (equal our-buffer buffer))
                                (equal racket-repl-buffer-name
                                       our-racket-repl-buffer-name))))
                       (buffer-list))
        (pcase (get-buffer racket-repl-buffer-name)
          ((and (pred bufferp) repl-buffer)
           (when (y-or-n-p
                  (format "No other `racket-mode' buffers are using %s -- kill it, too? "
                          racket-repl-buffer-name))
             ;; They already said yes. Avoid another prompt about
             ;; killing the buffer's process.
             (pcase (get-buffer-process repl-buffer)
               ((and (pred processp) repl-process)
                (set-process-query-on-exit-flag repl-process nil)))
             (kill-buffer repl-buffer))))))))

(provide 'racket-repl-buffer-name)

;; racket-repl-buffer-name.el ends here

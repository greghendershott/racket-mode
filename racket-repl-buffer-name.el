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

(require 'racket-custom)
(require 'racket-repl)

;;;###autoload
(defun racket-repl-buffer-name-shared ()
  "All `racket-mode' edit buffers share one `racket-repl-mode' buffer."
  (interactive)
  (setq-default racket-repl-buffer-name "*Racket REPL*"))

;;;###autoload
(defun racket-repl-buffer-name-unique ()
  "Each `racket-mode' edit buffer gets its own `racket-repl-mode' buffer."
  (interactive)
  (let ((name (concat "*Racket REPL "
                      (buffer-file-name)
                      "*")))
    (setq-local racket-repl-buffer-name name)))

;;;###autoload
(defun racket-repl-buffer-name-project ()
  "Files belonging to a projectile project share a `racket-repl-mode' buffer"
  (interactive)
  (let* ((project (if (fboundp 'projectile-project-name)
                      (projectile-project-name)
                    (file-name-directory (buffer-file-name))))
         (name (concat "*Racket REPL project \""
                       project
                       "\"*")))
    (setq-local racket-repl-buffer-name name)))

(provide 'racket-repl-buffer-name)

;; racket-repl-buffer-name.el ends here

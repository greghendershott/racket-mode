;;; racket-complete.el -*- lexical-binding: t -*-

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

(require 'racket-common)

(defun racket--call-with-completion-prefix-positions (proc)
  (let ((beg (save-excursion (skip-syntax-backward "^-()>") (point))))
    (unless (or (eq beg (point-max))
                (member (char-syntax (char-after beg)) '(?\" ?\( ?\))))
      (condition-case nil
          (save-excursion
            (goto-char beg)
            (forward-sexp 1)
            (let ((end (point)))
              (and
               (<= (+ beg 2) end) ;prefix at least 2 chars
               (funcall proc beg end))))
        (scan-error nil)))))

(defun racket--in-require-form-p ()
  (save-excursion
    (save-match-data
      (racket--escape-string-or-comment)
      (let ((done nil)
            (result nil))
        (condition-case ()
            (while (not done)
              (backward-up-list)
              (when (looking-at (rx ?\( (or "require" "#%require")))
                (setq done t)
                (setq result t)))
          (scan-error nil))
        result))))

(provide 'racket-complete)

;; racket-complete.el ends here

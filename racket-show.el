;;; racket-show.el -*- lexical-binding: t -*-

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

(require 'racket-util)
(require 'racket-custom)
(require 'pos-tip)

(defun racket-show (val &optional pos)
  (dolist (f racket-show-functions)
    (funcall f val pos)))

(defun racket-show-echo-area (v &optional _pos)
  "Show things in the echo area.

A value for the variable `racket-show-functions'."
  (if v
      (message "%s" v)
    (message "")))

(defun racket-show-header-line (v &optional _pos)
  "Show things using a buffer header line.

A value for the variable `racket-show-functions'.

When there is nothing to show, keep a blank header-line. That
way, the buffer below doesn't \"jump up and down\" by a line as
messages appear and disappear. Only when V is nil do we remove
the header line."
  (setq-local header-line-format
              (and v (format "%s" (racket--only-first-line v)))))

(defun racket--only-first-line (str)
  (save-match-data
    (string-match (rx (group (* (not (any ?\n))))) str)
    (match-string 1 str)))

(defun racket-show-pos-tip (v &optional pos)
  "Show things using `pos-tip-show' if available.

A value for the variable `racket-show-functions'."
  (when (racket--pos-tip-available-p)
    (if (racket--non-empty-string-p v)
        (pos-tip-show v nil pos)
      (pos-tip-hide))))

(defun racket--pos-tip-available-p ()
  "Is `pos-tip' available and expected to work on current frame?"
  (and (fboundp 'x-hide-tip)
       (fboundp 'x-show-tip)
       (not (memq window-system (list nil 'pc)))))

(provide 'racket-show)

;; racket-show.el ends here

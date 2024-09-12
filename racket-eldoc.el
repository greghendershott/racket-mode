;;; racket-eldoc.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(defun racket--eldoc-do-callback (callback thing str)
  (if str
      (funcall callback
               str
               :thing thing
               :face 'font-lock-function-name-face)
    (funcall callback nil))
  t)

(provide 'racket-eldoc)

;; racket-eldoc.el ends here

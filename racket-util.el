;;; racket-util.el

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

(require 's)

;;; trace

(defvar racket--trace-enable nil)

(defun racket--trace (p &optional s retval)
  (when racket--trace-enable
    (let ((b (get-buffer-create "*Racket Trace*"))
          (deactivate-mark deactivate-mark))
      (save-excursion
        (save-restriction
          (with-current-buffer b
            (insert p ": " (if (stringp s) s (format "%S" s)) "\n"))))))
  retval)

(defun racket--toggle-trace (arg)
  (interactive "P")
  (setq racket--trace-enable (or arg (not racket--trace-enable)))
  (if racket--trace-enable
      (message "Racket trace on")
    (message "Racket trace off"))
  (let ((b (get-buffer-create "*Racket Trace*")))
    (pop-to-buffer b t t)
    (setq truncate-lines t)))

;;; racket--easy-keymap-define

(defun racket--easy-keymap-define (spec)
  "Make a sparse keymap with the bindings in SPEC.

This is simply a way to DRY many calls to `define-key'.

SPEC is
  (list (list key-or-keys fn) ...)

where key-or-keys is either a string given to `kbd', or (for the
case where multiple keys bind to the same command) a list of such
strings."
  (let ((m (make-sparse-keymap)))
    (mapc (lambda (x)
            (let ((keys (if (listp (car x))
                            (car x)
                          (list (car x))))
                  (fn (cadr x)))
              (mapc (lambda (key)
                      (define-key m (kbd key) fn))
                    keys)))
          spec)
    m))

;;; racket--module-level-form-start

(defun racket--module-level-form-start ()
  "Start position of the module-level form point is within.

A module-level form is the outermost form not nested in a Racket
module form.

If point is not within a module-level form, returns nil.

If point is already exactly at the start of a module-level form,
-- i.e. on the opening ?\( -- returns nil.

If point is within a string or comment, returns nil.

This is NOT suitable for the variable `syntax-begin-function'
because it (i) doesn't move point, and (ii) doesn't know how to
find the start of a string or comment."
  (save-excursion
    (ignore-errors
      (let ((result nil)
            (parse-sexp-ignore-comments t))
        (while (ignore-errors
                 (goto-char (scan-lists (point) -1 1))
                 (unless (looking-at (rx (syntax ?\() "module" (zero-or-one (any ?* ?+))))
                   (setq result (point)))
                 t))
        result))))

(provide 'racket-util)

;; racket-util.el ends here

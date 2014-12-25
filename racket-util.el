;;; racket-util.el

;; Copyright (c) 2013-2014 by Greg Hendershott.
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


;;; trace

(defvar racket--trace-enable nil)

(defun racket--trace (p &optional s retval)
  (if racket--trace-enable
      (let ((b (get-buffer-create "*Racket Trace*"))
	    (deactivate-mark deactivate-mark))
	(save-excursion
	  (save-restriction
	    (set-buffer b)
	    (insert p ": " (if (stringp s) s (format "%S" s)) "\n")))))
  retval)

(defun racket--toggle-trace (arg)
  (interactive "P")
  (setq racket--trace-enable (or arg (not racket--trace-enable)))
  (if racket--trace-enable
      (message "Racket trace on"))
  (let ((b (get-buffer "*Racket Trace*")))
    (if b
	(if racket--trace-enable
	    (kill-buffer b)
	  (pop-to-buffer b t t)
	  (setq truncate-lines t)))))


;;; racket--symbol-at-point-or-prompt

(defun racket--symbol-at-point-or-prompt (prefix prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or prefix is true."
  (let ((sap (symbol-at-point)))
    (if (or prefix (not sap))
        (read-from-minibuffer prompt (if sap (symbol-name sap) ""))
      sap)))

(provide 'racket-util)

;; racket-util.el ends here

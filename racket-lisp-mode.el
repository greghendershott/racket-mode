;;; racket-lisp-mode.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Some packages like paredit and lispy directly call `lisp-mode'
;; functions `lisp-indent-line' and `indent-sexp'. (As opposed to
;; calling functions like `indent-line-to' and `prog-indent-sexp' that
;; a mode can specialize via `indent-line-function' and
;; `indent-region-function'.)
;;
;; Although that's fine for modes like `scheme-mode' derived from
;; `lisp-mode', `racket-mode' is not.
;;
;; Therefore if users want to use such packages hardwired to call
;; those two `lisp-mode' function, AFAICT we have no choice but to
;; advise those two functions. :(
;;
;; Furthermore lisp-mode's `indent-sexp' differs from
;; `prog-indent-sexp' as explained below in the doc string for
;; `racket-indent-sexp-contents'.

(require 'lisp-mode)
(require 'racket-util)

(defun racket--lisp-indent-line-advice (orig &rest args)
  (apply (if (racket--mode-edits-racket-p)
             indent-line-function
           orig)
         args))

(advice-add #'lisp-indent-line :around #'racket--lisp-indent-line-advice)

(defun racket--indent-sexp-advice (orig &rest args)
  (apply (if (racket--mode-edits-racket-p)
             #'racket-indent-sexp-contents
           orig)
         args))

(advice-add #'indent-sexp :around #'racket--indent-sexp-advice)

(defun racket-indent-sexp-contents ()
  "Indent each line of the sexp starting just after point.

Unlike `prog-indent-sexp', which indents the entire sexp, this
does /not/ indent the first line at point, just subsequent lines
if any. In other words it does not indent the sexp as a whole,
just its contents. In this regard it behaves like the
`lisp-mode'-specific function `indent-sexp'."
  (interactive)
  (condition-case _
      (let ((beg-of-2nd-line   (save-excursion (forward-line 1) (point)))
            (end-of-expression (save-excursion (forward-sexp 1) (point))))
        (when (< beg-of-2nd-line end-of-expression)
          (indent-region beg-of-2nd-line end-of-expression)))
    (scan-error nil)))

(provide 'racket-lisp-mode)

;; racket-lisp-mode.el ends here

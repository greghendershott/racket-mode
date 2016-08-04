;;; racket-imenu.el

;; Copyright (c) 2013-2016 by Greg Hendershott.
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

(require 'cl-lib)
(require 'imenu)

(defun racket--variables-imenu ()
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-create-index-function)
       #'racket--imenu-create-index-function))

(defun racket--imenu-create-index-function ()
  "A function for the variable `imenu-create-index-function'.

Knows about Racket module forms, and prefixes identiers with
their parent module name(s)."
  (save-excursion
    (goto-char (point-min))
    (racket--next-sexp)
    (racket--walk-sexps "")))

(defun racket--walk-sexps (prefix)
  "With point at the start of a sexp, walk all the sepxs.

`racket--menu-sexp' will walk into Racket module forms and call
us recursively."
  (cl-loop append (racket--menu-sexp prefix) into xs
           while (racket--next-next-sexp)
           finally return xs))

(defun racket--menu-sexp (prefix)
  "Return the identifier for the sexp at point if any, else nil.

If sexp at point is a Racket module form, descend and walk that."
  (cond ((looking-at (rx "(define" (* (or (syntax word) (syntax symbol)))
                         (+ (syntax whitespace))
                         (? ?\()
                         (group (+ (or (syntax word) (syntax symbol))))))
         (let* ((beg (match-beginning 1))
                (beg (if imenu-use-markers
                         (save-excursion (goto-char beg) (point-marker))
                       beg)))
           (list (cons (concat prefix (match-string-no-properties 1))
                       beg))))
        ((looking-at (rx "(module" (? (any ?+ ?*))
                         (+ (syntax whitespace))
                         (group (+ (or (syntax word) (syntax symbol))))))
         (save-excursion
           (goto-char (match-end 1))
           (racket--next-sexp)
           (racket--walk-sexps (concat prefix (match-string-no-properties 1) ":"))))
        (t nil)))

(defun racket--next-sexp ()
  "Move point to start of next sexp in buffer."
  (forward-sexp 1)
  (forward-sexp -1))

(defun racket--next-next-sexp ()
  "If another sexp, move point to its start and return t, else return nil."
  (condition-case nil
      (progn
        (forward-sexp 1)
        (let ((orig (point)))
          (forward-sexp 1)
          (if (or (eobp) (equal orig (point)))
              nil
            (forward-sexp -1)
            t)))
    (scan-error nil)))

(provide 'racket-imenu)

;;; racket-imenu.el ends here

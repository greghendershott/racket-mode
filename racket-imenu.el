;;; racket-imenu.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2021 by Greg Hendershott.
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

(defun racket-imenu-create-index-function ()
  "A function for the variable `imenu-create-index-function'."
  (save-excursion
    (goto-char (point-min))
    (racket--imenu-goto-start-of-current-sexp)
    (racket--imenu-walk)))

(defun racket--imenu-walk ()
  "With point at the start of a sexp, walk all the sepxs.

Note that `racket--imenu-item' will walk into Racket module forms
and call us recursively."
  (cl-loop append (racket--imenu-item) into xs
           while (racket--imenu-goto-start-of-following-sexp)
           finally return xs))

(defun racket--imenu-item ()
  "Return the identifier for the sexp at point if any, else nil.

If sexp at point is a Racket module form create a submenu."
  (cond ((looking-at (rx "(define" (* (or (syntax word)
                                          (syntax symbol)
                                          (syntax punctuation)))
                         (+ (syntax whitespace))
                         (* ?\()
                         (group (+ (or (syntax word)
                                       (syntax symbol)
                                       (syntax punctuation))))))
         (list (cons (match-string-no-properties 1)
                     (if imenu-use-markers
                         (copy-marker (match-beginning 1))
                       (match-beginning 1)))))
        ((looking-at (rx "(module" (? (any ?+ ?*))
                         (+ (syntax whitespace))
                         (group (+ (or (syntax word)
                                       (syntax symbol)
                                       (syntax punctuation))))))
         (save-excursion
           (goto-char (match-end 1))
           (racket--imenu-goto-start-of-current-sexp)
           (list (cons (concat "Module: " (match-string-no-properties 1))
                       (racket--imenu-walk )))))
        (t nil)))

(defun racket--imenu-goto-start-of-current-sexp ()
  (ignore-errors
    (forward-sexp 1)
    (forward-sexp -1)))

(defun racket--imenu-goto-start-of-following-sexp ()
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

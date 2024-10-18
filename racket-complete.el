;;; racket-complete.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-common)

(defun racket--call-with-completion-prefix-positions (proc)
  (cl-flet ((maybe-call (beg end)
              (when (and (<= (+ beg 2) end) ;prefix at least 2 chars
                         (eq (line-number-at-pos beg)
                             (line-number-at-pos end)))
                (funcall proc beg end))))
    (if forward-sexp-function ;not necessarily sexp lang
        (condition-case _
            (save-excursion
              (let ((beg (progn (forward-sexp -1) (point)))
                    (end (progn (forward-sexp  1) (point))))
                (maybe-call beg end)))
          (error nil))
      (let ((beg (save-excursion (skip-syntax-backward "^-()>") (point))))
        (unless (or (eq beg (point-max))
                    (member (char-syntax (char-after beg)) '(?\" ?\( ?\))))
          (condition-case _
              (save-excursion
                (goto-char beg)
                (forward-sexp 1)
                (maybe-call beg (point)))
            (error nil)))))))

(defun racket--in-require-form-p ()
  (unless forward-sexp-function ;not necessarily sexp lang
    (save-excursion
      (save-match-data
        (racket--escape-string-or-comment)
        (let ((done nil)
              (result nil))
          (condition-case _
              (while (not done)
                (backward-up-list)
                (when (looking-at-p (rx ?\( (or "require" "#%require")))
                  (setq done t)
                  (setq result t)))
            (scan-error nil))
          result)))))

;;; Completion tables with "category" metadata

(defconst racket--identifier-category 'racket-identifier
  "Value for category metadata of identifier completion tables.")

;; Suggest default; can customize via `completion-category-overrides'.
(add-to-list 'completion-category-defaults
             `(,racket--identifier-category (styles basic)))

(defconst racket--module-category 'racket-module
  "Value for category metadata of module completion tables.")

;; Suggest default; can customize via `completion-category-overrides'.
(add-to-list 'completion-category-defaults
             `(,racket--module-category (styles basic)))

(defun racket--completion-table (completions &optional metadata)
  "Like `completion-table-dynamic' but also supplies metadata.

METADATA defaults to `((category . ,`racket--identifier-category')).

Although sometimes completion metadata is specified as properties
in a `completion-at-point-functions' item, sometimes that is
insufficient or irrelevant -- as with category metadata, or, when
CAPF isn't involved and instead the completion table is given
directly to `completing-read'.

Supplying category metadata allows the user to configure a
completion matching style for that category. It also prevents
third party packages like marginalia from misclassifying and
displaying inappropriate annotations."
  (lambda (prefix predicate action)
    (pcase action
      ('metadata
       (cons 'metadata
             (or metadata
                 `((category . ,racket--identifier-category)))))
      (_
       (complete-with-action action completions prefix predicate)))))

(provide 'racket-complete)

;; racket-complete.el ends here

;;; racket-complete.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2023 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'racket-common)

(defun racket--call-with-completion-prefix-positions (proc)
  (if forward-sexp-function ;not necessarily sexp lang
      (condition-case _
          (save-excursion
            (let ((beg (progn (forward-sexp -1) (point)))
                  (end (progn (forward-sexp  1) (point))))
              (when (<= (+ beg 2) end)  ;prefix at least 2 chars
                (funcall proc beg end))))
        (error nil)))
  (let ((beg (save-excursion (skip-syntax-backward "^-()>") (point))))
    (unless (or (eq beg (point-max))
                (member (char-syntax (char-after beg)) '(?\" ?\( ?\))))
      (condition-case _
          (save-excursion
            (goto-char beg)
            (forward-sexp 1)
            (let ((end (point)))
              (when (<= (+ beg 2) end) ;prefix at least 2 chars
               (funcall proc beg end))))
        (error nil)))))

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

(defun racket--completion-table (completions &optional metadata)
  "Like `completion-table-dynamic' but also metadata.

METADATA defaults to `((category . ,`racket--identifier-category')).

Category metadata needs to be returned by the completion table
function itself, unlike metadata supplied as properties in the
`completion-at-point-functions' list.

Supplying category metadata allows the user to configure a
completion matching style for that category."
  (lambda (prefix predicate action)
    (if (eq action 'metadata)
        (cons 'metadata (or metadata `((category . ,racket--identifier-category))))
      (complete-with-action action completions prefix predicate))))

(provide 'racket-complete)

;; racket-complete.el ends here

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

(defun racket--make-affix (specs &optional prop)
  "Make an affixation-function to show completion annotations.

For more information about affixation-function completion
metadata, see Info node `(elisp)Programmed Completion'.

PROP is the symbol name of a text property that must be attached
to each of the completion candidate strings. The value of the
property is a list of strings -- each string is a suffix column
value to show as an annotation. The list length must be the same
for all candidate strings. The property name defaults to
\\='racket-affix.

SPECS is a vector of specs for each column -- one for the
completion candidate string, plus the length of the list of
suffix columns. Each spec may be an integer, which is a minimum
width, or [WIDTH FACE]. Note: The width is N/A for the last
suffix column. The face is N/A for the first column, which shows
the candidate string. For suffix columns, the face defaults to
completions-anntoations. An explicit nil value in the spec means
not to add a face, because the string is already propertized with
one.

The affixation-function arranges for each suffix column to be
aligned, considering the minimum width and the maximum width of
the previous column.

When a candidate string ends with text made invisible by a
\\='display \"\" property -- as is done by
`racket--doc-index-make-alist' -- that text is ignored for
purposes of calculating widths."
  ;; Note: Below we use `cl-loop' because `seq-do-indexed' and
  ;; `seq-map-indexed' are unavailable in Emacs 25.
  (let ((min-widths (cl-loop
                     for spec across specs
                     collect (pcase spec
                               (`[,width ,_face] width)
                               ((and (pred numberp) width) width)
                               (_ 0))))
        (suffix-faces (cl-loop for spec across (seq-drop specs 1)
                               collect (pcase spec
                                         (`[,_width ,face] face)
                                         (_ 'completions-annotations))))
        (prop (or prop 'racket-affix)))
    (lambda (strs)
      (let* ((max-widths (apply #'vector min-widths))
             (rows
              (cl-loop
               for str in strs
               collect
               (let ((visible-str
                      (substring str
                                 0
                                 (text-property-any 0 (length str)
                                                    'display ""
                                                    str)))
                     (suffixes (get-text-property 0 prop str)))
                 ;; Mutate `max-widths'.
                 (cl-loop
                  for col in (cons visible-str suffixes)
                  for ix from 0
                  do (aset max-widths ix
                           (max (aref max-widths ix)
                                (1+ (length col)))))
                 (cons str suffixes))))
             (suffix-offsets
              (let ((offset 0))
                (cl-loop
                 for max-width across max-widths
                 collect
                 (setq offset (+ offset max-width))))))
        (cl-loop
         for row in rows
         collect
         (pcase-let*
             ((`(,str . ,suffixes) row)
              (suffixes-str
               (cl-loop
                for suffix in suffixes
                for offset in suffix-offsets
                for face in suffix-faces
                concat
                (concat
                 (propertize " "
                             'display
                             `(space :align-to ,offset))
                 (if face
                     (propertize (or suffix "")
                                 'face face)
                   (or suffix ""))))))
           (list str "" suffixes-str)))))))

(provide 'racket-complete)

;; racket-complete.el ends here

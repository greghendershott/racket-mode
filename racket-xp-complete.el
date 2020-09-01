;;; racket-xp-complete.el -*- lexical-binding: t -*-

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

(require 'racket-complete)
(require 'racket-describe)

(defvar racket--xp-binding-completions nil
  "Completion candidates that are bindings.
Set by `racket-xp-mode'. Used by `racket-xp-complete-at-point'.")

(defvar racket--xp-module-completions nil
  "Completion candidates that are available collection module paths.
Set by `racket-xp-mode'. Used by `racket-xp-complete-at-point'.")

(defun racket-xp-complete-at-point ()
  "A value for the variable `completion-at-point-functions'.

- Within a textually apparent \"require\" form, when completing:

  - A symbol immediately after an opening paren: Candidates are
    names of require transformers.

  - Another symbol: Candidates are absolute module paths like
    \"racket/path\".

  - Anything `thing-at-point' thinks is a filename: Candidates
    are from `completion-file-name-table'.

- Otherwise, when completing a symbol: Candidates are bindings as
  found by drracket/check-syntax plus our own back end analysis
  of imported bindings."
  (cond ((racket--in-require-form-p)
         (or (racket--call-with-completion-prefix-positions
              (lambda (beg end)
                (if (eq ?\( (char-syntax (char-before beg)))
                    (racket--xp-capf-require-transformers beg end)
                  (racket--xp-capf-absolute-module-paths beg end))))
             (racket--xp-capf-relative-module-paths)))
        (t
         (racket--call-with-completion-prefix-positions
          #'racket--xp-capf-bindings))))

(defun racket--xp-capf-bindings (beg end)
  (list beg
        end
        (completion-table-dynamic
         (lambda (prefix)
           (all-completions prefix racket--xp-binding-completions)))
        :predicate          #'identity
        :exclusive          'no
        :company-location   (racket--xp-make-company-location-proc)
        :company-doc-buffer (racket--xp-make-company-doc-buffer-proc)))

(defun racket--xp-capf-require-transformers (beg end)
  "Note: Currently this returns too many candidates -- all
available bindings, not just those that are require transformers.
Although not ideal, I think it's less-worse than having some
hardwired list of require transformers. In general with
completion candidates, if you have to err, better to err on the
side of too many not too few. Having said that, someday maybe our
back end could give us the exact subset of available bindings
that are require transformers."
  (racket--xp-capf-bindings beg end))

(defun racket--xp-capf-absolute-module-paths (beg end)
  (list beg
        end
        (completion-table-dynamic
         (lambda (prefix)
           (all-completions prefix racket--xp-module-completions)))
        :exclusive 'no))

(defun racket--xp-capf-relative-module-paths ()
  (pcase (thing-at-point 'filename t)
    ((and (pred stringp) str)
     (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'filename)))
       (pcase (completion-file-name-table str #'file-exists-p t)
         ((and (pred listp) table)
          (let* ((dir (file-name-directory str))
                 (table (mapcar (lambda (v) (concat dir v)) ;#466
                                table)))
            (list beg
                  end
                  table
                  :exclusive 'no))))))))

(defun racket--xp-make-company-location-proc ()
  (when (racket--cmd-open-p)
    (let ((how (buffer-file-name)))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (pcase (racket--cmd/await nil `(def ,how ,str))
            (`(,path ,line ,_) (cons path line))))))))

(defun racket--xp-make-company-doc-buffer-proc ()
  (when (racket--cmd-open-p)
    (let ((how (buffer-file-name)))
      (lambda (str)
        (let ((str (substring-no-properties str)))
          (racket--do-describe how nil str))))))

(provide 'racket-xp-complete)

;; racket-xp-complete.el ends here

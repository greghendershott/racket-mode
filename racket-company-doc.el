;;; racket-company-doc.el -*- lexical-binding: t -*-

;; Copyright (c) 2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'seq)
(require 'shr)
(require 'racket-back-end)
(require 'racket-describe)
(require 'racket-scribble)

(defun racket--company-doc-buffer (how str)
  (pcase (racket--cmd/await (racket--repl-session-id)
                            `(describe ,(racket-how-front-to-back how) ,str))
    (`(,(and path (pred stringp)) . ,anchor)
     (let ((path (racket-file-name-back-to-front path))
           (name "*racket-company-doc-buffer*"))
       (when-let (buf (get-buffer name))
         (when (buffer-live-p buf)
           (kill-buffer buf)))
       (with-current-buffer (get-buffer-create name)
         (goto-char (point-min))
         (racket--scribble-path+anchor-insert path anchor)
         (goto-char (point-min))
         (setq buffer-read-only t)
         (current-buffer))))))

(defun racket--scribble-path+anchor-insert (path anchor)
  (with-temp-message (format "Getting and formatting documentation %s %s ..."
                             path anchor)
    (let* ((tramp-verbose 2)            ;avoid excessive messages
           (dom   (racket--html-file->dom path))
           (body  (racket--scribble-body dom))
           (elems (racket--company-elements-for-anchor body anchor))
           (dom   `(div () ,@elems))
           (dom   (racket--walk-dom dom)))
      (ignore tramp-verbose)
      (save-excursion
        (let ((shr-use-fonts nil)
              (shr-external-rendering-functions `((span . ,#'racket-render-tag-span)))
              (shr-width 76)) ;for company-quickhelp-mode
          (shr-insert-document dom)))
      (while (re-search-forward (string racket--scribble-temp-nbsp) nil t)
        (replace-match " " t t)))))

(defun racket--company-elements-for-anchor (xs anchor)
  "Return the subset of XS dom elements pertaining to ANCHOR."
  (while (and xs (not (racket--anchored-element (car xs) anchor)))
    (setq xs (cdr xs)))
  (and xs
       (let ((result nil))
         (push (car xs) result)
         (setq xs (cdr xs))
         (while (and xs (not (or (racket--heading-element (car xs))
                                 (racket--anchored-element (car xs)))))
           (push (car xs) result)
           (setq xs (cdr xs)))
         (reverse result))))

(defun racket--heading-element (x)
  (and (listp x)
       (memq (car x) '(h1 h2 h3 h4 h5 h6))))

(defun racket--anchored-element (x &optional name)
  (pcase x
    (`(a ((name . ,a)) . ,_) (or (not name) (equal name a)))
    (`(,_tag ,_as . ,es) (seq-some (lambda (v) (racket--anchored-element v name))
                                   es))))

(provide 'racket-company-doc)

;; racket-company-doc.el ends here

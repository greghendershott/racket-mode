;;; racket-company-doc.el -*- lexical-binding: t -*-

;; Copyright (c) 2022 by Greg Hendershott.
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

(require 'shr)
(require 'racket-describe)

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
           (mains (racket--scribble-main-elements path dom))
           (mains (racket--scribble-mains-for-anchor mains anchor))
           (dom   `(div () ,@mains))
           (dom   (racket--walk-dom dom)))
      (save-excursion
        (let ((shr-use-fonts nil)
              (shr-external-rendering-functions `((span . ,#'racket-render-tag-span)))
              (shr-width 76)) ;for company-quickhelp-mode
          (shr-insert-document dom)))
      (while (re-search-forward (string racket--scribble-temp-nbsp) nil t)
        (replace-match " " t t)))))

(defun racket--scribble-mains-for-anchor (mains anchor)
  "Return the subset of MAINS dom elements pertaining to ANCHOR."
  (while (and mains (not (racket--anchored-element (car mains) anchor)))
    (setq mains (cdr mains)))
  (and mains
       (let ((result nil))
         (push (car mains) result)
         (setq mains (cdr mains))
         (while (and mains (not (or (racket--heading-element (car mains))
                                    (racket--anchored-element (car mains)))))
           (push (car mains) result)
           (setq mains (cdr mains)))
         (reverse result))))

(defun racket--heading-element (x)
  (and (listp x)
       (memq (car x) '(h1 h2 h3 h4 h5 h6))))

(defun racket--anchored-element (x &optional name)
  (cl-labels
      ((anchor
        (xs)
        (cl-some (lambda (x)
                   (pcase x
                     (`(a ((name . ,a)) . ,_) (or (not name) (equal name a)))
                     (`(,_tag ,_as . ,es) (anchor es))))
                 xs)))
    (pcase x
      (`(div ((class . "SIntrapara")) . ,es)
       (anchor es))
      ((and `(blockquote ((class . "leftindent"))
                         (p)
                         (div ((class . "SIntrapara"))
                              (blockquote ((class "SVInsetFlow"))
                                          (table ,as . ,es)))))
       (when (member '(class . "boxed RBoxed") as)
         (anchor es))))))

(provide 'racket-company-doc)

;; racket-company-doc.el ends here

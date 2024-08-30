;;; racket-company-doc.el -*- lexical-binding: t -*-

;; Copyright (c) 2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-macs)
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
           (dom (racket--scribble-path->shr-dom path))
           (dom (racket--company-elements-for-anchor dom anchor)))
      (ignore tramp-verbose)
      (save-excursion
        (let ((shr-use-fonts nil)
              (shr-external-rendering-functions `((span . ,#'racket-render-tag-span)))
              (shr-width 76)) ;for company-quickhelp-mode
          (shr-insert-document dom)))
      (while (re-search-forward (string racket--scribble-temp-nbsp) nil t)
        (replace-match " " t t)))))

(defun racket--company-elements-for-anchor (dom anchor)
  "Return the subset of DOM elements pertaining to ANCHOR."
  (cl-labels
      ((heading-p (x)
         (memq (dom-tag x) '(h1 h2 h3 h4 h5 h6)))
       (anchor-p (x name)
         (if (and (eq 'racket-anchor (dom-tag x))
                  (or (not name) (equal name (dom-attr x 'name))))
             t
           (seq-some (lambda (v) (anchor-p v name))
                     (dom-non-text-children x)))))
    ;; Consider immediate children of the "main" div.
    (let ((result nil)
          (xs (dom-children (car (dom-by-class dom "main\\'")))))
      ;; Discard elements before the one containing a matching anchor.
      (while (and xs (not (anchor-p (car xs) anchor)))
        (setq xs (cdr xs)))
      ;; Accumulate result up to another anchor or a heading.
      (when xs
        (push (car xs) result)
        (setq xs (cdr xs))
        (while (and xs (not (or (heading-p (car xs))
                                (anchor-p (car xs) nil))))
          (push (car xs) result)
          (setq xs (cdr xs))))
      (racket--walk-dom `(div () ,@(reverse result))))))

(provide 'racket-company-doc)

;; racket-company-doc.el ends here

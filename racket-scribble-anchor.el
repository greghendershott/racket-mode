;;; racket-scribble-anchor.el -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-macs)
(require 'ring)
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

(defvar racket--path+anchor-ring (make-ring 16)
  "A small MRU cache of the N most recent strings.
Each ring item is (cons (cons path anchor) str).")

(defun racket--path+anchor->string (path anchor)
  "A wrapper for `racket--scribble-path+anchor-insert'.
Uses `racket--path+anchor-cache'."
  (pcase (seq-some (lambda (item)
                     (and (equal (car item) (cons path anchor))
                          item))
                   (ring-elements racket--path+anchor-ring))
    ((and `(,_path+anchor . ,str) item)
     ;; Re-insert as newest.
     (ring-remove+insert+extend racket--path+anchor-ring item)
     str)
    (_
     (let* ((str (with-temp-buffer
                   (racket--scribble-path+anchor-insert path anchor)
                   (buffer-string)))
            (item (cons (cons path anchor) str)))
       ;; Insert as newest; oldest discarded when ring full.
       (ring-insert racket--path+anchor-ring item)
       str))))

(defun racket--scribble-path+anchor-insert (path anchor)
  (let* ((tramp-verbose 2) ;avoid excessive tramp messages
         (dom (racket--html-file->dom path))
         (dom (racket--elements-for-anchor dom anchor))
         (dom (racket--massage-scribble-dom path
                                            (file-name-directory path)
                                            dom)))
    (ignore tramp-verbose)
    (save-excursion
      (let ((shr-use-fonts nil)
            (shr-external-rendering-functions `((span . ,#'racket-render-tag-span)))
            (shr-width 76))
        (shr-insert-document dom)))
    (while (re-search-forward (string racket--scribble-temp-nbsp) nil t)
      (replace-match " " t t))))

(defun racket--elements-for-anchor (dom anchor)
  "Return the subset of DOM elements pertaining to ANCHOR."
  (cl-flet
      ((anchor-p (node name)
         (dom-search node
                     (lambda (node)
                       (and (eq 'a (dom-tag node))
                            (equal name (dom-attr node 'name))))))
       (boxed-p (node)
         (dom-search node
                     (lambda (node)
                       (and (eq 'table (dom-tag node))
                            (equal "boxed RBoxed" (dom-attr node 'class))))))
       (heading-p (node)
         (memq (dom-tag node) '(h1 h2 h3 h4 h5 h6))))
    ;; Consider immediate children of the "main" div.
    (let ((result nil)
          (xs (dom-children
               (dom-search (dom-child-by-tag dom 'body)
                           (lambda (node)
                             (and (eq 'div (dom-tag node))
                                  (equal "main" (dom-attr node 'class))))))))
      ;; Discard elements before the one containing a matching anchor.
      (while (and xs (not (anchor-p (car xs) anchor)))
        (setq xs (cdr xs)))
      ;; Accumulate result up to an element containing an RBoxed table
      ;; or heading.
      (when xs
        (push (car xs) result)
        (setq xs (cdr xs))
        (while (and xs (not (or (heading-p (car xs))
                                (boxed-p (car xs)))))
          (push (car xs) result)
          (setq xs (cdr xs))))
      `(div () ,@(reverse result)))))

(provide 'racket-scribble-anchor)

;; racket-scribble-anchor.el ends here

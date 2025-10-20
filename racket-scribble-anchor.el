;;; racket-scribble-anchor.el -*- lexical-binding: t -*-

;; Copyright (c) 2022-2025 by Greg Hendershott.
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
      ((bluebox-p (node)
         (and
          (and (eq 'div (dom-tag node))
               (equal "SIntrapara" (dom-attr node 'class)))
          (dom-search node
                      (lambda (node)
                        (and (eq 'table (dom-tag node))
                             (equal "boxed RBoxed" (dom-attr node 'class)))))))
       (section-or-heading-p (node)
         (memq (dom-tag node) '(section h1 h2 h3 h4 h5 h6))))
    ;; Note: This is not optimized, due to using `dom-search' and
    ;; `dom-parent'. It would be faster to hand-code a `dom-search'
    ;; that, while descending, remembers the ancestor bluebox and its
    ;; siblings.
    (let* (;; Drill all the way down to the anchor element.
           (a (car (dom-search dom
                               (lambda (node)
                                 (and (eq 'a (dom-tag node))
                                      (equal anchor (dom-attr node 'name)))))))
           ;; Nav back up to its ancestor `bluebox-p' element.
           (bluebox (let ((n (dom-parent dom a)))
                      (while (and n (not (bluebox-p n)))
                        (setq n (dom-parent dom n)))
                      n))
           ;; Get all siblings at same level as bluebox.
           (siblings (dom-parent dom bluebox))
           (result nil))
      ;; Discard siblings before the bluebox.
      (while (and siblings (not (eq (car siblings) bluebox)))
        (setq siblings (cdr siblings)))
      ;; Accumulate the bluebox and subsequent siblings up to but not
      ;; including some other bluebox, section, or heading.
      (when siblings
        (push (car siblings) result)
        (setq siblings (cdr siblings))
        (while (and siblings (not (or (bluebox-p (car siblings))
                                      (section-or-heading-p (car siblings)))))
          (push (car siblings) result)
          (setq siblings (cdr siblings))))
      `(div () ,@ (reverse result)))))

(provide 'racket-scribble-anchor)

;; racket-scribble-anchor.el ends here

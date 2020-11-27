;;; racket-complete.el -*- lexical-binding: t -*-

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

(require 'racket-cmd)
(require 'racket-describe)
(require 'racket-browse-url)

(defvar racket--make-index.rkt
  (expand-file-name "make-index.rkt" racket--rkt-source-dir)
  "Pathname of script to make index.")

(defvar racket--search-index '()
  "Racket search index")

(defun racket--search-format (words type-expr)
  (when type-expr
    (if (cdr type-expr)
        (format "%s: %s"
                (car words)
                (string-join
                 (cadr type-expr)
                 ", "))
      (car words))))

(defun racket--search-filter (proc str)
  (let
      ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; We use a newline as a delimiter for a new entry
        (if (string-match-p "\n" str)
            (let
                ;; Don’t read the last line — it may be incomplete
                ((line-pos
                  (save-excursion
                    (insert str)
                    (cons
                     (line-beginning-position)
                     (point)))))
              (cl-loop
               while (< (point) (car line-pos))
               do (progn
                    (goto-char
                     (line-beginning-position))
                    (pcase (read buf)
                      (`(,words ,type-expr ,link)
                       (let
                           ((e (racket--search-format
                                words
                                type-expr)))
                         (when e
                           (setq racket--search-index
                                 (cons
                                  (cons e link)
                                  racket--search-index))))))
                    (forward-line)))
              (goto-char (cdr line-pos)))
          (insert str))))))

(defun racket--search-make-index ()
    (let
        ((buf (get-buffer-create " *racket-index*")))
      (with-current-buffer buf
        (erase-buffer)
        (setq racket--index '())
        (make-process
         :name "racket"
         :buffer buf
         :sentinel
         (lambda (proc _)
           (unless (process-live-p proc)
             (let
                 ;; Finalize the index as a hash table
                 ((hash (make-hash-table
                         :size (length racket--search-index)
                         :test #'equal)))
               (cl-loop
                for e in racket--search-index
                do (puthash (car e) (cdr e) hash))
               (setq racket--search-index hash))
             (kill-buffer buf)))
         :connection-type 'pipe
         :filter #'racket--search-filter
         :command (list racket-program
                        racket--make-index.rkt)))))

(defun racket-search (key)
  (interactive
   (list
    (completing-read "Describe:" racket--search-index)))
  (when (hash-table-p racket--search-index)
    (let
        ((link (gethash key racket--search-index)))
      (racket--do-describe
       link
       (racket--repl-session-id)
       "" ; This is a dummy string because we have an absolute link
       t
       (lambda ()
         (racket-browse-url
          (concat "file://" (car link) "#" (cdr link))))
       nil))))

(provide 'racket-search)

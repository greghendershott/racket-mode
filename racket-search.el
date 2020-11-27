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

(defvar racket--index '())
(defconst racket--index-script "racket/make-index.rkt")

(defun racket--index-format (words type-expr)
  (when type-expr
    (if (cdr type-expr)
        (format "%s: %s"
                (car words)
                (string-join
                 (cadr type-expr)
                 ", "))
      (car words))))

(defun racket--build-index ()
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
             (kill-buffer buf)))
         :connection-type 'pipe
         :filter
         (lambda (proc str)
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
                                    ((e (racket--index-format
                                         words
                                         type-expr)))
                                  (when e
                                    (setq racket--index
                                          (cons
                                           (cons e link)
                                           racket--index))))))
                             (forward-line)))
                       (goto-char (cdr line-pos)))
                   (insert str))))))
         :command (list racket-program
                        racket--index-script)))))

(defun racket-search (link)
  (interactive
   (list
    (completing-read "Describe:" racket--index))))



(provide 'racket-search)

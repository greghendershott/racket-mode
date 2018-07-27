;;; racket-stepper.el -*- lexical-binding: t; -*-

;; Copyright (c) 2018 by Greg Hendershott.
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

(require 'easymenu)
(require 'rx)
(require 'racket-common)
(require 'racket-custom)
(require 'racket-repl)

;; Need to define this before racket-stepper-mode
(defvar racket-stepper-mode-map
  (racket--easy-keymap-define
   '(("s" racket-stepper-step)
     (("n" "j") racket-stepper-next-item)
     (("p" "k") racket-stepper-previous-item))))

(easy-menu-define racket-stepper-mode-menu racket-stepper-mode-map
  "Menu for Racket stepper mode."
  '("Racket"
    ["Step" racket-stepper-step]
    ["Next" racket-stepper-next-item]
    ["Previous" racket-stepper-previous-item]))

(defconst racket-stepper-font-lock-keywords
  (eval-when-compile
    `((,(rx bol alphanumeric (zero-or-more any) eol) . font-lock-function-name-face)
      (,(rx bol "@@" (zero-or-more any) "@@" eol) . font-lock-comment-face)
      (,(rx bol "-" (zero-or-more any) eol) . 'diff-removed)
      (,(rx bol "+" (zero-or-more any) eol) . 'diff-added))))

(define-derived-mode racket-stepper-mode special-mode "Racket-Stepper"
  "Major mode for Racket stepper output.
\\<racket-stepper-mode-map>

```
\\{racket-stepper-mode-map}
```
"
  (setq header-line-format
        "Press s to step. C-h m to see help.")
  (setq-local font-lock-defaults
              (list racket-stepper-font-lock-keywords
                    t)))        ;keywords only -- not strings/comments

(defvar racket-stepper--buffer-name "*Racket Stepper*")

;;; commands

(defun racket-stepper (&optional into-base)
  "Create the `racket-stepper-mode' buffer for the sexpr before point.

With a prefix, expands identifiers from racket/base, too -- but
beware that expanding all the way to primitives can result in
dozens or hundreds of expansion steps. "
  (interactive "P")
  ;; Get and expand text
  (save-excursion
    (let* ((beg    (progn (backward-sexp) (point)))
           (end    (progn (forward-sexp) (point)))
           (text   (buffer-substring-no-properties beg end))
           (basep  (and into-base t))
           (result (with-temp-message "Running macro stepper, please wait..."
                     (let ((racket-command-timeout 30))
                       (racket--repl-command `(macro-stepper ,text ,basep))))))
      ;; Create buffer if necessary
      (unless (get-buffer racket-stepper--buffer-name)
        (with-current-buffer (get-buffer-create racket-stepper--buffer-name)
          (racket-stepper-mode)))
      ;; Give it a window if necessary
      (unless (get-buffer-window racket-stepper--buffer-name)
        (pop-to-buffer (get-buffer racket-stepper--buffer-name)))
      ;; Select the stepper window and insert
      (select-window (get-buffer-window racket-stepper--buffer-name))
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert "Original" "\n"
                text "\n\n")
        (racket-stepper--insert result)
        (racket-stepper-next-item)))))

(defun racket-stepper--insert (v)
  (with-current-buffer racket-stepper--buffer-name
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (pcase v
        (`(final . ,text)
         (insert "Final\n" text))
        (`(,label . ,diff)
         (insert label "\n" diff "\n")))
      (racket-stepper-previous-item)
      (recenter))))

(defun racket-stepper-step ()
  (interactive)
  (racket-stepper--insert
   (racket--repl-command `(macro-stepper/next))))

(defconst racket-stepper--item-rx
  (rx bol alphanumeric (zero-or-more any) eol))

(defun racket-stepper-next-item (&optional count)
  "Move point N items forward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 item forward."
  (interactive "P")
  (forward-char 1)
  (if (re-search-forward racket-stepper--item-rx nil t count)
      (beginning-of-line)
    (backward-char 1)))

(defun racket-stepper-previous-item (&optional count)
  "Move point N items backward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 item backward."
  (interactive "P")
  (re-search-backward racket-stepper--item-rx nil t count))

(provide 'racket-stepper)

;;; racket-stepper.el ends here

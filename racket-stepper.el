;;; racket-stepper.el -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2020 by Greg Hendershott.
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
(require 'racket-cmd)
(require 'racket-custom)
(require 'racket-repl)
(require 'racket-util)

;; Need to define this before racket-stepper-mode
(defvar racket-stepper-mode-map
  (racket--easy-keymap-define
   '((("C-m")   racket-stepper-step)
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
    `((,(rx bol "! " (zero-or-more any) eol) . font-lock-warning-face)
      (,(rx bol alphanumeric (zero-or-more any) eol) . font-lock-function-name-face)
      (,(rx bol "@@" (zero-or-more any) "@@" eol) . font-lock-comment-face)
      (,(rx bol "-" (zero-or-more any) eol) . 'diff-removed)
      (,(rx bol "+" (zero-or-more any) eol) . 'diff-added))))

(define-derived-mode racket-stepper-mode special-mode "Racket-Stepper"
  "Major mode for Racket stepper output.
\\<racket-stepper-mode-map>

Used by the commands `racket-expand-file',
`racket-expand-definition', `racket-expand-region', and
`racket-expand-last-sexp'.

\\{racket-stepper-mode-map}
"
  (setq header-line-format
        "Press RET to step. C-u RET to step all. C-h m to see help.")
  (setq-local font-lock-defaults
              (list racket-stepper-font-lock-keywords
                    t)))        ;keywords only -- not strings/comments

(defvar racket-stepper--buffer-name "*Racket Stepper*")

;;; commands

(defun racket-expand-file (&optional into-base)
  "Expand the `racket-mode' buffer's file in `racket-stepper-mode'.

Uses the `macro-debugger` package to do the expansion.

You do _not_ need to `racket-run' the file first; the namespace
active in the REPL is not used.

If the file is non-trivial and/or is not compiled to a .zo
bytecode file, then it might take many seconds before the
original form is displayed and you can start stepping.

With \\[universal-argument] also expands syntax from racket/base
-- which can result in very many expansion steps."
  (interactive "P")
  (unless (eq major-mode 'racket-mode)
    (user-error "Only works in racket-mode buffer"))
  (racket--save-if-changed)
  (racket-stepper--start 'file (racket--buffer-file-name) into-base))

(defun racket-expand-region (start end &optional into-base)
  "Expand the active region using `racket-stepper-mode'.

Uses Racket's `expand-once` in the namespace from the most recent
`racket-run'."
  (interactive "rP")
  (unless (region-active-p)
    (user-error "No region"))
  (racket-stepper--expand-text into-base
                               (lambda ()
                                 (cons start end))))

(defun racket-expand-definition (&optional into-base)
  "Expand the definition around point using `racket-stepper-mode'.

Uses Racket's `expand-once` in the namespace from the most recent
`racket-run'."
  (interactive "P")
  (racket-stepper--expand-text into-base
                               (lambda ()
                                 (save-excursion
                                   (cons (progn (beginning-of-defun) (point))
                                         (progn (end-of-defun)       (point)))))))

(defun racket-expand-last-sexp (&optional into-base)
  "Expand the sexp before point using `racket-stepper-mode'.

Uses Racket's `expand-once` in the namespace from the most recent
`racket-run'."
  (interactive "P")
  (racket-stepper--expand-text into-base
                               (lambda ()
                                 (save-excursion
                                   (cons (progn (backward-sexp) (point))
                                         (progn (forward-sexp)  (point)))))))

(defun racket-stepper--expand-text (prefix get-region)
  (pcase (funcall get-region)
    (`(,beg . ,end)
     (racket-stepper--start 'expr
                            (buffer-substring-no-properties beg end)
                            prefix))))

(defvar racket--stepper-repl-session-id nil
  "The REPL session used when stepping.
May be nil for 'file stepping, but must be valid for 'expr stepping.")

(defun racket-stepper--start (which str into-base)
  "Ensure buffer and issue initial command.
WHICH should be 'expr or 'file.
STR should be the expression or pathname.
INTO-BASE is treated as a raw command prefix arg and converted to boolp."
  (unless (eq major-mode 'racket-mode)
    (error "Only works from racket-mode buffers"))
  (setq racket--stepper-repl-session-id (racket--repl-session-id))
  (unless (or racket--stepper-repl-session-id
              (eq which 'file))
    (error "Only works when the racket-mode buffer has a REPL buffer, and, you should racket-run first"))
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
    (insert "Starting macro expansion stepper... please wait...\n"))
  (racket--cmd/async racket--stepper-repl-session-id
                     `(macro-stepper (,which . ,str)
                                     ,(and into-base t))
                     #'racket-stepper--insert))

(defun racket-stepper--insert (steps)
  (with-current-buffer racket-stepper--buffer-name
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (dolist (step steps)
        (pcase step
          (`(original . ,text)
           (delete-region (point-min) (point-max))
           (insert "Original\n" text "\n" "\n"))
          (`(final    . ,text) (insert "Final\n" text "\n"))
          (`(,label   . ,diff) (insert label "\n" diff "\n"))))
      (racket-stepper-previous-item)
      (when (equal (selected-window) (get-buffer-window (current-buffer)))
        (recenter)))))

(defun racket-stepper-step (prefix)
  (interactive "P")
  (racket--cmd/async racket--stepper-repl-session-id
                     `(macro-stepper/next ,(if prefix 'all 'next))
                     #'racket-stepper--insert))

(defconst racket-stepper--item-rx
  (rx bol alphanumeric (zero-or-more any) eol))

(defun racket-stepper-next-item (&optional count)
  "Move point N items forward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric \\[universal-argument] command
prefix argument. If N is omitted or nil, move point 1 item
forward."
  (interactive "P")
  (forward-char 1)
  (if (re-search-forward racket-stepper--item-rx nil t count)
      (beginning-of-line)
    (backward-char 1)))

(defun racket-stepper-previous-item (&optional count)
  "Move point N items backward.

An \"item\" is a line starting with a log level in brackets.

Interactively, N is the numeric \\[universal-argument] command
prefix argument. If N is omitted or nil, move point 1 item
backward."
  (interactive "P")
  (re-search-backward racket-stepper--item-rx nil t count))

(provide 'racket-stepper)

;;; racket-stepper.el ends here

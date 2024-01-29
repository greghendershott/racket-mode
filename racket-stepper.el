;;; racket-stepper.el -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'easymenu)
(require 'rx)
(require 'racket-cmd)
(require 'racket-custom)
(require 'racket-repl)
(require 'racket-util)
(require 'racket-back-end)

;; Need to define this before racket-stepper-mode
(defvar racket-stepper-mode-map
  (racket--easy-keymap-define
   `((("C-m")   ,#'racket-stepper-step)
     (("n" "j") ,#'racket-stepper-next-item)
     (("p" "k") ,#'racket-stepper-previous-item)
     ("g"       ,#'racket-stepper-refresh))))

(easy-menu-define racket-stepper-mode-menu racket-stepper-mode-map
  "Menu for Racket stepper mode."
  `("Racket"
    ["Step"     ,#'racket-stepper-step]
    ["Next"     ,#'racket-stepper-next-item]
    ["Previous" ,#'racket-stepper-previous-item]
    ["Refresh"  ,#'racket-stepper-refresh]))

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

(defun racket--stepper-buffer-name ()
  (format "*Racket Stepper <%s>*" (racket-back-end-name)))

;;; commands

(defun racket-expand-file (&optional no-hiding)
  "Expand the `racket-mode' buffer's file in `racket-stepper-mode'.

Uses the `macro-debugger` package to do the expansion.

You do _not_ need to `racket-run' the file first; the namespace
active in the REPL is not used.

If the file is non-trivial and/or is not compiled to a .zo
bytecode file, then it might take many seconds before the
original form is displayed and you can start stepping.

With \\[universal-argument] behaves as if `racket-expand-hiding'
were \\='disabled."
  (interactive "P")
  (racket--assert-edit-mode)
  (racket--save-if-changed)
  (racket-stepper--start nil no-hiding))

(defun racket-expand-region (&optional no-hiding)
  "Expand the active region using `racket-stepper-mode'.

Uses the `macro-debugger` package to do the expansion.

With \\[universal-argument] behaves as if `racket-expand-hiding'
were \\='disabled."
  (interactive "P")
  (unless (region-active-p)
    (user-error "No region"))
  (racket--assert-edit-mode)
  (racket-stepper--expand-text no-hiding
                               (lambda ()
                                 (cons (region-beginning)
                                       (region-end)))))

(defun racket-expand-definition (&optional no-hiding)
  "Expand the definition around point using `racket-stepper-mode'.

Uses the `macro-debugger` package to do the expansion.

With \\[universal-argument] behaves as if `racket-expand-hiding'
were \\='disabled."
  (interactive "P")
  (racket--assert-sexp-edit-mode)
  (racket-stepper--expand-text no-hiding
                               (lambda ()
                                 (save-excursion
                                   (cons (progn (beginning-of-defun) (point))
                                         (progn (end-of-defun)       (point)))))))

(defun racket-expand-last-sexp (&optional no-hiding)
  "Expand the sexp before point using `racket-stepper-mode'.

Uses the `macro-debugger` package to do the expansion.

With \\[universal-argument] behaves as if `racket-expand-hiding'
were \\='disabled."
  (interactive "P")
  (racket--assert-sexp-edit-mode)
  (racket-stepper--expand-text no-hiding
                               (lambda ()
                                 (save-excursion
                                   (cons (progn (backward-sexp) (point))
                                         (progn (forward-sexp)  (point)))))))

(defun racket-stepper--expand-text (no-hiding get-region)
  (pcase (funcall get-region)
    (`(,beg . ,end)
     (racket-stepper--start (buffer-substring-no-properties beg end)
                            no-hiding))))

;; When starting, save the essential parameters in these vars, to
;; support a refresh command.
(defvar racket--stepper-repl-session-id nil)
(defvar racket--stepper-path nil)
(defvar racket--stepper-expr nil)
(defvar racket--stepper-no-hiding nil)

(defun racket-stepper--start (expression-str no-hiding)
  "Ensure buffer and issue initial command.

STR should be the expression or nil for file expansion."
  (racket--assert-edit-mode)
  (setq racket--stepper-repl-session-id (racket--repl-session-id))
  (unless (or (not expression-str)
              racket--stepper-repl-session-id)
    (error "Expression expansion only works when the edit buffer has a REPL buffer, and, you already did a racket-run"))
  (setq racket--stepper-path (racket-file-name-front-to-back (racket--buffer-file-name)))
  (setq racket--stepper-expr expression-str)
  (setq racket--stepper-no-hiding no-hiding)
  ;; Create buffer if necessary
  (let ((name (racket--stepper-buffer-name)))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (racket-stepper-mode)))
    ;; Give it a window if necessary
    (unless (get-buffer-window name)
      (pop-to-buffer (get-buffer name)))
    ;; Select the stepper window and start.
    (select-window (get-buffer-window name))
    (racket-stepper-refresh)))

(defun racket-stepper-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (insert "Starting macro expansion stepper... please wait...\n"))
  (racket--cmd/async racket--stepper-repl-session-id
                     `(macro-stepper ,racket--stepper-path
                                     ,racket--stepper-expr
                                     ,(if racket--stepper-no-hiding
                                          'disable
                                        racket-expand-hiding))
                     #'racket-stepper--insert))

(defun racket-stepper--insert (steps)
  (if (null steps)
      (message "Nothing to expand")
    (with-current-buffer (racket--stepper-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (dolist (step steps)
          (pcase step
            (`(original . ,text)
             (delete-region (point-min) (point-max))
             (if racket--stepper-no-hiding
                 (insert "macro hiding disabled by command prefix")
               (insert-text-button "racket-expand-hiding"
                                   'action #'racket-stepper-customize-hiding)
               (insert ": ")
               (princ (if racket--stepper-no-hiding 'disable racket-expand-hiding)
                      (current-buffer)))
             (insert "\n\n")
             (insert "Original\n" text "\n" "\n"))
            (`(final  . ,text) (insert "Final\n" text "\n"))
            (`(,label . ,diff) (insert label "\n" diff "\n"))))
        (racket-stepper-previous-item)
        (when (equal (selected-window) (get-buffer-window (current-buffer)))
          (recenter))))))

(defun racket-stepper-customize-hiding (_btn)
  (customize-variable 'racket-expand-hiding))

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

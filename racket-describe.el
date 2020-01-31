;;; racket-describe.el -*- lexical-binding: t -*-

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
(require 'racket-util)
(require 'shr)
(declare-function racket--do-visit-def-or-mod "racket-edit.el")

;; TODO: Rename to racket-repl-describe and move to racket-repl.el
(defun racket-describe (&optional prefix)
"Describe the identifier at point in a `*Racket Describe*` buffer.

The intent is to give a quick reminder or introduction to
something, regardless of whether it has installed documentation
-- and to do so within Emacs, without switching to a web browser.

This buffer is also displayed when you use `company-mode' and
press F1 or C-h in its pop up completion list.

- If the identifier has installed Racket documentation, then a
  simplified version of the HTML is presented in the buffer,
  including the \"blue box\", documentation prose, and examples.

- Otherwise, if the identifier is a function, then its signature
  is displayed, for example `(name arg-1-name arg-2-name)`. If it
  has a contract or a Typed Racket type, that is also displayed.

You can quit the buffer by pressing q. Also, at the bottom of the
buffer are Emacs buttons -- which you may navigate among using
TAB, and activate using RET -- for `racket-visit-definition' and
`racket-doc'."
  (interactive "P")
  (pcase (racket--symbol-at-point-or-prompt prefix "Describe: ")
    (`nil nil)
    (str (racket--do-describe 'namespace str t))))

(defun racket--do-describe (how str &optional display-and-pop-to-p)
  "A helper used by both `racket-describe' and `company-mode'.

HOW is supplied as the first argument to the back-end
\"describe\" command.

STR is the string form of an identifier that is to be described.

DISPLAY-AND-POP-TO-P should be t for use by `racket-describe' --
in which case some buttons are added and the buffer is displayed
-- and nil for use by `company-mode'.

Returns the buffer in which the description was written."
  ;; Work around what seems to be a bug with `shr-insert-document' --
  ;; elements are out of order when an existing Racket Describe buffer
  ;; hasn't had a `quit-window' -- by re-creating the buffer.
  (with-current-buffer (racket--get-buffer-recreate "*Racket Describe*")
    (let* ((html (racket--cmd/await `(describe ,how ,str)))
           ;; Because shr removes leading &nbsp; from <td> elements --
           ;; which messes up the indentation of s-expressions
           ;; including contracts -- replace &nbsp with `spc' in the
           ;; source HTML. Below we'll replace `spc' with " " in the
           ;; result of `shr-insert-document'.
           (spc (string #x2020))       ;unlikely character (hopefully)
           (dom (with-temp-buffer
                  (insert html)
                  (goto-char (point-min))
                  (while (re-search-forward "&nbsp;" nil t)
                    (replace-match spc t t))
                  (libxml-parse-html-region (point-min) (point-max)))))
      (racket-describe-mode)
      (read-only-mode -1)
      (let ((shr-use-fonts nil))
        (shr-insert-document dom))
      (goto-char (point-min))
      (while (re-search-forward spc nil t)
        (replace-match " " t t))
      (when display-and-pop-to-p
        (goto-char (point-max))
        (insert "\n")
        (insert-text-button "Definition"
                            'follow-link t
                            'action
                            (lambda (_btn)
                              (racket--do-visit-def-or-mod `(def ,how ,str))))
        (insert "   ")
        (insert-text-button "Documentation in Browser"
                            'follow-link t
                            'action
                            (lambda (_btn)
                              (racket--cmd/await `(doc ,str))))
        (insert "          [q]uit"))
      (read-only-mode 1)
      (goto-char (point-min))
      (when display-and-pop-to-p
        (display-buffer (current-buffer) t)
        (pop-to-buffer (current-buffer))
        (message "Type TAB to move to links, 'q' to restore previous window"))
      (current-buffer))))

(defvar racket-describe-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    (mapc (lambda (x)
            (define-key m (kbd (car x)) (cadr x)))
          '(("<tab>"   racket-describe--next-button)
            ("S-<tab>" racket-describe--prev-button)))
    m)
  "Keymap for Racket Describe mode.")

(define-derived-mode racket-describe-mode special-mode
  "RacketDescribe"
  "Major mode for describing Racket functions.
\\{racket-describe-mode-map}"
  (setq show-trailing-whitespace nil))

(defun racket-describe--next-button ()
  (interactive)
  (forward-button 1 t t))

(defun racket-describe--prev-button ()
  (interactive)
  (forward-button -1 t t))

(provide 'racket-describe)

;; racket-describe.el ends here

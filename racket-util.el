;;; racket-util.el -*- lexical-binding: t -*-

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

(require 'racket-custom)

(defun racket--easy-keymap-define (spec)
  "Make a sparse keymap with the bindings in SPEC.

SPEC is
  (list (list KEY-OR-KEYS DEF) ...)

KEY-OR-KEYS is either a string given to `kbd', or, for the case
where multiple keys bind to the same command, a list of such
strings.

DEF is the same as DEF for `define-key'."
  (let ((m (make-sparse-keymap)))
    (mapc (lambda (x)
            (let ((keys (if (listp (car x))
                            (car x)
                          (list (car x))))
                  (def  (cadr x)))
              (mapc (lambda (key)
                      (define-key m (kbd key) def))
                    keys)))
          spec)
    m))

(defun racket--buffer-file-name (&optional no-adjust)
  "Like `buffer-file-name' but always a non-propertized string.

Unless NO-ADJUST is not nil, applies the name to the function
variable `racket-path-from-emacs-to-racket-function'."
  (let ((v (and (buffer-file-name)
                (substring-no-properties (buffer-file-name)))))
    (if no-adjust
        v
      (funcall racket-path-from-emacs-to-racket-function
               v))))

(defun racket--get-buffer-recreate (bufname)
  "Like `get-buffer-create' but re-creates the buffer if it already exists."
  (let* ((buf (get-buffer bufname))
         (_   (when buf (kill-buffer buf)))))
  (get-buffer-create bufname))

(defun racket--save-if-changed ()
  (unless (eq major-mode 'racket-mode)
    (user-error "Current buffer is not a racket-mode buffer"))
  (when (or (buffer-modified-p)
            (and (buffer-file-name)
                 (not (file-exists-p (buffer-file-name)))))
    (save-buffer)))

(add-hook 'racket--repl-before-run-hook #'racket--save-if-changed)

(defun racket--mode-edits-racket-p ()
  "Return non-nil if the current major mode is one that edits Racket code.

This is intended to be used with commands that customize their
behavior based on whether they are editing Racket, such as
Paredit bindings, without each of those commands needing to have
a list of all modes in which Racket is edited."
  (memq major-mode '(racket-mode racket-repl-mode)))

(defun racket--take-while (xs pred)
  (pcase xs
    (`()         `())
    (`(,x . ,xs) (if (funcall pred x)
                     (cons x (racket--take-while xs pred))
                   `()))))

(defconst racket--el-source-dir
  (file-name-directory (or load-file-name (racket--buffer-file-name)))
  "Path to dir of our Emacs Lisp source files.
When installed as a package, this can be found from the variable
`load-file-name'. When developing interactively, get it from the
.el buffer file name.")

(defconst racket--rkt-source-dir
  (expand-file-name "./racket/" racket--el-source-dir)
  "Path to dir of our Racket source files. ")

;;; trace

(defvar racket--trace-enable nil)

(defun racket--trace (p &optional s retval)
  (when racket--trace-enable
    (let ((b (get-buffer-create "*Racket Trace*"))
          (deactivate-mark deactivate-mark))
      (save-excursion
        (save-restriction
          (with-current-buffer b
            (insert p ": " (if (stringp s) s (format "%S" s)) "\n"))))))
  retval)

(defun racket--toggle-trace (arg)
  (interactive "P")
  (setq racket--trace-enable (or arg (not racket--trace-enable)))
  (if racket--trace-enable
      (message "Racket trace on")
    (message "Racket trace off"))
  (let ((b (get-buffer-create "*Racket Trace*")))
    (pop-to-buffer b t t)
    (setq truncate-lines t)))

(defun racket--restoring-current-buffer (proc)
  "Return a procedure restoring `current-buffer' during the dynamic extent of PROC."
  (let ((buf (current-buffer)))
    (lambda (&rest args)
      (with-current-buffer buf
        (apply proc args)))))

;;; string trim

;; "inline" the one thing we used from `s' so we can drop the dep.
;; TO-DO: Rewrite racket--trim more simply; I just don't want to
;; detour now.

(defun racket--trim-left (s)
  "Remove whitespace at the beginning of S."
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun racket--trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun racket--trim (s)
  "Remove whitespace at the beginning and end of S."
  (racket--trim-left (racket--trim-right s)))

(defun racket--non-empty-string-p (v)
  (and (stringp v)
       (not (string-match-p "\\`[ \t\n\r]*\\'" v)))) ;`string-blank-p'

;;; at-point

(defun racket--symbol-at-point-or-prompt (force-prompt-p prompt &optional completions)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or FORCE-PROMPT-P is true. The
prompt uses `read-from-minibuffer' when COMPLETIONS is nil, else
`ido-completing-read'. Returns `stringp' not `symbolp' to
simplify using the result in a sexpr that can be passed to Racket
backend. Likewise text properties are stripped."
  (let ((sap (thing-at-point 'symbol t)))
    (if (or force-prompt-p (not sap))
        (let ((s (if completions
                     (ido-completing-read prompt completions nil nil sap)
                   (read-from-minibuffer prompt sap))))
          (if (or (not s)
                  (equal ""
                         (racket--trim
                          (substring-no-properties s))))
              nil
            s))
      sap)))


(defconst racket--config-dir (file-name-as-directory
                              (locate-user-emacs-file "racket-mode")))

(provide 'racket-util)

;; racket-util.el ends here

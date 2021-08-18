;;; racket-util.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
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

(require 'subr-x)
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

(defun racket--buffer-file-name (&optional no-replace-slash)
  "Like `buffer-file-name' but adjusted for use outside Emacs.

Always a non-propertized string.

When on Windows and unless NO-REPLACE-SLASH is not nil, replaces
back slashes with forward slashes. Emacs uses forward slashes for
buffer file names even on Windows, so we need to \"reverse\"
this to use the names with shell programs or a Racket back end."
  (let ((v (and (buffer-file-name)
                (substring-no-properties (buffer-file-name)))))
    (if (and racket--winp
             (not no-replace-slash))
        (subst-char-in-string ?\\ ?/ v)
      v)))

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

(defvar racket--rkt-source-dir
  (expand-file-name "./racket/" racket--el-source-dir)
  "Path to dir of our Racket source files. ")

(defun racket--restoring-current-buffer (proc)
  "Return a procedure restoring `current-buffer' during the dynamic extent of PROC."
  (let ((buf (current-buffer)))
    (lambda (&rest args)
      (with-current-buffer buf
        (apply proc args)))))

(defun racket--non-empty-string-p (v)
  (and (stringp v) (not (string-blank-p v))))

(defun racket--symbol-at-point-or-prompt (force-prompt-p
                                          prompt
                                          &optional
                                          completions
                                          allow-blank-p)
  "If symbol at point return it, else prompt user.

When FORCE-PROMPT-P always prompt. The prompt uses
`read-from-minibuffer' when COMPLETIONS is nil, else
`completing-read'.

Returns `stringp' not `symbolp' to simplify using the result in a
sexpr that can be passed to Racket backend. Likewise the string
is trimmed and text properties are stripped.

Unless ALLOW-BLANK-P, a blank string after trimming returns nil
as if the user had C-g to quit."
  (let ((sap (thing-at-point 'symbol t)))
    (if (or force-prompt-p
            (not sap))
        (let* ((s (if completions
                      (completing-read prompt completions nil nil sap)
                    (read-from-minibuffer prompt sap)))
               (s (if s
                      (string-trim (substring-no-properties s))
                    s)))
          (if (or (not s)
                  (and (not allow-blank-p) (string-blank-p s)))
              nil
            s))
      sap)))

(defun racket-project-root (file)
  "Given an absolute pathname for FILE, return its project root directory.

The \"project\" is determined by trying, in order:

- `projectile-project-root'
- `vc-root-dir'
- `project-current'
- `file-name-directory'"
  (let ((dir (if file
                 (file-name-directory file)
               default-directory)))
    (or (and (fboundp 'projectile-project-root)
             (projectile-project-root dir))
        (and (fboundp 'vc-root-dir)
             (vc-root-dir))
        (and (fboundp 'project-current)
             (cdr (project-current nil dir)))
        dir)))

(provide 'racket-util)

;; racket-util.el ends here

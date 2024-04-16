;;; racket-util.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'subr-x)
(require 'racket-custom)

(defun racket--easy-keymap-define (spec)
  "Make a sparse keymap with the bindings in SPEC.

SPEC is
  (list (list KEY-OR-KEYS DEF) ...)

KEY-OR-KEYs is either a single key, or, as a convenience when
multiple keys bind to the same command, a list of keys.

Each key is either a string, which transformed by `kbd' before
being given to `define-key', or another value given directly to
`define-key'. An example of the latter is [remap command-name].

DEF is the same as DEF for `define-key'."
  (let ((m (make-sparse-keymap)))
    (mapc (lambda (x)
            (let ((keys (if (listp (car x))
                            (car x)
                          (list (car x))))
                  (def  (cadr x)))
              (mapc (lambda (key)
                      (define-key m
                        (if (stringp key)
                            (kbd key)
                          key)
                        def))
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
  (racket--assert-edit-mode)
  (when (or (buffer-modified-p)
            (and (buffer-file-name)
                 (not (file-exists-p (buffer-file-name)))))
    (save-buffer)))

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

(defun racket--edit-mode-p ()
  (and (seq-some #'derived-mode-p '(racket-mode racket-hash-lang-mode)) t))

(defun racket--assert-edit-mode (&optional fail-thunk)
  (unless (racket--edit-mode-p)
    (when fail-thunk (funcall fail-thunk))
    (user-error "%S works only in racket-mode or racket-hash-lang-mode edit buffers"
                this-command)))

(defun racket--assert-edit-or-repl-mode (&optional fail-thunk)
  (unless (or (racket--edit-mode-p)
              (derived-mode-p 'racket-repl-mode))
    (when fail-thunk (funcall fail-thunk))
    (user-error "%S works only in racket-mode or racket-hash-lang-mode edit buffers, or racket-repl-mode buffers"
                this-command)))

;; Avoid circular require
(declare-function racket-hash-lang-forward-sexp "racket-hash-lang" (&optional arg))

(defun racket--sexp-edit-mode-p ()
  "Either `racket-mode' or `racket-hash-lang-mode', provided the
latter has /not/ set the variable `forward-sexp-function' because
the hash-lang uses racket-grouping-position. In other words, when
`forward-sexp-function' is nil we may assume that the lang uses
s-expressions."
  (and (racket--edit-mode-p)
       (not (equal forward-sexp-function #'racket-hash-lang-forward-sexp))))

(defun racket--assert-sexp-edit-mode ()
  (unless (racket--sexp-edit-mode-p)
    (user-error "%S only works in racket-mode, or, racket-hash-lang-mode when the lang uses sexps"
                this-command)))

(provide 'racket-util)

;; racket-util.el ends here

;;; racket-util.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2025 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'project)
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

(defun racket-project-root (file)
  "Given an absolute pathname for FILE, return its project root directory.

The \"project\" is determined by trying, in order:

- `projectile-project-root', if that exists
- `project-current'
- `file-name-directory'"
  (let ((dir (if (and (stringp file)
                      (file-exists-p file))
                 (file-name-directory file)
               default-directory)))
    (or (and (fboundp 'projectile-project-root)
             (projectile-project-root dir))
        (when-let (pr (project-current nil dir))
          ;; In newer Emacs `project-root' is defined; `project-roots'
          ;; is deprecated (and someday may disappear). We check both
          ;; with `fboundp' here, in order to work correctly and to
          ;; please byte compiler on older, current, and future
          ;; versions of Emacs.
          (cond ((fboundp 'project-root)  (project-root pr))
                ((fboundp 'project-roots) (car (project-roots pr)))))
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

;;; Mouse event posn (for context menus) as well as `point'.

(defun racket--menu-position ()
  (ignore-errors
    (posn-point (event-start (aref (this-command-keys-vector) 0)))))

(defun racket--point ()
  (or (racket--menu-position)
      (point)))

(defun racket--thing-at-point (thing &optional no-properties)
  (if-let (pos (racket--menu-position))
      (save-excursion
        (goto-char pos)
        (thing-at-point thing no-properties))
    (thing-at-point thing no-properties)))

(defun racket--bounds-of-thing-at-point (thing)
  (if-let (pos (racket--menu-position))
      (save-excursion
        (goto-char pos)
        (bounds-of-thing-at-point thing))
    (bounds-of-thing-at-point thing)))

(defun racket--symbol-at-point-or-prompt (force-prompt-p
                                          prompt
                                          &optional
                                          completions
                                          allow-blank-p)
  "Return `racket-thing-at-point` symbol or prompt user.

When FORCE-PROMPT-P always prompt. The prompt uses
`read-from-minibuffer' when COMPLETIONS is nil, else
`completing-read'.

Returns `stringp' not `symbolp' to simplify using the result in a
sexpr that can be passed to Racket backend. Likewise the string
is trimmed and text properties are stripped.

Unless ALLOW-BLANK-P, a blank string after trimming returns nil
as if the user had C-g to quit."
  (let ((sap (racket--thing-at-point 'symbol t)))
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

(defconst racket--f5-bindings
  '(("<f5>"     racket-run-and-switch-to-repl)
    ("M-C-<f5>" racket-racket)
    ("C-<f5>"   racket-test))
  "On the one hand, we want to allow `racket-mode-map' and
`racket-hash-lang-mode-map' to bind <f5> as a convenience for
users coming from DrRacket.

On the other hand, Emacs convention reserves <f5> for user
bindings. See issue #714.

On the third hand, we want to initialize the major mode's keymaps
with these, for use by doc/generate.el, to document the default
bindings.

Solution: Append these in the keymap initialization, and also
call `racket--polite-user-f-keys' in the major mode
initialization function. That adds/remove the binding based on
whether it would shadow an end user binding in the global map.")

(defun racket--polite-user-f-keys (major-mode-keymap keys+cmds)
  "Politely bind/unbind KEYS+CMDS in MAJOR-MODE-KEYMAP."
  (dolist (k+c keys+cmds)
    (let ((key (kbd (car k+c)))
          (cmd (cadr k+c)))
      ;; Avoid shadowing a binding user has made in the global map.
      (if (lookup-key (current-global-map) key)
          (define-key major-mode-keymap key nil)
        ;; Unless user has modified binding in major-mode-keymap,
        ;; restore our binding there.
        (unless (lookup-key major-mode-keymap key)
          (define-key major-mode-keymap key cmd))))))

(defun racket--file-name-slug (str)
  "Change STR to a string that is a valid file name."
  ;; 2. But not leading or trailing ?-
  (replace-regexp-in-string
   (rx (or (seq bos (+ ?-))
           (seq (+ ?-) eos)))
   ""
   ;; 1. Replace runs of anything that is not alnum with a single ?-.
   (replace-regexp-in-string
    (rx (+ (not (any alnum))))
    "-"
    str)))

(provide 'racket-util)

;; racket-util.el ends here

;;; racket-parens.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Things related to parens, paredit, electric-pair-mode

(require 'racket-custom)
(require 'racket-ppss)
(require 'racket-util)

;;; racket--self-insert

(defun racket--self-insert (event)
  "Simulate a `self-insert-command' of EVENT.

Using this intead of `insert' allows self-insert hooks to run,
which is important for things like `electric-pair-mode'.

A command using this should probably set its delete-selection
property to t so that `delete-selection-mode' works:

  (put \\='racket-command \\='delete-selection t)

If necessary the value of the property can be a function, for
example `racket--electric-pair-mode-not-active'."
  (let ((last-command-event event))     ;set this for hooks
    (self-insert-command (prefix-numeric-value nil))))

(defun racket--electric-pair-mode-not-active ()
  "A suitable value for the delete-selection property of commands
that insert parens: Inserted text should replace the selection
unless a mode like `electric-pair-mode' is enabled, in which case
the selection is to be wrapped in parens."
  (not (and (boundp 'electric-pair-mode)
            electric-pair-mode)))


;;; Automatically insert matching \?) \?] or \?}

(defconst racket--matching-parens
  '(( ?\( . ?\) )
    ( ?\[ . ?\] )
    ( ?\{ . ?\} )))

(defun racket-insert-closing (&optional prefix)
  "Insert a matching closing delimiter.

With \\[universal-argument] insert the typed character as-is.

This is handy if you're not yet using something like
`paredit-mode', `smartparens-mode', `parinfer-mode', or simply
`electric-pair-mode' added in Emacs 24.5."
  (interactive "P")
  (let* ((do-it (not (or prefix
                         (and (string= "#\\"
                                       (buffer-substring-no-properties
                                        (- (point) 2) (point) )))
                         (racket--ppss-string-p (syntax-ppss)))))
         (open-char  (and do-it        (racket--open-paren #'backward-up-list)))
         (close-pair (and open-char    (assq open-char racket--matching-parens)))
         (close-char (and close-pair   (cdr close-pair))))
    (racket--self-insert (or close-char last-command-event))))

(put 'racket-insert-closing 'delete-selection
     #'racket--electric-pair-mode-not-active)

(defun racket--open-paren (back-func)
  "Use BACK-FUNC to find an opening ( [ or { if any.
BACK-FUNC should be something like #\\='backward-sexp or #\\='backward-up-list."
  (save-excursion
    (ignore-errors
      (funcall back-func)
      (let ((ch (char-after)))
        (and (eq ?\( (char-syntax ch))
             ch)))))

;;; paredit spaces in reader literals and at-expressions

(defun racket--paredit-space-for-delimiter-predicate (endp delimiter)
  "A value for hook `paredit-space-for-delimiter-predicates'."
  (if (and (racket--mode-edits-racket-p)
           (not endp))
      (not
       (or
        ;; reader literal: e.g. #(), #hasheq(), #"bstr", #px".*"
        (looking-back (rx ?# (* (or (syntax word)
                                    (syntax symbol)
                                    (syntax punctuation))))
                      nil)
        ;; at-expression: @foo[ @foo{
        (and (memq delimiter '(?\[ ?\{))
             (looking-back (rx ?@ (* (or (syntax word)
                                         (syntax symbol)
                                         (syntax punctuation))))
                           nil))
        ;; at-expression: @foo[]{
        (and (eq delimiter ?\{)
             (looking-back (rx ?@ (* (or (syntax word)
                                         (syntax symbol)
                                         (syntax punctuation)))
                               ?\[
                               (* (or (syntax word)
                                      (syntax symbol)
                                      (syntax punctuation)))
                               ?\])
                           nil))
        ))
    t))

;;; Cycle paren shapes

(defconst racket--paren-shapes
  '( (?\( ?\[ ?\] )
     (?\[ ?\{ ?\} )
     (?\{ ?\( ?\) ))
  "This is not user-configurable because we expect them have to
  have actual ?\( and ?\) char syntax.")

(defun racket-cycle-paren-shapes ()
  "Cycle the sexpr among () [] {}."
  (interactive)
  (racket--assert-sexp-edit-mode)
  (save-excursion
    (unless (eq ?\( (char-syntax (char-after)))
      (backward-up-list))
    (pcase (assq (char-after) racket--paren-shapes)
      (`(,_ ,open ,close)
       (delete-char 1)
       (insert open)
       (backward-char 1)
       (forward-sexp 1)
       (delete-char -1)
       (insert close))
      (_
       (user-error "Don't know that paren shape")))))

(provide 'racket-parens)

;; racket-parens.el ends here

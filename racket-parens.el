;;; racket-parens.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2021 by Greg Hendershott.
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

;; Things related to parens, paredit, electric-pair-mode

(require 'racket-custom)
(require 'racket-ppss)
(require 'racket-util)

;;; racket--self-insert

(defun racket--self-insert (event)
  "Simulate a `self-insert-command' of EVENT.

Using this intead of `insert' allows self-insert hooks to run,
which is important for things like `'electric-pair-mode'.

A command using this should probably set its 'delete-selection
property to t so that `delete-selection-mode' works:

  (put 'racket-command 'delete-selection t)

If necessary the value of the property can be a function, for
example `racket--electric-pair-mode-not-active'."
  (let ((last-command-event event))     ;set this for hooks
    (self-insert-command (prefix-numeric-value nil))))

(defun racket--electric-pair-mode-not-active ()
  "A suitable value for the 'delete-selection property of
commands that insert parens: Inserted text should replace the
selection unless a mode like `electric-pair-mode' is enabled, in
which case the selection is to be wrapped in parens."
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

;;; paredit and reader literals

(defun racket--reader-literal-paredit-space-for-delimiter-predicate (endp _delimiter)
  "`paredit-mode' shouldn't insert space beteween # and open delimiters.

Examples: #() #2() #fl() #hasheq  etc.

This function is a suitable element for the list variable
`paredit-space-for-delimiter-predicates'."
  (if (and (racket--mode-edits-racket-p)
           (not endp))
      (not (looking-back (rx ?# (* (or (syntax word)
                                       (syntax symbol)
                                       (syntax punctuation))))
                         nil))
    t))

(eval-after-load 'paredit
  '(add-hook 'paredit-space-for-delimiter-predicates
             #'racket--reader-literal-paredit-space-for-delimiter-predicate))

;;; paredit and at-expressions

(defun racket--at-expression-paredit-space-for-delimiter-predicate (endp delimiter)
  "`paredit-mode' shouldn't insert space before [ or { in Racket at-expressions.

This function is a suitable element for the list variable
`paredit-space-for-delimiter-predicates'."
  (if (and (racket--mode-edits-racket-p)
           (not endp))
      (not (or
            ;; @foo[ @foo{
            (and (memq delimiter '(?\[ ?\{))
                 (looking-back (rx ?@ (* (or (syntax word)
                                             (syntax symbol)
                                             (syntax punctuation))))
                               nil))
            ;; @foo[]{
            (and (eq delimiter ?\{)
                 (looking-back (rx ?@ (* (or (syntax word)
                                             (syntax symbol)
                                             (syntax punctuation)))
                                   ?\[
                                   (* (or (syntax word)
                                          (syntax symbol)
                                          (syntax punctuation)))
                                   ?\])
                               nil))))
    t))

(eval-after-load 'paredit
  '(add-hook 'paredit-space-for-delimiter-predicates
             #'racket--at-expression-paredit-space-for-delimiter-predicate))


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
  (save-excursion
    (unless (eq ?\( (char-syntax (char-after)))
      (backward-up-list))
    (pcase (assq (char-after) racket--paren-shapes)
      (`(,_ ,open ,close)
       (delete-char 1)
       (insert open)
       (backward-char 1)
       (forward-sexp 1)
       (backward-delete-char 1)
       (insert close))
      (_
       (user-error "Don't know that paren shape")))))

(defun racket--open-paren (back-func)
  "Use BACK-FUNC to find an opening ( [ or { if any.
BACK-FUNC should be something like #'backward-sexp or #'backward-up-list."
  (save-excursion
    (ignore-errors
      (funcall back-func)
      (let ((ch (char-after)))
        (and (eq ?\( (char-syntax ch))
             ch)))))

(provide 'racket-parens)

;; racket-parens.el ends here

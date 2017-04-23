;;; racket-complete.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2016 by Greg Hendershott.
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

(require 'cl-lib)
(require 'ido)
(require 'racket-custom)
(require 'racket-repl)
(require 'shr)
(require 's)

;;; namespace symbols i.e. completion candidates

(make-variable-buffer-local
 (defvar racket--namespace-symbols nil
   "A cache of the list of all Racket namespace symbols.

This var is local to each buffer, including the REPL buffer.

See `racket--invalidate-completion-cache' and
`racket--get-namespace-symbols'."))

(defun racket--invalidate-completion-cache ()
  "Both current `racket-mode' buffer and `racket-repl-mode' buffer (if any)."
  (setq racket--namespace-symbols nil)
  (with-racket-repl-buffer
    (setq racket--namespace-symbols nil)))

(defun racket--get-namespace-symbols ()
  "Get Racket namespace symbols from the cache or from the Racket process."
  (unless racket--namespace-symbols
    (if (racket--in-repl-or-its-file-p)
        (setq racket--namespace-symbols
              (racket--repl-command "syms"))
      (error "Completions not available until you `racket-run' this buffer")))
  racket--namespace-symbols)

(defun racket--complete-prefix (prefix)
  (all-completions prefix (racket--get-namespace-symbols)))

(defun racket--complete-prefix-begin ()
  (save-excursion (skip-syntax-backward "^-()>")
                  (point)))

(defun racket--complete-prefix-end (beg)
  (unless (or (eq beg (point-max))
              (member (char-syntax (char-after beg)) '(?\" ?\( ?\))))
    (let ((pos (point)))
      (condition-case nil
          (save-excursion
            (goto-char beg)
            (forward-sexp 1)
            (when (>= (point) pos)
              (point)))
        (scan-error pos)))))

(defun racket-complete-at-point (&optional _predicate)
  (with-syntax-table racket-mode-syntax-table ;probably don't need this??
    (let* ((beg    (racket--complete-prefix-begin))
           (end    (or (racket--complete-prefix-end beg) beg))
           (prefix (and (> end beg) (buffer-substring-no-properties beg end)))
           (cmps   (and prefix (completion-table-dynamic
                                (lambda (_)
                                  (racket--complete-prefix prefix))))))
      (and cmps
           (list beg
                 end
                 cmps
                 :predicate #'identity
                 :company-docsig #'racket--get-type
                 :company-doc-buffer #'racket--do-describe
                 :company-location #'racket--get-def-file+line)))))

(defun racket--get-def-file+line (sym)
  "Return a value suitable for use as :company-location."
  (pcase (racket--repl-command "def %s" sym)
    (`(,path ,line ,_) (cons path line))
    (_ nil)))

;;; "types" (i.e. TR types, contracts, and/or function signatures)

(make-variable-buffer-local
 (defvar racket--type-cache (make-hash-table :test #'eq)
   "Memoize ,type commands in Racket REPL.

`racket-run' should call `racket-invalidate-type-cache'."))

(defun racket--invalidate-type-cache ()
  (setq racket--type-cache (make-hash-table :test #'eq))
  (with-racket-repl-buffer
    (setq racket--type-cache (make-hash-table :test #'eq))))

(defun racket--get-type (str)
  (let* ((sym (intern str))
         (v (gethash sym racket--type-cache)))
    (or v
        (and (racket--in-repl-or-its-file-p)
             (let ((v (racket--repl-command (concat "type " str))))
               (puthash sym v racket--type-cache)
               v)))))

;;; at-point

(defun racket--symbol-at-point-or-prompt (force-prompt-p prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or FORCE-PROMPT-P is true. The
prompt uses `read-from-minibuffer'."
  (racket--x-at-point-or-prompt force-prompt-p
                                prompt
                                #'read-from-minibuffer))

(defun racket--identifier-at-point-or-prompt (force-prompt-p prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or FORCE-PROMPT-P is true. The
prompt uses `racket--read-identifier'."
  (racket--x-at-point-or-prompt force-prompt-p
                                prompt
                                #'racket--read-identifier))

(defun racket--x-at-point-or-prompt (force-prompt-p prompt reader)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or FORCE-PROMPT-P is true. The
prompt uses READER, which must be a function like
`read-from-minibuffer'."
  (let ((sap (symbol-at-point)))
    (if (or force-prompt-p (not sap))
        (let ((s (funcall reader prompt (and sap (symbol-name sap)))))
          (if (equal "" (s-trim s))
              nil
            s))
      sap)))

(defun racket--read-identifier (prompt default)
  "Do `ido-completing-read with `racket--get-namespace-symbols'."
  (ido-completing-read prompt
                       (racket--get-namespace-symbols)
                       nil      ;predicate
                       nil      ;require-match
                       default  ;initial
                       nil      ;history
                       default))

;;; eldoc

(defun racket-eldoc-function ()
  "A value suitable for the variable `eldoc-documentation-function'.

By default racket-mode sets `eldoc-documentation-function' to nil
-- no `eldoc-mode' support. You may set it to this function in a
`racket-mode-hook' if you really want to use `eldoc-mode' with
Racket. But it is not a very satisfying experience because Racket
is not a very \"eldoc friendly\" language. Although racket-mode
attempts to discover argument lists, contracts, or types this
doesn't work in many common cases:

- Many Racket functions are defined in #%kernel. There's no easy
  way to determine their argument lists. Most are not provided
  with a contract.

- Many of the interesting Racket forms are syntax (macros) not
  functions. There's no easy way to determine their \"argument
  lists\".

A more satisfying experience is to use `racket-describe' or
`racket-doc'."
  (and (racket--repl-live-p)
       (> (point) (point-min))
       (save-excursion
         (condition-case nil
             ;; The char-before and looking-at checks below are to
             ;; avoid calling `racket--get-type' when the sexp is
             ;; quoted or when its first elem couldn't be a Racket
             ;; function name.
             (let* ((beg (progn
                           (backward-up-list)
                           (and (not (memq (char-before) '(?` ?' ?,)))
                                (progn (forward-char 1) (point)))))
                    (beg (and beg (looking-at "[^0-9#'`,\"]") beg))
                    (end (and beg (progn (forward-sexp) (point))))
                    (end (and end
                              (char-after (point))
                              (eq ?\s (char-syntax (char-after (point))))
                              end))
                    (sym (and beg end (buffer-substring-no-properties beg end)))
                    (str (and sym (racket--get-type sym))))
               str)
           (scan-error nil)))))

;;; describe

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
  has a Typed Racket type or a contract, that is also displayed.

You can quit the buffer by pressing q. Also, at the bottom of the
buffer are Emacs buttons -- which you may navigate among using
TAB, and activate using RET -- for `racket-visit-definition' and
`racket-doc'."
  (interactive "P")
  (let ((sym (racket--identifier-at-point-or-prompt prefix
                                                    "Describe: ")))
    (when sym
      (racket--do-describe sym t))))

(defun racket--do-describe (sym &optional pop-to)
  "A helper for `racket-describe' and company-mode.

POP-TO should be t for the former (in which case some buttons are
added) and nil for the latter.

Returns the buffer in which the description was written."
  (let* ((bufname "*Racket Describe*")
         (html (racket--repl-command "describe %s" sym))
         ;; Emacs shr renderer removes leading &nbsp; from <td> elements
         ;; -- which messes up the indentation of s-expressions including
         ;; contracts. So replace &nbsp with `spc' in the source HTML,
         ;; and replace `spc' with " " after shr-insert-document outputs.
         (spc (string #x2020)) ;unlikely character (hopefully)
         (dom (with-temp-buffer
                (insert html)
                (goto-char (point-min))
                (while (re-search-forward "&nbsp;" nil t)
                  (replace-match spc t t))
                (libxml-parse-html-region (point-min) (point-max))))
         ;; Work around what seems to be a bug with shr -- inserting
         ;; elements out of order, when an existing Racket Describe buffer
         ;; hasn't had a quit-window -- by re-creating the bufer.
         (buf (get-buffer bufname))
         (_   (and buf (kill-buffer buf)))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (racket-describe-mode)
      (read-only-mode -1)
      (erase-buffer)
      (let ((shr-use-fonts nil))
        (shr-insert-document dom))
      (goto-char (point-min))
      (while (re-search-forward spc nil t)
        (replace-match " " t t))
      (goto-char (point-max))
      (when pop-to
        (insert-text-button "Definition"
                            'action
                            `(lambda (_btn)
                               (racket--do-visit-def-or-mod
                                "def"
                                ,(substring-no-properties (format "%s" sym)))))
        (insert "   ")
        (insert-text-button "Documentation in Browser"
                            'action
                            `(lambda (_btn)
                               (racket--repl-command
                                "doc %s"
                                ,(substring-no-properties (format "%s" sym)))))
        (insert "          [q]uit"))
      (read-only-mode 1)
      (goto-char (point-min))
      (display-buffer (current-buffer) t)
      (when pop-to
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


(provide 'racket-complete)

;; racket-complete.el ends here

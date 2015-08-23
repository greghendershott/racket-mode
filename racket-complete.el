;;; racket-complete.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2015 by Greg Hendershott.
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
(declare-function racket--get-def-file+line "racket-edit.el" (sym))
(declare-function racket--do-describe                        (sym pop-to))

(make-variable-buffer-local
 (defvar racket--namespace-symbols nil
   "A cache of Racket namespace symbols.

See `racket--invalidate-completion-cache' and
`racket--get-namespace-symbols'."))

(defun racket--invalidate-completion-cache ()
  "Empties `racket--namespace-symbols'."
  (setq racket--namespace-symbols nil))

(defun racket--get-namespace-symbols ()
  "Get Racket namespace symbols from the cache or from the Racket process."
  (unless racket--namespace-symbols
    (setq racket--namespace-symbols
          (racket--repl-cmd/sexpr ",syms")))
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
                 :company-docsig #'racket-get-type
                 :company-doc-buffer #'racket--do-describe
                 :company-location #'racket--get-def-file+line)))))


;;; racket--xxx-at-point-or-prompt

(defun racket--symbol-at-point-or-prompt (force-prompt-p prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or FORCE-PROMPT-P is true. The
prompt uses `read-from-minibuffer'."
  (racket--x-at-point-or-prompt force-prompt-p
                                prompt
                                #'read-from-minibuffer))

(defun racket--identifier-at-point-or-prompt (force-prompt-p prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-ant-point or FORCE-PROMPT-P is true. The
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


;;; types (i.e. TR types, contracts, and/or function signatures)

(defvar racket--type-cache (make-hash-table :test 'eq)
  "Memoize ,type commands in Racket REPL.
 `racket-run' should call `racket-invalidate-type-cache'.")

(defun racket--invalidate-type-cache ()
  (setq racket--type-cache (make-hash-table :test 'eq)))

(defun racket-get-type (str)
  (let* ((sym (intern str))
         (v (gethash sym racket--type-cache)))
    (or v
        (let ((v (racket--repl-cmd/sexpr (concat ",type " str))))
          (puthash sym v racket--type-cache)
          v))))

;;; eldoc

(defun racket-eldoc-function ()
  (and (> (point) (point-min))
       (save-excursion
         (condition-case nil
             ;; The char-before and looking-at checks below are to
             ;; avoid calling `racket-get-type' when the sexp is
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
                    (str (and sym (racket-get-type sym))))
               str)
           (scan-error nil)))))

(provide 'racket-complete)

;; racket-complete.el ends here

;;; racket-complete.el

;; Copyright (c) 2013-2014 by Greg Hendershott.
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

(require 'racket-eval)

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
          (racket--eval/sexpr
           (format "%S"
                   `(map symbol->string (namespace-mapped-symbols))))))
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

(defun racket-complete-at-point (&optional predicate)
  (with-syntax-table racket-mode-syntax-table ;probably don't need this??
    (let* ((beg (racket--complete-prefix-begin))
           (end (or (racket--complete-prefix-end beg) beg))
           (prefix (and (> end beg) (buffer-substring-no-properties beg end)))
           (cmps (and prefix (racket--complete-prefix prefix))))
      (and cmps (list beg end cmps)))))

;;; company-mode

;; Note: This works best when global-company-mode is already active,
;; before racket-mode starts. That way racket--company-setup will
;; actually run racket--do-company-setup and all will be configured.
;;
;; It does not always work when you open a racket-mode buffer, and
;; only then choose M-x company-mode to toggle it on.

(eval-after-load "company"
  '(progn
     (defun racket-company-backend (command &optional arg &rest ignore)
       (interactive (list 'interactive))
       (case command
         ('interactive (company-begin-backend 'racket-company-backend))
         ('prefix (racket--company-prefix))
         ('candidates (racket--company-candidates
                       (substring-no-properties arg)))
         ('location (racket--get-def-file+line arg))
         ('meta (racket-get-type arg))))
     (defun racket--do-company-setup (enable)
       (set (make-local-variable 'company-default-lighter) " co")
       (set (make-local-variable 'company-echo-delay) 0.01)
       (set (make-local-variable 'company-backends)
            (and enable '(racket-company-backend)))
       (company-mode (if enable 1 -1)))))

(defun racket--company-setup (enable)
  (when (fboundp 'racket--do-company-setup)
    (racket--do-company-setup enable)))

(make-variable-buffer-local
 (defvar racket--company-completions nil))

(defun racket--company-prefix ()
  (if (nth 8 (syntax-ppss))
      'stop
    (let* ((prefix (and (looking-at-p "\\_>")
                        (racket--get-repl-buffer-process)
                        (buffer-substring-no-properties
                         (racket--complete-prefix-begin)
                         (point))))
           (cmps (and prefix (racket--complete-prefix prefix))))
      (setq racket--company-completions (cons prefix cmps))
      prefix)))

(defun racket--company-candidates (prefix)
  (and (equal prefix (car racket--company-completions))
       (cdr racket--company-completions)))

;;; eldoc

(defvar racket--eldoc-cache (make-hash-table :test 'equal)
  "Used to speed up eldoc.")

(defun racket-invalidate-eldoc-cache ()
  (setq racket--eldoc-cache (make-hash-table :test 'equal)))

(defun racket-get-type (sym)
  (let ((v (gethash sym racket--eldoc-cache)))
    (or v
        (let ((v (racket--eval/sexpr (concat ",type " sym))))
          (puthash sym v racket--eldoc-cache)
          v))))

(defun racket-eldoc-function ()
  (and (> (point) (point-min))
       (save-excursion
         (and (eq ?\s (char-syntax (char-before (point))))
              (condition-case nil
                  (let* ((beg (progn (backward-sexp) (point)))
                         (end (progn (forward-sexp) (point)))
                         (sym (buffer-substring-no-properties beg end)))
                    (racket-get-type sym))
                (scan-error nil))))))

(provide 'racket-complete)

;; racket-complete.el ends here

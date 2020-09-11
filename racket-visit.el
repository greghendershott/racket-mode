;;; racket-visit.el -*- lexical-binding: t -*-

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
(require 'racket-ppss)
(require 'racket-util)

;;; visiting defs and mods

(defun racket-visit-module (&optional prefix)
  "Visit definition of module at point, e.g. net/url or \"file.rkt\".

If there is no module at point, prompt for it.

With \\[universal-argument] always prompt for the module.

Use `racket-unvisit' to return.

See also: `racket-find-collection'."
  (interactive "P")
  (let* ((v (racket--module-at-point))
         (v (if (or prefix (not v))
                (read-from-minibuffer "Visit module: " (or v ""))
              v)))
    ;; If the module name is quoted e.g. "file.rkt", just do
    ;; equivalent of `find-file-at-point'. Else ask back-end.
    (cond ((and (equal "\"" (substring v 0 1))
                (equal "\"" (substring v -1 nil)))
           (racket--push-loc)
           (find-file (expand-file-name (substring v 1 -1)))
           (message "Type M-, to return"))
          (t (racket--do-visit-def-or-mod nil `(mod ,v))))))

(defun racket--module-at-point ()
  "Treat point as a Racket module path name, possibly in a multi-in form."
  ;; `thing-at-point' 'filename matches both net/url and "file.rkt".
  ;; But. 1. Returns both without the quotes; use `syntax-ppss' to
  ;; detect latter (string). 2. Returns nil if on the opening quote;
  ;; use `forward-char' then.
  (save-excursion
    (when (eq ?\" (char-syntax (char-after))) ;2
      (forward-char))
    (pcase (thing-at-point 'filename t)
      (`() `())
      (v
       (let* ((ppss       (syntax-ppss))
              (relative-p (and (racket--ppss-string-p ppss) t)) ;1
              (multi-in   (condition-case ()
                              (progn
                                (when relative-p
                                  (goto-char (racket--ppss-string/comment-start ppss)))
                                (backward-up-list 1)
                                (backward-sexp 2)
                                (when (looking-at-p "multi-in")
                                  (forward-sexp 2)
                                  (backward-sexp 1)
                                  (when (eq ?\" (char-syntax (char-after))) ;2
                                    (forward-char))
                                  (unless (eq relative-p
                                              (and (racket--ppss-string-p ppss) t)) ;1
                                    (user-error "multi-in mixes absolute and relative paths"))
                                  (thing-at-point 'filename t)))
                            (scan-error nil))))
         (concat (if relative-p "\"" "") ;1
                 (if multi-in
                     (concat multi-in "/")
                   "")
                 v
                 (if relative-p "\"" ""))))))) ;1

(defun racket--do-visit-def-or-mod (repl-session-id cmd)
  (unless (memq major-mode '(racket-mode racket-repl-mode racket-describe-mode))
    (user-error "That doesn't work in %s" major-mode))
  (racket--cmd/async
   repl-session-id
   cmd
   (lambda (result)
     (pcase result
       (`(,path ,line ,col)
        (racket--push-loc)
        (find-file (funcall racket-path-from-racket-to-emacs-function path))
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char col)
        (message "Type M-, to return"))
       (`kernel
        (message "Defined in #%%kernel -- source not available"))
       (_
        (message "Not found"))))))

(defvar racket--loc-stack '())

(defun racket--push-loc ()
  (push (cons (current-buffer) (point))
        racket--loc-stack))

(defun racket-unvisit ()
  "Return from previous `racket-visit-definition' or `racket-visit-module'."
  (interactive)
  (if racket--loc-stack
      (pcase (pop racket--loc-stack)
        (`(,buffer . ,pt)
         (pop-to-buffer-same-window buffer)
         (goto-char pt)))
    (message "Stack empty.")))

(provide 'racket-visit)

;; racket-visit.el ends here

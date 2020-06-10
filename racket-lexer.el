;;; racket-lexer.el -*- lexical-binding: t; -*-

;; Copyright (c) 2020 by Greg Hendershott.
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

(require 'cl-macs)
(require 'racket-cmd)

;; These are simply to save the original values, to be able to restore
;; when the minor mode is disabled:
(defvar-local racket--lexer-orig-font-lock-defaults nil)
(defvar-local racket--lexer-orig-syntax-propertize-function nil)
(defvar-local racket--lexer-orig-syntax-table nil)

(defvar-local racket--lexindent-id nil)

(define-minor-mode racket-lexer-mode
  "Use #lang color-lexer."
  :lighter " Lexer"
  ;; (unless (eq major-mode 'racket-mode)
  ;;   (setq racket-lexer-mode nil)
  ;;   (user-error "racket-lexer-mode only works with racket-mode buffers"))
  (if racket-lexer-mode
      (racket--cmd/async
       nil
       `(lexindent create ,(save-restriction
                             (widen)
                             (buffer-substring-no-properties (point-min) (point-max))))
       (lambda (result)
         (font-lock-mode -1)
         (with-silent-modifications
           (remove-text-properties (point-min) (point-max)
                                   '(face nil fontified nil syntax-table nil)))
         (setq racket--lexindent-id (car result))
         (racket--lexer-propertize (cdr result))
         (setq-local racket--lexer-orig-font-lock-defaults
                     font-lock-defaults)
         (setq-local font-lock-defaults nil)
         (setq-local racket--lexer-orig-syntax-propertize-function
                     syntax-propertize-function)
         (setq-local syntax-propertize-function
                     nil)
         (setq-local racket--lexer-orig-syntax-table
                     (syntax-table))
         (set-syntax-table (make-char-table 'syntax-table '(0)))
         (add-hook 'after-change-functions
                   #'racket--lexer-after-change-hook
                   t t)
         (add-hook 'kill-buffer-hook
                   #'racket--lexer-delete
                   t t)))
    (setq-local font-lock-defaults
                racket--lexer-orig-font-lock-defaults)
    (setq-local syntax-propertize-function
                racket--lexer-orig-syntax-propertize-function)
    (set-syntax-table racket--lexer-orig-syntax-table)
    (remove-hook 'after-change-functions
                 #'racket--lexer-after-change-hook
                 t)
    (remove-hook 'kill-buffer-hook
                 #'racket--lexer-delete
                 t)
    (racket--lexer-delete)
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max)
                              '(face nil fontified nil syntax-table nil)))
    (font-lock-mode 1)
    (syntax-ppss-flush-cache (point-min))
    (syntax-propertize (point-max))))

(defun racket--lexer-delete ()
  (when racket--lexindent-id
    (racket--cmd/async
     nil
     `(lexindent delete ,racket--lexindent-id)
     #'ignore)
    (setq racket--lexindent-id nil)))

(defun racket--lexer-after-change-hook (beg end len)
  ;; This might be called as frequently as once per single changed
  ;; character.
  (racket--lexer-update beg end len))

(defun racket--lexer-update (beg end len)
  (racket--cmd/async
   nil
   `(lexindent update
               ,racket--lexindent-id
               ,beg
               ,len
               ,(save-restriction
                  (widen)
                  (buffer-substring-no-properties beg end)))
   #'racket--lexer-propertize))

(defun racket--lexer-propertize (lexemes)
  ;;(message "%S" lexemes)
  (with-silent-modifications
    (cl-labels ((put-face (beg end face) (put-text-property beg end 'face face))
                (put-stx  (beg end stx ) (put-text-property beg end 'syntax-table stx)))
      (let ((sexp-prefix-ends nil))
        (dolist (lexeme lexemes)
          (pcase-let ((`(,beg ,end ,kind ,opposite) lexeme))
            (remove-text-properties beg end
                                    '(face nil syntax-table nil))
            (cl-case kind
              (open
               (put-stx beg end (cons 4 (aref opposite 0))))
              (close
               (put-stx beg end (cons 5 (aref opposite 0))))
              (comment
               (put-stx beg (1+ beg) '(14)) ;generic comment
               (put-stx (1- end) end '(14))
               (let ((beg (+ beg 1))    ;comment _contents_ if any
                     (end (- end 2)))
                 (when (< beg end)
                   (put-stx beg end (standard-syntax-table))))
               (put-face beg end 'font-lock-comment-face))
              (sexp-comment
               ;; This is just the "#;" prefix not the following sexp.
               (put-stx beg end '(14)) ;generic comment
               (put-face beg end 'font-lock-comment-face)
               ;; Defer until we've applied following lexemes and as a
               ;; result can use e.g. `forward-sexp'.
               (push end sexp-prefix-ends))
              (string
               (put-stx beg (1+ beg) '(7)) ;string quote
               (put-stx (1- end) end '(7))
               (let ((beg (+ beg 1))    ;string _contents_ if any
                     (end (- end 2)))
                 (when (< beg end)
                   (put-stx beg end (standard-syntax-table))))
               (put-face beg end 'font-lock-string-face))
              (text
               (put-stx beg end (standard-syntax-table)))
              (constant
               (put-stx beg end '(2)) ;word
               (put-face beg end 'font-lock-constant-face))
              (error
               (put-face beg end 'error))
              (symbol
               (put-stx beg end '(3)) ;symbol
               ;; TODO: Consider using default font here, because e.g.
               ;; racket-lexer almost everything is "symbol" because
               ;; it is an identifier. Meanwhile, using a non-default
               ;; face here is helping me spot bugs.
               (put-face beg end 'font-lock-variable-name-face))
              (keyword
               (put-stx beg end '(2)) ;word
               (put-face beg end 'font-lock-keyword-face))
              (hash-colon-keyword
               (put-stx beg end '(2)) ;word
               (put-face beg end 'racket-keyword-argument-face))
              (white-space
               (put-stx beg end '(0)))
              (other
               (put-stx beg end (standard-syntax-table)))
              (otherwise
               (put-face beg end 'error)))))
        (dolist (sexp-prefix-end sexp-prefix-ends)
          (save-excursion
            (goto-char sexp-prefix-end)
            (let ((end (progn (forward-sexp  1) (point)))
                  (beg (progn (forward-sexp -1) (point))))
              (put-face beg end 'font-lock-comment-face))))))))

(provide 'racket-lexer)

;; racket-lexer.el ends here

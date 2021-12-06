;;; racket-hash-lang.el -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2021 by Greg Hendershott.
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
(require 'seq)
(require 'racket-cmd)
(require 'racket-common)
(require 'racket-indent)

(defvar-local racket--hash-lang-generation 1
  "Monotonic increasing value for hash-lang updates.

This is set to 1 when we hash-lang create, incremented every time
we do a hash-lang update, and then supplied for all other, query
hash-lang operations. That way the queries can block if necessary
until updates have completed sufficiently, i.e. re-tokenized far
enough.")

;; These are simply to save the original values, to be able to restore
;; when the minor mode is disabled:
(defvar-local racket--hash-lang-orig-font-lock-defaults nil)
(defvar-local racket--hash-lang-orig-syntax-table nil)
(defvar-local racket--hash-lang-orig-indent-line-function nil)
(defvar-local racket--hash-lang-orig-indent-region-function nil)

(defvar racket-hash-lang-mode-map
  (racket--easy-keymap-define
   `(("RET" ,#'newline-and-indent)
     ("C-M-b" ,#'racket-hash-lang-backward)
     ("C-M-f" ,#'racket-hash-lang-forward)
     ("C-M-u" ,#'racket-hash-lang-up)
     ("C-M-d" ,#'racket-hash-lang-down))))

(defconst racket--hash-lang-bare-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "w  " st)
    st))

(defconst racket--hash-lang-text-properties
  '(face fontified syntax-table racket-token)
  "The text properties we use.")

(defun racket--hash-lang-remove-text-properties (beg end)
  "Remove from region `racket--hash-lang-text-properties'."
  (remove-text-properties
   beg end
   ;; Make a property list like (face nil fontified nil ...)
   (apply #'append (mapcar (lambda (v) (cons v nil))
                           racket--hash-lang-text-properties))))

(defvar-local racket-hash-lang-mode-lighter " #lang")

;;;###autoload
(define-minor-mode racket-hash-lang-mode
  "Use color-lexer and other things supplied by a #lang.

Some #langs do not supply any special navigation or indent
functionality, in which case we use \"normal\" s-expression
navigation or indent.

\\{racket-hash-lang-mode-map}
"
  :lighter racket-hash-lang-mode-lighter
  :keymap  racket-hash-lang-mode-map
  (if racket-hash-lang-mode
      (progn
        (setq racket--hash-lang-generation 1)
        (font-lock-mode -1)
        (electric-indent-local-mode -1)
        (with-silent-modifications
          (racket--hash-lang-remove-text-properties (point-min) (point-max)))

        (setq-local racket--hash-lang-orig-font-lock-defaults
                    font-lock-defaults)
        (setq-local font-lock-defaults nil)

        (setq-local racket--hash-lang-orig-syntax-table
                    (syntax-table))
        (set-syntax-table racket--hash-lang-bare-syntax-table)

        (setq-local racket--hash-lang-orig-indent-line-function
                    indent-line-function)
        (setq-local racket--hash-lang-orig-indent-region-function
                    indent-region-function)

        (add-hook 'after-change-functions
                  #'racket--hash-lang-after-change-hook
                  t t)
        (add-hook 'kill-buffer-hook
                  #'racket--hash-lang-delete
                  t t)
        (racket--cmd/async
         nil
         `(hash-lang create
                     ,(racket--buffer-file-name)
                     ,(save-restriction
                        (widen)
                        (buffer-substring-no-properties (point-min) (point-max))))
         #'ignore))
    (setq-local font-lock-defaults
                racket--hash-lang-orig-font-lock-defaults)
    (set-syntax-table racket--hash-lang-orig-syntax-table)
    (setq-local indent-line-function
                racket--hash-lang-orig-indent-line-function)
    (setq-local indent-region-function
                racket--hash-lang-orig-indent-region-function)
    (remove-hook 'after-change-functions
                 #'racket--hash-lang-after-change-hook
                 t)
    (remove-hook 'kill-buffer-hook
                 #'racket--hash-lang-delete
                 t)
    (racket--hash-lang-delete)
    (with-silent-modifications
      (racket--hash-lang-remove-text-properties (point-min) (point-max)))
    (electric-indent-local-mode 1)
    (font-lock-mode 1)
    (syntax-ppss-flush-cache (point-min))
    (syntax-propertize (point-max))))

(defun racket--hash-lang-delete ()
  (racket--cmd/async
   nil
   `(hash-lang delete ,(racket--buffer-file-name))
   #'ignore))

(defun racket--hash-lang-after-change-hook (beg end len)
  ;; This might be called as frequently as once per single changed
  ;; character.
  (racket--cmd/async
   nil
   `(hash-lang update
               ,(racket--buffer-file-name)
               ,(cl-incf racket--hash-lang-generation)
               ,beg
               ,len
               ,(buffer-substring-no-properties beg end))
   #'ignore))

(defun racket--hash-lang-on-notify (id params)
  (with-current-buffer (find-buffer-visiting id)
    (pcase params
      (`(lang  . ,params) (racket--hash-lang-on-new-lang params))
      (`(token . ,token)  (racket--hash-lang-on-new-token token)))))

(defun racket--hash-lang-on-new-lang (plist)
  "We get this whenever the #lang changes in the user's program, including when we first open it."
 (with-silent-modifications
    (racket--hash-lang-remove-text-properties (point-min) (point-max)))
  (setq-local indent-line-function
              #'racket-hash-lang-indent-line-function)
  (setq-local indent-region-function
              (when (plist-get plist 'range-indenter)
                #'racket-hash-lang-indent-region-function))
  (setq-local racket-hash-lang-mode-lighter
              (concat " #lang"
                      (cond
                       ((plist-get plist 'range-indenter) "⇉")
                       ((plist-get plist 'line-indenter)  "→")
                       (t "")))))

(defun racket--hash-lang-on-new-token (token)
  (with-silent-modifications
    (cl-flet ((put-face (beg end face) (put-text-property beg end 'face face))
              (put-stx  (beg end stx)  (put-text-property beg end 'syntax-table stx)))
      (pcase-let ((`(,beg ,end ,kinds) token))
        (racket--hash-lang-remove-text-properties beg end)
        ;; 'racket-token is just informational for me for debugging
        (put-text-property beg end 'racket-token kinds)
        (dolist (kind kinds)
          (pcase kind
            ('parenthesis
             ;; Note: We don't attempt to put open/close paren syntax
             ;; here. The tokens might have span > 1. Also we entirely
             ;; rely on the lang's "grouping-position" function for
             ;; navigation. Some things in Emacs ecosystem might not
             ;; work, e.g. paredit. Too bad. There's no upstream
             ;; interest in making this work by using e.g.
             ;; paren-matches to set this up.
             (put-face beg end 'parenthesis))
            ('comment
             ;; I'm not sure we even need to do this; see comment about
             ;; parens above.
             (put-stx beg (1+ beg) '(11)) ;comment-start
             (put-stx (1- end) end '(12)) ;comment-end
             (let ((beg (1+ beg))         ;comment _contents_ if any
                   (end (1- end)))
               (when (< beg end)
                 (put-stx beg end '(14)))) ;generic comment
             (put-face beg end 'font-lock-comment-face))
            ('sexp-comment
             ;; This is just the "#;" prefix not the following sexp.
             (put-stx beg end '(14))    ;generic comment
             (put-face beg end 'font-lock-comment-face))
            ('sexp-comment-body
             (put-face beg end 'font-lock-comment-face))
            ('string
             (put-face beg end 'font-lock-string-face))
            ('text
             ;(put-stx beg end (standard-syntax-table))
             nil)
            ('constant
             (put-stx beg end '(2))     ;word
             (put-face beg end 'font-lock-constant-face))
            ('error
             (put-face beg end 'error))
            ('symbol
             (put-stx beg end '(3))     ;symbol
             ;; TODO: Consider using default font here, because e.g.
             ;; racket-lexer almost everything is "symbol" because
             ;; it is an identifier. Meanwhile, using a non-default
             ;; face here is helping me see behavior and spot bugs.
             (put-face beg end 'font-lock-variable-name-face))
            ('keyword
             (put-stx beg end '(2))     ;word
             (put-face beg end 'font-lock-keyword-face))
            ('hash-colon-keyword
             (put-stx beg end '(2))     ;word
             (put-face beg end 'racket-keyword-argument-face))
            ('white-space
             ;;(put-stx beg end '(0))
             nil)
            ('other
             ;;(put-stx beg end (standard-syntax-table))
             nil)))))))

(defun racket-hash-lang-indent-line-function ()
  "Maybe use #lang drracket:indentation, else `racket-indent-line'."
  (let ((bol (save-excursion (beginning-of-line) (point))))
    (if-let (amount (racket--cmd/await  ; await = :(
                     nil
                     `(hash-lang indent-amount
                                 ,(racket--buffer-file-name)
                                 ,racket--hash-lang-generation
                                 ,bol)))
        ;; When point is within the leading whitespace, move it past
        ;; the new indentation whitespace. Otherwise preserve its
        ;; position relative to the original text.
        (let ((pos (- (point-max) (point))))
          (goto-char bol)
          (skip-chars-forward " \t")
          (unless (= amount (current-column))
            (delete-region bol (point))
            (indent-to amount))
          (when (< (point) (- (point-max) pos))
            (goto-char (- (point-max) pos))))
      (racket-indent-line))))

(defun racket-hash-lang-indent-region-function (from upto)
  "Maybe use #lang drracket:range-indentation, else plain `indent-region'."
  (pcase (racket--cmd/await   ; await = :(
                    nil
                    `(hash-lang indent-region-amounts
                                ,(racket--buffer-file-name)
                                ,racket--hash-lang-generation
                                ,from
                                ,upto))
    ('false (let ((indent-region-function nil))
              (indent-region from upto)))
    (`() nil)
    (results
     (save-excursion
       (goto-char from)
        ;; drracket:range-indent docs say `results` could have more
        ;; elements than lines in from..upto, and we should ignore
        ;; extras. Handle that. (Although it could also have fewer, we
        ;; need no special handling for that here.)
        (let ((results (seq-take results (count-lines from upto))))
          (dolist (result results)
            (pcase-let ((`(,delete-amount ,insert-string) result))
              (beginning-of-line)
              (when (< 0 delete-amount) (delete-char delete-amount))
              (unless (equal "" insert-string) (insert insert-string))
              (end-of-line 2))))))))

(defun racket-hash-lang-move (direction &optional count)
  (let ((count (or count 1)))
    (pcase (racket--cmd/await       ; await = :(
            nil
            `(hash-lang grouping
                        ,(racket--buffer-file-name)
                        ,racket--hash-lang-generation
                        ,(point)
                        ,direction
                        0
                        ,count))
      ((and (pred numberp) pos)
       (goto-char pos))
      (_ (user-error "Cannot move %s %s times" direction count)))))

(defun racket-hash-lang-backward ()
  (interactive)
  (racket-hash-lang-move 'backward))

(defun racket-hash-lang-forward ()
  (interactive)
  (racket-hash-lang-move 'forward))

(defun racket-hash-lang-up ()
  (interactive)
  (racket-hash-lang-move 'up))

(defun racket-hash-lang-down ()
  (interactive)
  (racket-hash-lang-move 'down))


(provide 'racket-hash-lang)

;; racket-hash-lang.el ends here

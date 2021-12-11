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
(defvar-local racket--hash-lang-orig-electric-indent-inhibit nil)
(defvar-local racket--hash-lang-orig-blink-paren-function nil)
(defvar-local racket--hash-lang-orig-syntax-propertize-function nil)
(defvar-local racket--hash-lang-orig-font-lock-defaults nil)
(defvar-local racket--hash-lang-orig-font-lock-fontify-region-function nil)
(defvar-local racket--hash-lang-orig-syntax-table nil)
(defvar-local racket--hash-lang-orig-text-property-default-nonsticky nil)
(defvar-local racket--hash-lang-orig-indent-line-function nil)
(defvar-local racket--hash-lang-orig-indent-region-function nil)

(defvar racket-hash-lang-mode-map
  (racket--easy-keymap-define
   `(("RET"   ,#'newline-and-indent)
     ;; Disable `racket-insert-closing'; not necessarily appropriate
     ;; for all langs.
     (")"     ,#'self-insert-command)
     ("}"     ,#'self-insert-command)
     ("]"     ,#'self-insert-command)
     ("C-M-b" ,#'racket-hash-lang-backward)
     ("C-M-f" ,#'racket-hash-lang-forward)
     ("C-M-u" ,#'racket-hash-lang-up)
     ("C-M-d" ,#'racket-hash-lang-down))))

(defconst racket--hash-lang-plain-syntax-table
  (let ((st (make-syntax-table)))
    ;; Modify entries for characters for parens, strings, and
    ;; comments, setting them to word syntax instead. (For the
    ;; numbers, see Emacs Lisp Info: "Syntax Table Internals".)
    (map-char-table (lambda (key value)
                      (when (memq (car value) '(4 5 7 10 11 12))
                        (aset st key '(2))))
                    st)
    st))

(defconst racket--hash-lang-text-properties
  '(face syntax-table racket-token)
  "The text properties we use.")

(defun racket--hash-lang-remove-text-properties (beg end)
  "Remove from region `racket--hash-lang-text-properties'."
  (remove-text-properties
   beg end
   ;; Make a property list like (face nil syntax-table nil ...)
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

        (electric-indent-local-mode -1)
        (setq-local racket--hash-lang-orig-electric-indent-inhibit
                    electric-indent-inhibit)
        (setq-local electric-indent-inhibit t)

        ;; The default value of `blink-paren-function',
        ;; `blink-matching-open', can for closing paren edits cause
        ;; notifications from the back end to be lost, somehow. Maybe
        ;; due to its use of `sit-for'? Investigate for general
        ;; solution, but for now simply disable this. (But also, its
        ;; implementation heavily relies on Emacs char syntax and
        ;; likely won't work at all with non-sexp hash-langs.)
        (setq-local racket--hash-lang-orig-blink-paren-function
                    blink-paren-function)
        (setq-local blink-paren-function #'ignore)

        (with-silent-modifications
          (save-restriction
            (widen)
            (racket--hash-lang-remove-text-properties (point-min) (point-max))
            (remove-text-properties (point-min) (point-max) 'racket-here-string nil)))

        ;; Font lock
        (setq-local racket--hash-lang-orig-font-lock-defaults
                    font-lock-defaults)
        (setq-local font-lock-defaults nil)
        (setq-local racket--hash-lang-orig-font-lock-fontify-region-function
                    font-lock-fontify-region-function)
        (setq-local font-lock-fontify-region-function
                    #'racket--hash-lang-font-lock-fontify-region)
        (font-lock-flush)

        ;; Syntax table and propertization
        (setq-local racket--hash-lang-orig-syntax-table
                    (syntax-table))
        (set-syntax-table (standard-syntax-table))
        (setq-local racket--hash-lang-orig-syntax-propertize-function
                    syntax-propertize-function)
        (setq-local syntax-propertize-function nil)
        (syntax-ppss-flush-cache (point-min))

        (setq-local racket--hash-lang-orig-text-property-default-nonsticky
                    text-property-default-nonsticky)
        (setq-local text-property-default-nonsticky
                    (append (mapcar (lambda (p)
                                      (cons p t))
                                    racket--hash-lang-text-properties)
                            text-property-default-nonsticky))

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
    ;; Disable
    (setq-local font-lock-defaults
                racket--hash-lang-orig-font-lock-defaults)
    (setq-local font-lock-fontify-region-function
                racket--hash-lang-orig-font-lock-fontify-region-function)
    (set-syntax-table racket--hash-lang-orig-syntax-table)
    (setq-local text-property-default-nonsticky
                racket--hash-lang-orig-text-property-default-nonsticky)
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
      (save-restriction
        (widen)
        (racket--hash-lang-remove-text-properties (point-min) (point-max))))
    (electric-indent-local-mode 1)
    (setq-local electric-indent-inhibit
                racket--hash-lang-orig-electric-indent-inhibit)
    (setq-local blink-paren-function
                racket--hash-lang-orig-blink-paren-function)
    (setq-local racket--hash-lang-orig-syntax-propertize-function
                syntax-propertize-function)
    (syntax-ppss-flush-cache (point-min))
    (font-lock-flush)))

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
      (`(lang       . ,params)      (racket--hash-lang-on-new-lang params))
      (`(invalidate ,gen ,beg ,end) (racket--hash-lang-on-invalidate gen beg end)))))

(defun racket--hash-lang-on-new-lang (plist)
  "We get this whenever any #lang supplied attributes have changed.

We do /not/ get notified when a new lang uses exactly the same
attributes as the old one. For example changing from #lang racket
to #lang racket/base will /not/ notify us, because none of the
lang's attributes that care about have changed."
  (with-silent-modifications
    (save-restriction
      (widen)
      (racket--hash-lang-remove-text-properties (point-min) (point-max))
      (put-text-property (point-min) (point-max) 'fontified nil)))
  ;; If the lang uses racket-grouping-position, i.e. it uses
  ;; s-expressions, then use standard-syntax-table. That way other
  ;; Emacs features and packackages are more likel to work. [[TODO:
  ;; Should this instead be the same syntax-table used by normal
  ;; racket-mode??]] Otherwise, assume nothing about the lang and set
  ;; a "plain" syntax table where virtually every character is either
  ;; whitespace or word syntax (no chars signify e.g. parens,
  ;; comments, or strings).
  (set-syntax-table
   (if (plist-get plist 'racket-grouping)
       (standard-syntax-table)
     racket--hash-lang-plain-syntax-table))
  (syntax-ppss-flush-cache (point-min))
  (setq-local indent-line-function
              #'racket-hash-lang-indent-line-function)
  (setq-local indent-region-function
              (when (plist-get plist 'range-indenter)
                #'racket-hash-lang-indent-region-function))
  (setq-local racket-hash-lang-mode-lighter
              (concat " #lang"
                      (cond ((plist-get plist 'range-indenter) "⇉")
                            ((plist-get plist 'line-indenter)  "→")
                            (t "")))))

(defun racket--hash-lang-on-invalidate (_gen beg end)
  ;;;(message "invalidate %s %s %s" _gen beg end)
  (with-silent-modifications
    (save-restriction
      (widen)
      (put-text-property beg (min end (point-max)) 'fontified nil))))

(defun racket--hash-lang-font-lock-fontify-region (beg end _loudly)
  ;;;(message "fontify-region %s %s" beg end)
  ;; Note: We do this async. Not appropriate to be doing command I/O
  ;; from jit-lock-mode called from Emacs C redisplay engine.
  (racket--cmd/async
   nil
   `(hash-lang get-tokens
               ,(racket--buffer-file-name)
               ,racket--hash-lang-generation
               ,beg
               ,end)
   #'racket--hash-lang-on-tokens))

(defun racket--hash-lang-on-tokens (tokens)
  (with-silent-modifications
    (cl-flet ((put-face (beg end face) (put-text-property beg end 'face face))
              (put-stx  (beg end stx)  (put-text-property beg end 'syntax-table stx)))
      (dolist (token tokens)
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
               ;; work, e.g. paredit, although they might if the buffer
               ;; syntax-table is standard-syntax-table; see
               ;; `racket--hash-lang-on-new-lang'.
               (put-face beg end 'parenthesis))
              ('comment
               ;; I'm not sure we need to put-stx here; see comment
               ;; about parens above.
               (put-stx beg (1+ beg) '(11)) ;comment-start
               (put-stx (1- end) end '(12)) ;comment-end
               (let ((beg (1+ beg))         ;comment _contents_ if any
                     (end (1- end)))
                 (when (< beg end)
                   (put-stx beg end '(14)))) ;generic comment
               (put-face beg end 'font-lock-comment-face))
              ('sexp-comment
               ;; This is just the "#;" prefix not the following sexp.
               (put-stx beg end '(14))  ;generic comment
               (put-face beg end 'font-lock-comment-face))
              ('sexp-comment-body
               (put-face beg end 'font-lock-comment-face))
              ('string
               (put-face beg end 'font-lock-string-face))
              ('text
               (put-stx beg end racket--hash-lang-plain-syntax-table)
               nil)
              ('constant
               (put-stx beg end '(2))   ;word
               (put-face beg end 'font-lock-constant-face))
              ('error
               (put-face beg end 'error))
              ('symbol
               (put-stx beg end '(3))   ;symbol
               ;; TODO: Consider using default font here, because e.g.
               ;; racket-lexer almost everything is "symbol" because
               ;; it is an identifier. Meanwhile, using a non-default
               ;; face here is helping me see behavior and spot bugs.
               (put-face beg end 'font-lock-variable-name-face))
              ('keyword
               (put-stx beg end '(2))   ;word
               (put-face beg end 'font-lock-keyword-face))
              ('hash-colon-keyword
               (put-stx beg end '(2))   ;word
               (put-face beg end 'racket-keyword-argument-face))
              ('white-space
               ;;(put-stx beg end '(0))
               nil)
              ('other
               ;;(put-stx beg end (standard-syntax-table))
               nil))))))))

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
      (_ (user-error "Cannot move %s" direction (unless (zerop count)
                                                  (format " %s times" count)))))))

(defun racket-hash-lang-backward ()
  "Like `backward-sexp' but uses #lang supplied navigation."
  (interactive)
  (racket-hash-lang-move 'backward))

(defun racket-hash-lang-forward ()
  "Like `forward-sexp' but uses #lang supplied navigation."
  (interactive)
  (racket-hash-lang-move 'forward))

(defun racket-hash-lang-up ()
  "Like `backward-up-list' but uses #lang supplied navigation."
  (interactive)
  (racket-hash-lang-move 'up))

(defun racket-hash-lang-down ()
  "Like `down-list' but uses #lang supplied navigation."
  (interactive)
  (racket-hash-lang-move 'down))


(provide 'racket-hash-lang)

;; racket-hash-lang.el ends here

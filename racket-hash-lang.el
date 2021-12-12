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
until re-tokenization has progressed sufficiently.")

(defvar-local racket--hash-lang-changed-vars nil
  "Where we save spec to restore original values.")

(defvar racket-hash-lang-mode-map
  (racket--easy-keymap-define
   `(([remap backward-sexp]    ,#'racket-hash-lang-backward)
     ([remap forward-sexp]     ,#'racket-hash-lang-forward)
     ([remap backward-up-list] ,#'racket-hash-lang-up)
     ([remap down-list]        ,#'racket-hash-lang-down)
     ("RET"                    ,#'newline-and-indent)
     ;; Disable `racket-insert-closing'. Not necessarily appropriate
     ;; for all langs. Plus somewhat obsolete in a world of things
     ;; like paredit or electric-pair-mode.
     (")"                      ,#'self-insert-command)
     ("}"                      ,#'self-insert-command)
     ("]"                      ,#'self-insert-command))))

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
  (with-silent-modifications
    (save-restriction
      (widen)
      (racket--hash-lang-remove-text-properties (point-min) (point-max))
      (remove-text-properties (point-min) (point-max) 'racket-here-string nil)))
  (if racket-hash-lang-mode
      (progn
        (setq racket--hash-lang-generation 1)
        (electric-indent-local-mode -1)
        (setq-local
         racket--hash-lang-changed-vars
         (racket--hash-lang-set/reset
          `((electric-indent-inhibit t)
            (blink-paren-function nil)
            (font-lock-defaults nil)
            (font-lock-fontify-region-function ,#'racket--hash-lang-font-lock-fontify-region)
            ((,#'set-syntax-table ,#'syntax-table) ,racket-mode-syntax-table)
            (syntax-propertize-function nil)
            (text-property-default-nonsticky ,(append
                                               (racket--hash-lang-text-prop-list #'cons t)
                                               text-property-default-nonsticky))
            (indent-line-function ,indent-line-function)
            (indent-region-function ,indent-region-function))))
        (add-hook 'after-change-functions #'racket--hash-lang-after-change-hook t t)
        (add-hook 'kill-buffer-hook #'racket--hash-lang-delete t t)
        (racket--hash-lang-create))
    ;; Disable
    (racket--hash-lang-delete)
    (racket--hash-lang-set/reset racket--hash-lang-changed-vars)
    (remove-hook 'after-change-functions #'racket--hash-lang-after-change-hook t)
    (remove-hook 'kill-buffer-hook #'racket--hash-lang-delete t)
    (electric-indent-local-mode 1)
    (font-lock-flush))
  (syntax-ppss-flush-cache (point-min))
  (font-lock-flush))

;; Upon enabling or disabling our minor mode, we need to set/restore
;; quite a few variables. Make this less tedious and error-prone.
(defun racket--hash-lang-set/reset (specs)
  "Call with SPECS to initially set things.
Returns new specs to restore the original values. Each spec is
either (variable-symbol new-value) or ((setter-function
getter-function) new-value)."
  (mapcar (lambda (spec)
            (pcase spec
              (`((,setter ,getter) ,new-val)
               (let ((old-val (funcall getter)))
                 (funcall setter new-val)
                 `((,setter ,getter) ,old-val)))
              (`(,sym ,new-val)
               (let ((old-val (symbol-value sym)))
                 (make-local-variable sym) ;do equivalent of...
                 (set sym new-val)         ;..setq-local
                 `(,sym ,old-val)))))
          specs))

(defun racket--hash-lang-create ()
  (racket--cmd/async
   nil
   `(hash-lang create
               ,(racket--buffer-file-name)
               ,(save-restriction
                  (widen)
                  (buffer-substring-no-properties (point-min) (point-max))))
   #'ignore))

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

(defconst racket--hash-lang-plain-syntax-table
  (let ((st (make-syntax-table)))
    ;; Modify entries for characters for parens, strings, and
    ;; comments, setting them to word syntax instead. (For the these
    ;; raw syntax descriptor numbers, see Emacs Lisp Info: "Syntax
    ;; Table Internals".)
    (map-char-table (lambda (key value)
                      (when (memq (car value) '(4 5 7 10 11 12))
                        (aset st key '(2))))
                    st)
    st))

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
  ;; s-expressions, then use racket-mode-syntax-table. That way other
  ;; Emacs features and packackages are more likely to work.
  ;; Otherwise, assume nothing about the lang and set a "plain" syntax
  ;; table where virtually every character is either whitespace or
  ;; word syntax (no chars signify e.g. parens, comments, or strings).
  (set-syntax-table (if (plist-get plist 'racket-grouping)
                        racket-mode-syntax-table
                      racket--hash-lang-plain-syntax-table))
  (syntax-ppss-flush-cache (point-min))
  (setq-local indent-line-function
              #'racket-hash-lang-indent-line-function)
  (setq-local indent-region-function
              (when (plist-get plist 'range-indenter)
                #'racket-hash-lang-indent-region-function))
  (setq-local racket-hash-lang-mode-lighter
              (concat " #lang"
                      (when (plist-get plist 'racket-grouping) "()")
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
          ;; Add 'racket-token just for me to examine results using
          ;; `describe-char'; use vector b/c `describe-property-list'
          ;; assumes lists of symbols are "widgets".
          (put-text-property beg end 'racket-token (apply #'vector kinds))
          (dolist (kind kinds)
            (pcase kind
              ('comment
               ;; Although I'm not 100% sure we need to put-stx here I
               ;; think it might be important to make sure that the
               ;; buffer's syntax-table does not consider things
               ;; within comments to be e.g. parens/strings? This may
               ;; help when people use Emacs features that rely on
               ;; char syntax.
               (put-stx beg (1+ beg) '(11)) ;comment-start
               (put-stx (1- end) end '(12)) ;comment-end
               (let ((beg (1+ beg))         ;comment _contents_ if any
                     (end (1- end)))
                 (when (< beg end)
                   (put-stx beg end '(14)))) ;generic comment
               (put-face beg end 'font-lock-comment-face))
              ('sexp-comment ;just the "#;" prefix not following sexp
               (put-stx beg end '(14))  ;generic comment
               (put-face beg end 'font-lock-comment-face))
              ('sexp-comment-body (put-face beg end 'font-lock-comment-face))
              ('parenthesis (put-face beg end 'parenthesis))
              ('string (put-face beg end 'font-lock-string-face))
              ('text (put-stx beg end racket--hash-lang-plain-syntax-table))
              ('constant (put-face beg end 'font-lock-constant-face))
              ('error (put-face beg end 'error))
              ('symbol
               ;; TODO: Consider using default font here, because e.g.
               ;; racket-lexer almost everything is "symbol" because
               ;; it is an identifier. Meanwhile, using a non-default
               ;; face here is helping me see behavior and spot bugs.
               (put-face beg end 'font-lock-variable-name-face))
              ('keyword (put-face beg end 'font-lock-keyword-face))
              ('hash-colon-keyword (put-face beg end 'racket-keyword-argument-face))
              ('white-space nil)
              ('other nil))))))))

(defconst racket--hash-lang-text-properties
  '(face syntax-table racket-token)
  "The text properties we use.")

(defun racket--hash-lang-text-prop-list (f val)
  (mapcar (lambda (prop-sym) (funcall f prop-sym val))
          racket--hash-lang-text-properties))

(defun racket--hash-lang-remove-text-properties (beg end)
  "Remove `racket--hash-lang-text-properties' from region BEG..END."
  (remove-text-properties beg end
                          (apply #'append
                                 (racket--hash-lang-text-prop-list #'list nil))))

(defun racket-hash-lang-indent-line-function ()
  "Use drracket:indentation supplied by the lang.

If a lang doesn't supply this, or if the supplied function ever
returns false, then we always use the standard s-expression
indenter from syntax-color/racket-indentation.

We never use `racket-indent-line' from traditional
`racket-mode'."
  (let* ((bol (save-excursion (beginning-of-line) (point)))
         (pos (- (point-max) (point)))
         (col (racket--cmd/await        ; await = :(
               nil
               `(hash-lang indent-amount
                           ,(racket--buffer-file-name)
                           ,racket--hash-lang-generation
                           ,(point)))))
    (goto-char bol)
    (skip-chars-forward " \t") ;;TODO: Is this reliable for all langs?
    (unless (= col (current-column))
      (delete-region bol (point))
      (indent-to col))
    ;; When point is within the leading whitespace, move it past the
    ;; new indentation whitespace. Otherwise preserve its position
    ;; relative to the original text.
    (when (< (point) (- (point-max) pos))
      (goto-char (- (point-max) pos)))))

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
      (_ (user-error "Cannot move %s%s" direction (if (zerop count)
                                                      ""
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

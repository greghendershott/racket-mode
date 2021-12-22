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
(require 'racket-repl)

(defvar-local racket--hash-lang-id nil
  "Used to identify the back end hash-lang object.
Although it's tempting to use `buffer-file-name' for the ID, not
all buffers have files, especially `racket-repl-mode' buffers.
Although it's tempting to use `buffer-name', buffers can be
renamed. Although it's tempting to use the buffer object, we
can't serialize that. So use `gensym' for this.")

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
   `(("RET"   ,#'racket-hash-lang-return)
     (")"     ,#'self-insert-command) ;not `racket-insert-closing'
     ("}"     ,#'self-insert-command) ;not `racket-insert-closing'
     ("]"     ,#'self-insert-command) ;not `racket-insert-closing'
     ("C-M-b" ,#'racket-hash-lang-backward)
     ("C-M-f" ,#'racket-hash-lang-forward)
     ("C-M-u" ,#'racket-hash-lang-up)
     ("C-M-d" ,#'racket-hash-lang-down))))

(defvar-local racket-hash-lang-mode-lighter " #lang")

(defvar-local racket--hash-lang-offset 0
  "The offset into the buffer that is treated as a hash-lang.

For `racket-mode' buffers this is 0.

For `racket-repl-mode' buffers this is the offset after the last
'output field -- i.e. where the user starts typing after the last
REPL prompt. We automatically disable `racket-hash-lang-mode'
before each run, and enable it after each run. When enabled we
create a back end hash-lang% object representing just this
\"tail\" portion of the buffer. Font-lock and indent within the
tail use the hash-lang, else defaults.")

;;;###autoload
(define-minor-mode racket-hash-lang-mode
  "Use color-lexer, indent, and navigation supplied by a #lang.

Minor mode to enhance `racket-mode' and `racket-repl-mode' buffers.

\\{racket-hash-lang-mode-map}
"
  :lighter racket-hash-lang-mode-lighter
  :keymap  racket-hash-lang-mode-map
  (with-silent-modifications
    (save-restriction
      (widen)
      (pcase major-mode
        ('racket-mode      (setq racket--hash-lang-offset 0))
        ('racket-repl-mode (setq racket--hash-lang-offset
                                 (save-excursion
                                   (goto-char (point-max))
                                   (- (field-beginning) (point-max)))))
        (_ (error "racket-hash-lang-mode only works with racket-mode and racket-repl-mode buffers")))
      (racket--hash-lang-remove-text-properties (+ (point-min) racket--hash-lang-offset)
                                                (point-max))
      (remove-text-properties (+ (point-min) racket--hash-lang-offset)
                              (point-max)
                              'racket-here-string nil)
      (cond
       (racket-hash-lang-mode
        (setq-local racket--hash-lang-generation 1)
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
       (t                               ;disable
        (racket--hash-lang-delete)
        (racket--hash-lang-set/reset racket--hash-lang-changed-vars)
        (remove-hook 'after-change-functions #'racket--hash-lang-after-change-hook t)
        (remove-hook 'kill-buffer-hook #'racket--hash-lang-delete t)
        (electric-indent-local-mode 1)))
      (save-restriction
        (widen)
        (syntax-ppss-flush-cache (+ (point-min) racket--hash-lang-offset)))))
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

;; For racket-repl-mode buffers associated with a racket-mode buffer
;; using racket-hash-lang-mode, racket-hash-lang-mode is automatically
;; disabled before and enabled after each run. It is only used at the
;; end of the repl buffer, for user input for the current interaction;
;; see `racket--hash-lang-offset'.

(defun racket--hash-lang-before-run-hook ()
  (with-racket-repl-buffer
    (when racket-hash-lang-mode
      (racket-hash-lang-mode -1))))
(add-hook 'racket--repl-before-run-hook #'racket--hash-lang-before-run-hook)

(defun racket--hash-lang-after-run-hook ()
  (when racket-hash-lang-mode
    (with-racket-repl-buffer
      (racket-hash-lang-mode 1))))
(add-hook 'racket--repl-after-run-hook #'racket--hash-lang-after-run-hook)

(defun racket--hash-lang-create ()
  (setq-local racket--hash-lang-id
              (cl-gensym (concat "racket-hash-lang-" (buffer-name) "-")))
  (racket--cmd/await
   nil
   `(hash-lang create
               ,racket--hash-lang-id
               ,(when (eq major-mode 'racket-repl-mode) racket--repl-session-id)
               ,(save-restriction
                  (widen)
                  (buffer-substring-no-properties (+ (point-min) racket--hash-lang-offset)
                                                  (point-max))))))

(defun racket--hash-lang-delete ()
  (racket--cmd/async
   nil
   `(hash-lang delete ,racket--hash-lang-id)))

(defun racket--hash-lang-after-change-hook (beg end len)
  ;; This might be called as frequently as once per single changed
  ;; character.
  (racket--cmd/async
   nil
   `(hash-lang update
               ,racket--hash-lang-id
               ,(cl-incf racket--hash-lang-generation)
               ,(- beg racket--hash-lang-offset)
               ,len
               ,(buffer-substring-no-properties beg end))))

(defun racket--hash-lang-on-notify (id params)
  (when-let ((buf (cl-some (lambda (b)
                             (equal (buffer-local-value 'racket--hash-lang-id b)
                                    id))
                           (buffer-list))))
   (with-current-buffer buf
     (pcase params
       (`(lang . ,params)        (racket--hash-lang-on-new-lang params))
       (`(update ,gen ,beg ,end) (racket--hash-lang-on-update gen beg end))))))

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
      (racket--hash-lang-remove-text-properties (+ (point-min) racket--hash-lang-offset)
                                                (point-max))
      (put-text-property (+ (point-min) racket--hash-lang-offset)
                         (point-max)
                         'fontified nil)
      ;; If the lang uses racket-grouping-position, i.e. it uses
      ;; s-expressions, then use racket-mode-syntax-table. That way other
      ;; Emacs features and packackages are more likely to work.
      ;; Otherwise, assume nothing about the lang and set a "plain" syntax
      ;; table where virtually every character is either whitespace or
      ;; word syntax (no chars signify e.g. parens, comments, or strings).

      (set-syntax-table (if (plist-get plist 'racket-grouping)
                            racket-mode-syntax-table
                          racket--hash-lang-plain-syntax-table))
      (syntax-ppss-flush-cache (+ (point-min) racket--hash-lang-offset))
      (setq-local indent-line-function
                  #'racket-hash-lang-indent-line-function)
      (setq-local indent-region-function
                  (when (plist-get plist 'range-indenter)
                    #'racket-hash-lang-indent-region-function))
      (setq-local racket-hash-lang-mode-lighter
                  (concat " #lang"
                          (when (plist-get plist 'racket-grouping) "()")
                          (when (plist-get plist 'range-indenter) "⇉"))))))

(defun racket--hash-lang-on-update (_gen beg end)
  ;;;(message "update %s %s %s" _gen beg end)
  (with-silent-modifications
    (save-restriction
      (widen)
      (put-text-property (+ beg racket--hash-lang-offset)
                         (min end (point-max))
                         'fontified nil))))

(defun racket--hash-lang-font-lock-fontify-region (beg end loudly)
  ;;;(message "fontify-region %s %s" beg end)
  (if (or (<= end racket--hash-lang-offset)
          (eq 'output (field-at-pos beg))) ;for racket-repl-mode
      (font-lock-default-fontify-region beg end loudly)
    ;; Note: We do this async. Not appropriate to be doing command I/O
    ;; from jit-lock-mode called from Emacs C redisplay engine.
    (racket--cmd/async
     nil
     `(hash-lang get-tokens
                 ,racket--hash-lang-id
                 ,racket--hash-lang-generation
                 ,(- beg racket--hash-lang-offset)
                 ,(- end racket--hash-lang-offset))
     #'racket--hash-lang-on-tokens)))

(defun racket--hash-lang-on-tokens (tokens)
  (with-silent-modifications
    (cl-flet ((put-face (beg end face) (put-text-property beg end 'face face))
              (put-stx  (beg end stx)  (put-text-property beg end 'syntax-table stx)))
      (dolist (token tokens)
        (pcase-let ((`(,beg ,end ,kinds) token))
          (let ((beg (+ beg racket--hash-lang-offset))
                (end (+ end racket--hash-lang-offset)))
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
                ('other (put-face beg end 'font-lock-doc-face))
                ('white-space nil)))))))))

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
                           ,racket--hash-lang-id
                           ,racket--hash-lang-generation
                           ,(- (point) racket--hash-lang-offset)))))
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
                                ,racket--hash-lang-id
                                ,racket--hash-lang-generation
                                ,(- from racket--hash-lang-offset)
                                ,(- upto racket--hash-lang-offset)))
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
                        ,racket--hash-lang-id
                        ,racket--hash-lang-generation
                        ,(- (point) racket--hash-lang-offset)
                        ,direction
                        0
                        ,count))
      ((and (pred numberp) pos)
       (goto-char (+ pos racket--hash-lang-offset)))
      (_ (user-error "Cannot move %s%s" direction (if (memq count '(-1 0 1))
                                                      ""
                                                    (format " %s times" count)))))))

(defun racket-hash-lang-backward (&optional count)
  "Like `backward-sexp' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'backward count))

(defun racket-hash-lang-forward (&optional count)
  "Like `forward-sexp' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'forward count))

(defun racket-hash-lang-up (&optional count)
  "Like `backward-up-list' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'up count))

(defun racket-hash-lang-down (&optional count)
  "Like `down-list' but uses #lang supplied navigation."
  (interactive "^p")
  (racket-hash-lang-move 'down count))

(defun racket-hash-lang-return ()
  (interactive)
  (if (eq major-mode 'racket-mode)
      (newline-and-indent)
    (racket-repl-submit)))

(provide 'racket-hash-lang)

;; racket-hash-lang.el ends here

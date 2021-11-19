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
(defvar-local racket--hash-lang-orig-syntax-propertize-function nil)
(defvar-local racket--hash-lang-orig-syntax-table nil)
(defvar-local racket--hash-lang-orig-indent-line-function nil)
(defvar-local racket--hash-lang-orig-indent-region-function nil)
(defvar-local racket--hash-lang-orig-forward-sexp-function nil)

;;;###autoload
(define-minor-mode racket-hash-lang-mode
  "Use color-lexer and indenter supplied by the #lang.

This mode allows a #lang to support multi-character open and
close tokens, by setting the variable `forward-sexp-function'.
However this means various Emacs features and packages that do
not use `forward-sexp' -- and instead use `scan-list' or look
specifically for parentheses -- will not work well. See also
`racket-sexp-hash-lang-mode'.

\\{racket-hash-lang-mode-map}
"
  :lighter " #lang"
  (racket--hash-lang-mode racket-hash-lang-mode t))

;;;###autoload
(define-minor-mode racket-sexp-hash-lang-mode
  "Use color-lexer and indenter supplied by the #lang.

When a #lang has a sexp surface syntax, this mode allows more
Emacs features to work, in contrast to `racket-hash-lang-mode'.

\\{racket-hash-lang-mode-map}
"
  :lighter " #lang()"
  (racket--hash-lang-mode racket-sexp-hash-lang-mode nil))

(defun racket--hash-lang-mode (mode-var forward-sexp-function-p)
  "Helper function for both of our mode functions."
  (if mode-var
      (progn
        (setq racket--hash-lang-generation 1)
        (font-lock-mode -1)
        (with-silent-modifications
          (remove-text-properties (point-min) (point-max)
                                  '(face nil fontified nil syntax-table nil)))

        (setq-local racket--hash-lang-orig-font-lock-defaults
                    font-lock-defaults)
        (setq-local font-lock-defaults nil)

        (setq-local racket--hash-lang-orig-syntax-propertize-function
                    syntax-propertize-function)
        (setq-local syntax-propertize-function nil)

        (setq-local racket--hash-lang-orig-syntax-table
                    (syntax-table))
        ;; Mostly we use 'syntax-table text properties. However some
        ;; things (e.g. paredit) might do e.g. (char-syntax
        ;; (char-before)) which will ignore that. So we need some
        ;; syntax-table with some reasonable default(s). Whitespace
        ;; seems like a good choice, except that breaks
        ;; `paredit-delete-leading-whitespace'. Instead let's use
        ;; symbol. That might cause its own problem, but it's my
        ;; least-worst idea, for the time being.
        (set-syntax-table (make-char-table 'syntax-table '(3)))

        (setq-local racket--hash-lang-orig-indent-line-function
                    indent-line-function)
        (setq-local indent-line-function
                    #'racket-hash-lang-indent-line-function)

        (setq-local racket--hash-lang-orig-indent-region-function
                    indent-region-function)
        (setq-local indent-region-function
                    #'racket-hash-lang-indent-region-function)

        (setq-local racket--hash-lang-orig-forward-sexp-function
                    forward-sexp-function)
        (setq-local forward-sexp-function
                    (and forward-sexp-function-p
                         #'racket-hash-lang-forward-sexp-function))

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
    (setq-local syntax-propertize-function
                racket--hash-lang-orig-syntax-propertize-function)
    (set-syntax-table racket--hash-lang-orig-syntax-table)
    (setq-local indent-line-function
                racket--hash-lang-orig-indent-line-function)
    (setq-local indent-region-function
                racket--hash-lang-orig-indent-region-function)
    (setq-local forward-sexp-function
                racket--hash-lang-orig-forward-sexp-function)
    (remove-hook 'after-change-functions
                 #'racket--hash-lang-after-change-hook
                 t)
    (remove-hook 'kill-buffer-hook
                 #'racket--hash-lang-delete
                 t)
    (racket--hash-lang-delete)
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max)
                              '(face nil fontified nil syntax-table nil)))
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

(defconst racket--string-content-syntax-table
  (let ((st (copy-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\" "w" st)
    ;; FIXME? Should we iterate the entire table looking for string
    ;; _values_ and set them _all_ to "w" instead?
    st)
  "A syntax-table property value for _inside_ strings.
Specifically, do _not_ treat quotes as string syntax. That way,
things like #rx\"blah\" in Racket, which are lexed as one single
string token, will not give string syntax to the open quote after
x.")

(defun racket--hash-lang-on-token (id token)
  (with-current-buffer (find-buffer-visiting id)
    (racket--hash-lang-propertize token)))

(defun racket--hash-lang-propertize (token)
  (with-silent-modifications
    (cl-flet ((put-face (beg end face) (put-text-property beg end 'face face))
              (put-stx  (beg end stx ) (put-text-property beg end 'syntax-table stx)))
      (let ((sexp-prefix-ends nil))
        (pcase-let ((`(,beg ,end ,kind . ,maybe-paren-data) token))
          (remove-text-properties beg end
                                  '(face nil syntax-table nil))
          (cl-case kind
            ;; When our forward-sexp-function is in use, ignore
            ;; parenthesis tokens. This supports hash-langs with
            ;; multi-char open and close tokens, both. Emacs uses
            ;; char-syntax -- /char/. This won't work. Instead we
            ;; must rely on `forward-sexp-function' and hope enough
            ;; things use it via `forward-sexp'.
            ;;
            ;; Otherwise, assume the tokens are for an sexpr lang,
            ;; and only open tokens might be multi-char, e.g. "#("
            ;; or "#hasheq(". Handle those as expression-prefix
            ;; syntax followed by a single char with open-paren
            ;; syntax. Although this is less general, it lets more
            ;; Emacs functions and packages (e.g. paredit) work well
            ;; even when they do not always use forward-sexp, and
            ;; instead do things like use `scan-lists' or look for
            ;; paren char-syntax directy. :(
            (parenthesis
             (unless (equal forward-sexp-function
                            #'racket-hash-lang-forward-sexp-function)
               (pcase-let ((`(,open-p ,opposite) maybe-paren-data))
                 (cond (open-p
                        (when (< 1 (- end beg))
                          (put-stx beg (- end 1) '(6)))
                        (put-stx (- end 1) end (cons 4 (aref opposite 0))))
                       (t
                        (put-stx beg end (cons 5 (aref opposite 0))))))))
            (comment
             (put-stx beg (1+ beg) '(14)) ;generic comment
             (put-stx (1- end) end '(14))
             (let ((beg (1+ beg))    ;comment _contents_ if any
                   (end (1- end)))
               (when (< beg end)
                 (put-stx beg end (standard-syntax-table))))
             (put-face beg end 'font-lock-comment-face))
            (sexp-comment
             ;; This is just the "#;" prefix not the following sexp.
             (put-stx beg end '(14)) ;generic comment
             (put-face beg end 'font-lock-comment-face)
             ;; Defer until we've applied following tokens and as a
             ;; result can use e.g. `forward-sexp'.
             (push end sexp-prefix-ends))
            (string
             (put-stx beg (1+ beg) '(15)) ;generic string
             (put-stx (1- end) end '(15))
             (let ((beg (+ beg 1))    ;string _contents_ if any
                   (end (- end 2)))
               (when (< beg end)
                 (put-stx beg end racket--string-content-syntax-table)))
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
            (otherwise nil)))
        (dolist (sexp-prefix-end sexp-prefix-ends)
          (save-excursion
            (goto-char sexp-prefix-end)
            (let ((end (progn (forward-sexp  1) (point)))
                  (beg (progn (forward-sexp -1) (point))))
              (put-face beg end 'font-lock-comment-face))))))))

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

;; TODO: Actually exercise/test this using some lang like rhombus that
;; supplies drracket:range-indentation.
(defun racket-hash-lang-indent-region-function (from upto)
  "Maybe use #lang drracket:range-indentation, else plain `indent-region'."
  (if-let (results (racket--cmd/await   ; await = :(
                    nil
                    `(hash-lang indent-region-amounts
                                ,(racket--buffer-file-name)
                                ,racket--hash-lang-generation
                                ,from
                                ,upto)))
      (save-excursion
        ;; drracket:range-indent docs say `results` could have more
        ;; elements than lines in from..upto, and we should ignore
        ;; extras. Handle that. (Although it could also have fewer, we
        ;; need no special handling for that here.)
        (let ((results (seq-take results (count-lines from upto))))
          (dolist (result results)
            (pcase-let ((`(,delete-amount ,insert-string) result))
              (beginning-of-line)
              (delete-char delete-amount)
              (insert insert-string)
              (end-of-line 2)))))
    (let ((indent-region-function nil))
      (indent-region from upto))))

(defun racket-hash-lang-forward-sexp-function (&optional arg)
  "Maybe use #lang drracket:grouping-position, else use sexp motion."
  (let ((dir (if (or (not arg) (< 0 arg)) 'forward 'backward))
        (count (abs arg)))
    (pcase (racket--cmd/await           ; await = :(
            nil
            `(hash-lang grouping
                        ,(racket--buffer-file-name)
                        ,racket--hash-lang-generation
                        ,(point)
                        ,dir
                        0
                        ,count))
      ((and (pred numberp) pos)
       (goto-char pos))
      ('use-default-s-expression
       (let ((forward-sexp-function nil))
         (forward-sexp arg)))
      (`(,from ,upto)
       (signal 'scan-error (list (format "Cannot move %s" dir)
                                 from upto)))
      (v (error "unexpected grouping-position value %s" v)))))

;; TODO: Advise e.g. up-list/down-list using drracket:grouping-position, too?

(defun racket-hash-lang-up ()
  (interactive)
  (pcase (racket--cmd/await             ; await = :(
          nil
          `(hash-lang grouping
                      ,(racket--buffer-file-name)
                      ,racket--hash-lang-generation
                      ,(point)
                      up
                      0
                      1))
    ((and (pred numberp) pos)
     (goto-char pos))
    ('use-default-s-expression
     (racket-backward-up-list))
    (_ (user-error "Cannot move up"))))

(defun racket-hash-lang-down ()
  (interactive)
  (pcase (racket--cmd/await             ; await = :(
          nil
          `(hash-lang grouping
                      ,(racket--buffer-file-name)
                      ,racket--hash-lang-generation
                      ,(point)
                      down
                      0
                      1))
    ((and (pred numberp) pos)
     (goto-char pos))
    ('use-default-s-expression
     (down-list))
    (_ (user-error "Cannot move down"))))


(provide 'racket-hash-lang)

;; racket-hash-lang.el ends here

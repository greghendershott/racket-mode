;;; racket-common.el

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

;; Things used by both racket-mode and racket-repl-mode

(require 'thingatpt)
(require 'racket-custom)
(require 'racket-keywords-and-builtins)
(require 'racket-font-lock)
(require 'racket-indent)
(require 'cl-lib)

(defvar racket-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))
    ;; Symbol constituents
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "< 2 " st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar racket-mode-abbrev-table nil)
(define-abbrev-table 'racket-mode-abbrev-table ())

(defconst racket-sexp-comment-syntax-table
  (let ((st (make-syntax-table racket-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(defun racket-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
             (eq (char-after (nth 8 state)) ?#)
             (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
            (end
             (condition-case err
                 (let ((parse-sexp-lookup-properties nil))
                   (goto-char (+ 2 (nth 8 state)))
                   ;; FIXME: this doesn't handle the case where the sexp
                   ;; itself contains a #; comment.
                   (forward-sexp 1)
                   (point))
               (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2)
                             'syntax-table racket-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

(defconst racket-syntax-propertize-function
  (syntax-propertize-rules
   ;; Treat #px"" and #rx"" as single sexpr for navigation and indent.
   ((rx (group (or "#px" "#rx"))
        (group "\"")
        (group (zero-or-more (not (any "\""))))
        (group "\""))
    (1 "'")
    (2 "\"")
    (3 (ignore))
    (4 "\""))))

(defun racket--variables-for-both-modes ()
  ;; Set many things explicitly. We wouldn't need to set most of these
  ;; if our editing major mode, `racket-mode`, were derived from
  ;; `scheme-mode` instead of from `prog-mode`. So why do it this way?
  ;; Because of our `racket-repl-mode`. That needs to derive from
  ;; `comint-mode`, therefore it needs to set them explicitly. Setting them
  ;; all here ensures consistency. And in that case, racket-mode need not
  ;; derive from scheme-mode, it can derive from just prog-mode.
  (set-syntax-table racket-mode-syntax-table)
  (setq-local syntax-propertize-function racket-syntax-propertize-function)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local local-abbrev-table racket-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'racket-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local comment-start ";")
  (setq-local comment-add 1)            ;default to `;;' in comment-region
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning:
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  ;; Font lock mode uses this only when it KNOWS a comment is starting:
  (setq-local font-lock-comment-start-skip ";+ *")
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'racket-indent-function)
  (racket--set-indentation)
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults
              `(,racket-font-lock-keywords     ;keywords
                nil                            ;keywords-only?
                nil                            ;case-fold?
                (("+-*/.<>=!?$%_&~^:" . "w")   ;syntax-alist
                 (?# . "w 14"))
                beginning-of-defun             ;syntax-begin
                ;; Additional variables:
                (font-lock-mark-block-function . mark-defun)
                (font-lock-syntactic-face-function
                 . racket-font-lock-syntactic-face-function)
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props syntax-table)))
  (setq-local completion-at-point-functions '(racket-complete-at-point))
  (setq-local eldoc-documentation-function 'racket-eldoc-function))


;;; Insert lambda char (like DrRacket)

(defconst racket-lambda-char (make-char 'greek-iso8859-7 107)
  "Character inserted by `racket-insert-labmda'.")

(defun racket-insert-lambda ()
  (interactive)
  (insert-char racket-lambda-char 1))


;;; Automatically insert matching \?) \?] or \?}

(defvar racket-matching-parens
  '(( ?\( . ?\) )
    ( ?\[ . ?\] )
    ( ?\{ . ?\} )))

(defun racket--insert-closing (prefix char)
  (insert char)
  (unless prefix
    (let ((open-pt (condition-case nil
                       (scan-sexps (point) -1)
                     (error (beep) nil))))
      (when open-pt
        (let* ((open-char
                (aref (buffer-substring-no-properties open-pt (1+ open-pt)) 0))
               (close-pair (assoc open-char racket-matching-parens)))
          (when close-pair
            (let ((close-char (cdr close-pair)))
              (when (not (= close-char char))
                (call-interactively 'delete-backward-char)
                (insert close-char))))))))
  (when blink-paren-function (funcall blink-paren-function)))

(defun racket-insert-closing-paren (&optional prefix)
  (interactive "P")
  (racket--insert-closing prefix ?\)))

(defun racket-insert-closing-bracket (&optional prefix)
  (interactive "P")
  (racket--insert-closing prefix ?\]))

(defun racket-insert-closing-brace (&optional prefix)
  (interactive "P")
  (racket--insert-closing prefix ?\}))

;;; Smart open bracket

(defconst racket--smart-open-bracket-data
  (eval-when-compile
    `(;; cond-like
      (0 0 ,(rx (seq "("
                     (or "cond"
                         "match-lambda"
                         "match-lambda*"
                         "match-lambda**")
                     (or space line-end))))
      ;; case-like
      (2 0 ,(rx (seq "("
                     (or "case"
                         "match"
                         "match*"
                         "syntax-parse"
                         "syntax-rules")
                     (or space line-end))))
      ;; syntax-case
      (3 0 ,(rx (seq "("
                     (or "syntax-case")
                     (or space line-end))))
      ;; syntax-case*
      (4 0 ,(rx (seq "("
                     (or "syntax-case*")
                     (or space line-end))))
      ;; let-like
      ;;
      ;; In addition to the obvious suspects with 'let' in the name,
      ;; handles forms like 'parameterize', 'with-handlers', 'for',
      ;; and 'for/fold' accumulator bindings.
      (0 1 ,(rx (seq (or "for"
                         "for/list"
                         "for/vector"
                         "for/hash"
                         "for/hasheq"
                         "for/hasheqv"
                         "for/and"
                         "for/or"
                         "for/lists"
                         "for/first"
                         "for/last"
                         "for/fold"
                         "for/flvector"
                         "for/extflvector"
                         "for/set"
                         "for/sum"
                         "for/product"
                         "for*"
                         "for*/list"
                         "for*/vector"
                         "for*/hash"
                         "for*/hasheq"
                         "for*/hasheqv"
                         "for*/and"
                         "for*/or"
                         "for*/lists"
                         "for*/first"
                         "for*/last"
                         "for*/fold"
                         "for*/flvector"
                         "for*/extflvector"
                         "for*/set"
                         "for*/sum"
                         "for*/product"
                         "fluid-let"
                         "let"
                         "let*"
                         "let*-values"
                         "let-struct"
                         "let-syntax"
                         "let-syntaxes"
                         "let-values"
                         "let/cc"
                         "let/ec"
                         "letrec"
                         "letrec-syntax"
                         "letrec-syntaxes"
                         "letrec-syntaxes+values"
                         "letrec-values"
			 "match-let"
			 "match-let-values"
			 "match-let*-values"
			 "match-letrec"
			 "parameterize"
			 "parameterize*"
			 "with-handlers"
			 "with-handlers*"
                         "with-syntax"
                         "with-syntax*")
                     (or space line-end))))
      ;; for/fold bindings
      ;;
      ;; Note: Previous item handles the first, accumulators subform.
      (0 2 ,(rx (seq (or "for/fold"
                         "for*/fold")
                     (or space line-end))))

      ;; named-let bindings
      ;;
      (0 2 ,(rx (seq "let" (1+ whitespace) (1+ (not (in "()[]{}\",'`;#|\" "))))))))

  "A list of lists. Each sub list is arguments to supply to
  `racket--smart-open-bracket-helper'.")

(defun racket--smart-open-bracket-helper (pre-backward-sexps
                                          post-backward-sexps
                                          regexp)
  "Is point is a subform (of a known form REGEXP) that should open with '['.

Returns \"[\" or nil."
  (and (save-excursion
         (condition-case ()
             (progn (backward-sexp pre-backward-sexps) t)
           (error nil)))
       (save-excursion
         (condition-case ()
             (let ((pt (point)))
               (racket-backward-up-list) ;works even in strings
               (backward-sexp post-backward-sexps)
               (when (or (racket--in-string-or-comment (point) pt)
                         (looking-at-p regexp))
                 "["))
           (error nil)))))

(defun racket-smart-open-bracket ()
  "Automatically insert a '(' or a '[' as appropriate.

By default, inserts a '('. Inserts a '[' in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

To force insert '[', use `quoted-insert': \\[quoted-insert] [.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

Combined with `racket-insert-closing-bracket', this means that
you can press the unshifted '[' and ']' keys to get whatever
delimiters follow the Racket conventions for these forms. (When
`paredit-mode' is active, you need not even press ']'; this
command calls `paredit-open-round' or `paredit-open-square' to
work as usual.)

To disable: Customize `racket-smart-open-bracket-enable'. This is
like the 'Automatically adjust opening square brackets'
preference in Dr. Racket."
  (interactive)
  (let ((ch (or (and (not racket-smart-open-bracket-enable) "[")
                (cl-some (lambda (xs)
                           (apply #'racket--smart-open-bracket-helper xs))
                         racket--smart-open-bracket-data)
                (racket--previous-sexp-open)
                "(")))
    (if (fboundp 'racket--paredit-aware-open)
        (racket--paredit-aware-open ch)
      (insert ch))))

(defun racket--in-string-or-comment (from to)
  "See if point is in a string or comment, without moving point."
  (save-excursion
    (let ((parse (parse-partial-sexp from to)))
      (or (elt parse 3)
          (elt parse 4)))))

(defun racket--previous-sexp-open ()
  (save-excursion
    (condition-case ()
        (progn
          (backward-sexp)
          (let ((ch (buffer-substring-no-properties (point) (1+ (point)))))
            (when (member ch '("(" "[" "{")) ch)))
      (error nil))))

(eval-after-load 'paredit
  '(progn
     (defvar racket--paredit-original-open-bracket-binding
       (lookup-key paredit-mode-map (kbd "["))
       "The previous `paredit-mode-map' binding for [. We don't
assume it's `paredit-open-square', in case someone else is doing
this, too.")

     (add-hook 'paredit-mode-hook
               (lambda ()
                 (define-key paredit-mode-map
                   (kbd "[") 'racket--paredit-open-square)))

     (defun racket--paredit-open-square ()
       "`racket-smart-open-bracket' or original `paredit-mode-map' binding.

To be compatible with `paredit-mode', `racket-smart-open-bracket'
must intercept [ and decide whether to call `paredit-open-round'
or `paredit-open-square'. To do so it must modify
`paredit-mode-map', which affects all major modes. Therefore we
check whether the current buffer's major mode is `racket-mode'.
If not we call `racket--paredit-original-open-bracket-binding'."
       (interactive)
       (if (eq major-mode 'racket-mode)
           (racket-smart-open-bracket)
         (funcall racket--paredit-original-open-bracket-binding)))

     (defun racket--paredit-aware-open (ch)
       "A paredit-aware helper for `racket-smart-open-bracket'.

When `paredit-mode' is active, use its functions (such as
`paredit-open-round') instead of directly `insert'ing. Note: This
this isn't defined unless paredit is loaded, so check for its
existence using `fboundp'."
       (let ((paredit-active (and (boundp 'paredit-mode) paredit-mode)))
         (cond ((not paredit-active) (insert ch))
               ((equal ch "(")       (paredit-open-round))
               ((equal ch "[")       (paredit-open-square))
               ((equal ch "{")       (paredit-open-curly))
               (t                    (insert ch)))))))

;;; Cycle paren shapes

(defun racket-cycle-paren-shapes ()
  "In an s-expression, move to the opening, and cycle the shape among () [] {}"
  (interactive)
  (save-excursion
    (unless (looking-at-p (rx (any "([{")))
      (backward-up-list))
    (let ((pt (point))
          (new (cond ((looking-at-p (rx "(")) (cons "[" "]"))
                     ((looking-at-p (rx "[")) (cons "{" "}"))
                     ((looking-at-p (rx "{")) (cons "(" ")"))
                     (t (beep) nil))))
      (when new
        (forward-sexp)
        (backward-delete-char 1)
        (insert (cdr new))
        (goto-char pt)
        (delete-char 1)
        (insert (car new))))))

;;; Misc

(defun racket-newline-and-indent ()
  "Do `newline' and `racket-indent-line'."
  (interactive)
  (newline)
  (racket-indent-line))

(defun racket-indent-or-complete ()
  "Try `indent-for-tab-command' then `completion-at-point'.

Call `indent-for-tab-command'. If did not change the indentation
or move point to `beginning-of-line-text', and if point is
in/after at least 3 word/symbol characters, then call
`completion-at-point'."
  (interactive)
  (let ((pt (point)))
    (indent-for-tab-command)
    (when (and (equal pt (point))
               (thing-at-point-looking-at (rx (>= 3 (or (syntax word)
                                                        (syntax symbol))))))
      (completion-at-point))))

(defun racket-backward-up-list ()
  "Like `backward-up-list' but also works when point is in a string literal."
  (interactive)
  (while (in-string-p)
    (backward-char))
  (backward-up-list))

(provide 'racket-common)

;; racket-common.el ends here

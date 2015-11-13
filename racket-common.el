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

(require 'cl-lib)
(require 'thingatpt)
(require 'racket-custom)
(require 'racket-keywords-and-builtins)
(require 'racket-font-lock)
(require 'racket-indent)
(require 'racket-util)

(declare-function racket-complete-at-point "racket-complete.el" (&optional predicate))
(declare-function racket-eldoc-function    "racket-complete.el" ())

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

    ;; Whitespace (except ?\n, see below in comment section)
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{  "(}  " st)
    (modify-syntax-entry ?}  "){  " st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?'  "'   " st)
    (modify-syntax-entry ?`  "'   " st)
    (modify-syntax-entry ?,  "'   " st)
    (modify-syntax-entry ?@  "'   " st)
    (modify-syntax-entry ?\\ "\\   " st)

    ;; Comment related
    (modify-syntax-entry ?\; "<   " st) ;line comments but NOT sexp #;
    (modify-syntax-entry ?\n ">   " st)

    (modify-syntax-entry ?#  "w 14" st) ;not necessarily prefix
    (modify-syntax-entry ?|  "_ 23bn" st)

    st))

(defvar racket-mode-abbrev-table nil)
(define-abbrev-table 'racket-mode-abbrev-table ())

(defun racket-syntax-propertize-function (start end)
  (goto-char start)
  (racket--syntax-propertize-here-string end)
  (funcall
   (syntax-propertize-rules
    ;; here strings: The main responsibility here is to set the "|"
    ;; char syntax around the "body" so it's treated as a string for
    ;; indent, nav, font-lock. Think of the \n in #<<ID\n as the open
    ;; | quote and the \n in ^ID\n as the close | quote.
    ((rx "#<<" (group (+? (not (any blank ?\n)))) (group ?\n))
     (2 (racket--syntax-propertize-open-here-string
         (match-beginning 0)
         (match-string-no-properties 1)
         (match-beginning 2))))
    ((rx (syntax string-delimiter))
     (0 (ignore (racket--syntax-propertize-here-string end))))
    ;; sexp comments should LOOK like comments but NOT ACT like
    ;; comments: Give the #; itself the syntax class "prefix" [1], but
    ;; allow the following sexp to get the usual syntaxes. That way
    ;; things like indent and sexp nav work within the sexp. Only
    ;; font-lock handles the sexp specially; see racket-font-lock.el.
    ;;
    ;; [1]: Although it's tempting to use punctuation -- so things like
    ;; `backward-sexp' and `racket-send-last-sexp' ignore the #; --
    ;; that would mess up indentation of things following the sexp
    ;; comment. Instead special-case `racket-send-last-sexp'.
    ((rx "#;")
     (0 "'"))
    ;; Treat "complex" reader literals as a single sexp for nav and
    ;; indent, by marking the stuff after the # as prefix syntax.
    ;; Racket predefines reader literals like #"" #rx"" #px"" #hash()
    ;; #hasheq() #fx3(0 1 2) #s() and so on. I think these -- plus any
    ;; user defined reader extensions -- can all be covered with the
    ;; following general rx. Also it seems sufficient to look for just
    ;; the opening delimiter pair -- the ( [ { or " -- here.
    ((rx (group ?#
                (zero-or-more (or (syntax symbol)
                                  (syntax word))))
         (or ?\" ?\( ?\[ ?\{))
     (1 "'"))
    ;; Syntax quoting
    ((rx ?# (or ?` ?' ?,))
     (0 "'"))
    ;; Treat '|symbol with spaces| as word syntax
    ((rx ?' ?| (+ any) ?|)
     (0 "w"))
    ;; Treat |identifier with spaces| -- but not #|comment|# -- as word syntax
    ((rx (not (any ?#)) (group ?| (+ any) ?|) (not (any ?#)))
     (1 "w")))
   (point)
   end))

(defun racket--syntax-propertize-open-here-string (start string eol)
  "Determine the syntax of the \\n after a #<<HERE
START is the position of #<<.
STRING is the actual word used as delimiter (e.g. \"HERE\").
EOL is the position of the \\n.
Point is at the beginning of the next line.

This sets the open | syntax and sets a 'racket-here-string
property whose value is STRING. The close | syntax is set by
`racket--syntax-propertize-here-string'."
  (unless (save-excursion
            (let ((ppss (syntax-ppss start)))
              (or (nth 3 ppss) (nth 4 ppss))))
    (let ((ppss (save-excursion (syntax-ppss eol))))
      (if (nth 4 ppss)
          ;; The \n not only starts the heredoc but also closes a comment.
          ;; Let's close the comment just before the \n.
          (put-text-property (1- eol) eol 'syntax-table '(12))) ;">"
      (if (or (nth 5 ppss) (> (count-lines start eol) 1))
          ;; If we matched several lines, make sure we refontify them
          ;; together. Furthermore, if (nth 5 ppss) is non-nil (i.e.
          ;; the \n is escaped), it means the right \n is actually
          ;; further down. Don't bother fixing it now, but place a
          ;; multiline property so that when jit-lock-context-*
          ;; refontifies the rest of the buffer, it also refontifies
          ;; the current line with it.
          (put-text-property start (1+ eol) 'syntax-multiline t))
      (put-text-property eol (1+ eol) 'racket-here-string string)
      (goto-char (+ 3 start))
      (string-to-syntax "|"))))

(defun racket--syntax-propertize-here-string (end)
  "If in a here string that ends before END, add | syntax for its close."
  (let ((ppss (syntax-ppss)))
    (when (eq t (nth 3 ppss)) ;t as opposed to ?" or ?'
      (let ((key (get-text-property (nth 8 ppss) 'racket-here-string)))
        (when (and key
                   (re-search-forward (concat "^" (regexp-quote key) "\\(\n\\)")
                                      end t))
          (let ((eol (match-beginning 1)))
            (put-text-property eol (1+ eol)
                               'syntax-table
                               (string-to-syntax "|"))))))))

(defun racket--variables-for-both-modes ()
  ;;; Syntax and font-lock stuff.
  (set-syntax-table racket-mode-syntax-table)
  (setq-local syntax-propertize-function #'racket-syntax-propertize-function)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local font-lock-defaults
              (list racket-font-lock-keywords ;keywords
                    nil                       ;keywords-only?
                    nil                       ;case-fold?
                    nil                       ;syntax-alist
                    nil                       ;syntax-begin
                    ;; Additional variables:
                    (cons 'syntax-begin-function #'racket--module-level-form-start)
                    (cons 'font-lock-mark-block-function #'mark-defun)
                    (cons 'parse-sexp-lookup-properties t)
                    (cons 'font-lock-multiline t)
                    (cons 'font-lock-syntactic-face-function
                          #'racket-font-lock-syntactic-face-function)
                    (list 'font-lock-extend-region-functions
                          #'font-lock-extend-region-wholelines
                          #'racket--font-lock-extend-region)))
  ;; -----------------------------------------------------------------
  ;; Comments. Borrowed from lisp-mode
  (setq-local comment-start ";")
  (setq-local comment-add 1)            ;default to `;;' in comment-region
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning:
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  ;; Font lock mode uses this only when it knows a comment is starting:
  (setq-local font-lock-comment-start-skip ";+ *")
  (setq-local parse-sexp-ignore-comments t)
  ;; -----------------------------------------------------------------
  ;;; Misc
  (setq-local comment-column 40)
  (setq-local local-abbrev-table racket-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function #'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function #'racket-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local lisp-indent-function #'racket-indent-function)
  (racket--set-indentation)
  (setq-local indent-tabs-mode nil)
  (setq-local completion-at-point-functions (list #'racket-complete-at-point))
  (setq-local eldoc-documentation-function #'racket-eldoc-function)
  (setq-local beginning-of-defun-function #'racket--beginning-of-defun-function))


;;; Insert lambda char (like DrRacket)

(defconst racket-lambda-char (make-char 'greek-iso8859-7 107)
  "Character inserted by `racket-insert-labmda'.")

(defun racket-insert-lambda ()
  (interactive)
  (insert-char racket-lambda-char 1))
(put 'racket-insert-lambda 'delete-selection t)


;;; racket--self-insert

(defun racket--self-insert (event)
  "Simulate a `self-insert-command' of EVENT.

Using this intead of `insert' allows self-insert hooks to run,
which is important for things like `'electric-pair-mode'.

A command using this should probably set its 'delete-selection
property to t so that `delete-selection-mode' works:

  (put 'racket-command 'delete-selection t)

If necessary the value of the property can be a function, as done
by `racket--set-delete-selection-property-for-parens'."
  (let ((last-command-event event)) ;set this for hooks
    (self-insert-command (prefix-numeric-value nil))))

(defun racket--set-delete-selection-property-for-parens (command)
  "Set `delete-selection-mode' property for commands that insert parens.
Inserted text should replace the selection unless a mode like
`electric-pair-mode' is enabled."
  (put command
       'delete-selection
       (lambda ()
         (not (memq 'electric-pair-mode minor-mode-list)))))


;;; Automatically insert matching \?) \?] or \?}

(defconst racket--matching-parens
  '(( ?\( . ?\) )
    ( ?\[ . ?\] )
    ( ?\{ . ?\} )))

(defun racket-insert-closing (&optional prefix)
  "Insert a matching closing delimiter.

With a prefix, insert the typed character as-is."
  (interactive "P")
  (let* ((open-char  (and (not prefix) (racket--open-paren #'backward-up-list)))
         (close-pair (and open-char    (assq open-char racket--matching-parens)))
         (close-char (and close-pair   (cdr close-pair))))
    (racket--self-insert (or close-char last-command-event))))

(racket--set-delete-selection-property-for-parens 'racket-insert-closing)


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
                         "match-let*"
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

Returns '[' or nil."

  (and (save-excursion
         (ignore-errors
           (backward-sexp pre-backward-sexps) t))
       (save-excursion
         (ignore-errors
           (let ((pt (point)))
             (racket-backward-up-list) ;works even in strings
             (backward-sexp post-backward-sexps)
             (when (or (racket--in-string-or-comment (point) pt)
                       (looking-at-p regexp))
               ?\[))))))

(defun racket-smart-open-bracket ()
  "Automatically insert a `(` or a `[` as appropriate.

When `racket-smart-open-bracket-enable' is nil, this simply
inserts `[`. Otherwise, this behaves like the \"Automatically
adjust opening square brackets\" feature in Dr. Racket:

By default, inserts a `(`. Inserts a `[` in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To force insert `[`, use `quoted-insert': \\[quoted-insert] [.

Combined with `racket-insert-closing' this means that
you can press the unshifted `[` and `]` keys to get whatever
delimiters follow the Racket conventions for these forms. (When
`electric-pair-mode' or `paredit-mode' is active, you need not
even press `]`."
  (interactive)
  (let ((ch (or (and (not racket-smart-open-bracket-enable) ?\[)
                (cl-some (lambda (xs)
                           (apply #'racket--smart-open-bracket-helper xs))
                         racket--smart-open-bracket-data)
                (racket--open-paren #'backward-sexp)
                ?\()))
    (if (fboundp 'racket--paredit-aware-open)
        (racket--paredit-aware-open ch)
      (racket--self-insert ch))))

(racket--set-delete-selection-property-for-parens 'racket-smart-open-bracket)

(eval-after-load 'paredit
  '(progn
     (defvar paredit-mode-map nil) ;byte compiler
     (declare-function paredit-open-round  'paredit)
     (declare-function paredit-open-square 'paredit)
     (declare-function paredit-open-curly  'paredit)
     (defvar racket--paredit-original-open-bracket-binding
       (lookup-key paredit-mode-map (kbd "["))
       "The previous `paredit-mode-map' binding for [.
Rather than assuming that it's `paredit-open-square', we store
the actual value. This seems like the right thing to do in case
someone else is doing similar hackery.")

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
If not we call the function in the variable
`racket--paredit-original-open-bracket-binding'."
       (interactive)
       (if (eq major-mode 'racket-mode)
           (racket-smart-open-bracket)
         (funcall racket--paredit-original-open-bracket-binding)))

     (defun racket--paredit-aware-open (ch)
       "A paredit-aware helper for `racket-smart-open-bracket'.

When `paredit-mode' is active, use its functions (such as
`paredit-open-round') Note: This function isn't defined unless
paredit is loaded, so check for this function's existence using
`fboundp'."
       (let ((paredit-active (and (boundp 'paredit-mode) paredit-mode)))
         (cond ((not paredit-active) (racket--self-insert ch))
               ((eq ch ?\()          (paredit-open-round))
               ((eq ch ?\[)          (paredit-open-square))
               ((eq ch ?\{)          (paredit-open-curly))
               (t                    (racket--self-insert ch)))))))


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


;;; racket--beginning-of-defun

(defun racket--beginning-of-defun-function ()
  "Like `beginning-of-defun' but aware of Racket module forms."
  (let ((orig (point)))
    (pcase (racket--module-level-form-start)
      (`() (ignore-errors (backward-sexp 1)))
      (pos (goto-char pos)))
    (/= orig (point))))


;;; Misc

(defun racket-backward-up-list ()
  "Like `backward-up-list' but also works when point is in a string literal."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (nth 3 ppss)
      (goto-char (nth 8 ppss))))
  (backward-up-list 1))

(defun racket--in-string-or-comment (from to)
  "See if point is in a string or comment, without moving point."
  (save-excursion
    (let ((state (parse-partial-sexp from to)))
      (or (nth 3 state) (nth 4 state)))))

(defun racket--open-paren (back-func)
  "Use BACK-FUNC to find an opening ( [ or { if any.
BACK-FUNC should be something like #'backward-sexp or #'backward-up-list."
  (save-excursion
    (ignore-errors
      (funcall back-func)
      (let* ((str (buffer-substring-no-properties (point) (1+ (point))))
             (ch (string-to-char str)))
        (when (memq ch '(?\( ?\[ ?\{)) ch)))))


(provide 'racket-common)

;; racket-common.el ends here

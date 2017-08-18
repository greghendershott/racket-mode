;;; racket-common.el

;; Copyright (c) 2013-2016 by Greg Hendershott.
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
(require 'racket-ppss)
(require 'racket-util)

(declare-function racket-complete-at-point "racket-complete.el" (&optional predicate))

(defvar racket-mode-abbrev-table nil)
(define-abbrev-table 'racket-mode-abbrev-table ())

;;; syntax-table and syntax-propertize-function

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
    ;; the opening delimiter -- the ( [ { or " -- here.
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
    ;; Treat |identifier with spaces| -- but not #|comment|# -- as
    ;; word syntax
    ((rx (not (any ?#))
         (group ?| (+? (not (any "|\r\n"))) ?|)
         (not (any ?#)))
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
              (or (racket--ppss-string-p ppss)
                  (racket--ppss-comment-p ppss))))
    (let ((ppss (save-excursion (syntax-ppss eol))))
      (if (racket--ppss-comment-p ppss)
          ;; The \n not only starts the heredoc but also closes a comment.
          ;; Let's close the comment just before the \n.
          (put-text-property (1- eol) eol 'syntax-table '(12))) ;">"
      (if (or (racket--ppss-quote-p ppss)
              (< 1 (count-lines start eol)))
          ;; If we matched several lines, make sure we refontify them
          ;; together. Furthermore, if the \n is quoted, it means the
          ;; right \n is actually further down. Don't bother fixing it
          ;; now, but place a multiline property so that when
          ;; jit-lock-context-* refontifies the rest of the buffer, it
          ;; also refontifies the current line with it.
          (put-text-property start (1+ eol) 'syntax-multiline t))
      (put-text-property eol (1+ eol) 'racket-here-string string)
      (goto-char (+ 3 start))
      (string-to-syntax "|"))))

(defun racket--syntax-propertize-here-string (end)
  "If in a here string that ends before END, add | syntax for its close."
  (let ((ppss (syntax-ppss)))
    (when (eq (racket--ppss-string-p ppss) t) ;t as opposed to ?" or ?'
      (let ((key (get-text-property (racket--ppss-string/comment-start ppss)
                                    'racket-here-string)))
        (when (and key
                   (re-search-forward (concat "^" (regexp-quote key) "\\(\n\\)")
                                      end t))
          (let ((eol (match-beginning 1)))
            (put-text-property eol (1+ eol)
                               'syntax-table
                               (string-to-syntax "|"))))))))

;;;

(defun racket--common-variables ()
  "Set variables common to `racket-mode' and `racket-repl-mode'."
  ;;; Syntax
  (set-syntax-table racket-mode-syntax-table)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local syntax-propertize-function #'racket-syntax-propertize-function)
  (syntax-propertize (point-max)) ;for e.g. paredit: see issue #222
  ;; -----------------------------------------------------------------
  ;; Font-lock
  (setq-local font-lock-defaults
              (list racket-font-lock-keywords ;keywords
                    nil                       ;keywords-only?
                    nil                       ;case-fold?
                    nil                       ;syntax-alist
                    nil                       ;syntax-begin
                    ;; Additional variables:
                    (cons 'font-lock-mark-block-function #'mark-defun)
                    (cons 'parse-sexp-lookup-properties t)
                    (cons 'font-lock-multiline t)
                    (cons 'font-lock-syntactic-face-function
                          #'racket-font-lock-syntactic-face-function)
                    (list 'font-lock-extend-region-functions
                          #'font-lock-extend-region-wholelines
                          #'font-lock-extend-region-multiline)))
  ;; -----------------------------------------------------------------
  ;; Comments. Mostly borrowed from lisp-mode and/or scheme-mode
  (setq-local comment-start ";")
  (setq-local comment-add 1)        ;default to `;;' in comment-region
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-column 40)
  (setq-local comment-multi-line t) ;for auto-fill-mode and #||# comments
  ;; Font lock mode uses this only when it knows a comment is starting:
  (setq-local font-lock-comment-start-skip ";+ *")
  ;; -----------------------------------------------------------------
  ;; Indent
  (setq-local indent-line-function #'racket-indent-line)
  (racket--set-indentation)
  (setq-local indent-tabs-mode nil)
  ;; -----------------------------------------------------------------
  ;;; Misc
  (setq-local local-abbrev-table racket-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function #'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local completion-at-point-functions (list #'racket-complete-at-point))
  (setq-local eldoc-documentation-function nil)
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

If necessary the value of the property can be a function, for
example `racket--electric-pair-mode-not-active'."
  (let ((last-command-event event))     ;set this for hooks
    (self-insert-command (prefix-numeric-value nil))))

(defun racket--electric-pair-mode-not-active ()
  "A suitable value for the 'delete-selection property of
commands that insert parens: Inserted text should replace the
selection unless a mode like `electric-pair-mode' is enabled, in
which case the selection is to be wrapped in parens."
  (not (and (boundp 'electric-pair-mode)
            electric-pair-mode)))


;;; Automatically insert matching \?) \?] or \?}

(defconst racket--matching-parens
  '(( ?\( . ?\) )
    ( ?\[ . ?\] )
    ( ?\{ . ?\} )))

(defun racket-insert-closing (&optional prefix)
  "Insert a matching closing delimiter.

With a prefix, insert the typed character as-is.

This is handy if you're not yet using `paredit-mode',
`smartparens-mode', or simply `electric-pair-mode' added in Emacs
24.5."
  (interactive "P")
  (let* ((do-it (not (or prefix
                         (and (string= "#\\"
                                       (buffer-substring-no-properties
                                        (- (point) 2) (point) )))
                         (racket--ppss-string-p (syntax-ppss)))))
         (open-char  (and do-it        (racket--open-paren #'backward-up-list)))
         (close-pair (and open-char    (assq open-char racket--matching-parens)))
         (close-char (and close-pair   (cdr close-pair))))
    (racket--self-insert (or close-char last-command-event))))

(put 'racket-insert-closing 'delete-selection
     #'racket--electric-pair-mode-not-active)


;;; Smart open bracket

(defconst racket--smart-open-bracket-data
  (eval-when-compile
    `(;; cond-like
      (0 0 ,(rx (seq "("
                     (or "augment"
                         "augment-final"
                         "augride"
                         "cond"
                         "field"
                         "inherit"
                         "inherit-field"
                         "inherit/super"
                         "inherit/inner"
                         "init"
                         "init-field"
                         "match-lambda"
                         "match-lambda*"
                         "match-lambda**"
                         "overment"
                         "override"
                         "override-final"
                         "public"
                         "pubment"
                         "public-final"
                         "rename-inner"
                         "rename-super"
                         "super-new")
                     (or space line-end))))
      ;; case-like
      (2 0 ,(rx (seq "("
                     (or "case"
                         "new"
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
             (backward-up-list)
             (backward-sexp post-backward-sexps)
             (when (looking-at-p regexp)
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

  - `class` declaration syntax, such as `init` and `inherit`.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To force insert `[`, use `quoted-insert': \\[quoted-insert] [.

Combined with `racket-insert-closing' this means that
you can press the unshifted `[` and `]` keys to get whatever
delimiters follow the Racket conventions for these forms. (When
`electric-pair-mode' or `paredit-mode' is active, you need not
even press `]`."
  (interactive)
  (let ((ch (or (and (not racket-smart-open-bracket-enable)
                     ?\[)
                (and (save-excursion
                       (let ((pt (point)))
                         (beginning-of-defun)
                         (let ((state (parse-partial-sexp (point) pt)))
                           (or (racket--ppss-string-p state)
                               (racket--ppss-comment-p state)))))
                     ?\[)
                (cl-some (lambda (xs)
                           (apply #'racket--smart-open-bracket-helper xs))
                         racket--smart-open-bracket-data)
                (racket--open-paren #'backward-sexp)
                ?\()))
    (if (fboundp 'racket--paredit-aware-open)
        (racket--paredit-aware-open ch)
      (racket--self-insert ch))))

(put 'racket-smart-open-bracket 'delete-selection
     #'racket--electric-pair-mode-not-active)

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
       (if (racket--mode-edits-racket-p)
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

;;; paredit and reader literals

(defun racket--reader-literal-paredit-space-for-delimiter-predicate (endp delimiter)
  "`paredit-mode' shouldn't insert space beteween # and open delimiters.

Examples: #() #2() #fl() #hasheq  etc.

This function is a suitable element for the list variable
`paredit-space-for-delimiter-predicates'. "
  (if (and (racket--mode-edits-racket-p)
           (not endp))
      (not (looking-back (rx ?# (* (or (syntax word) (syntax symbol))))
                         nil))
    t))

(eval-after-load 'paredit
  '(add-hook 'paredit-space-for-delimiter-predicates
             #'racket--reader-literal-paredit-space-for-delimiter-predicate))

;;; paredit and at-expressions

(defun racket--at-expression-paredit-space-for-delimiter-predicate (endp delimiter)
  "`paredit-mode' shouldn't insert space before [ or { in Racket at-expressions.

This function is a suitable element for the list variable
`paredit-space-for-delimiter-predicates'. "
  (if (and (racket--mode-edits-racket-p)
           (not endp))
      (not (or
            ;; @foo[ @foo{
            (and (memq delimiter '(?\[ ?\{))
                 (looking-back (rx ?@ (* (or (syntax word) (syntax symbol))))
                               nil))
            ;; @foo[]{
            (and (eq delimiter ?\{)
                 (looking-back (rx ?@ (* (or (syntax word) (syntax symbol)))
                                   ?\[
                                   (* (or (syntax word) (syntax symbol)))
                                   ?\])
                               nil))))
    t))

(eval-after-load 'paredit
  '(add-hook 'paredit-space-for-delimiter-predicates
             #'racket--at-expression-paredit-space-for-delimiter-predicate))


;;; Cycle paren shapes

(defconst racket--paren-shapes
  '( (?\( ?\[ ?\] )
     (?\[ ?\{ ?\} )
     (?\{ ?\( ?\) ))
  "This is not user-configurable because we expect them have to
  have actual ?\( and ?\) char syntax.")

(defun racket-cycle-paren-shapes ()
  "Cycle the sexpr among () [] {}."
  (interactive)
  (save-excursion
    (unless (eq ?\( (char-syntax (char-after)))
      (backward-up-list))
    (pcase (assq (char-after) racket--paren-shapes)
      (`(,_ ,open ,close)
       (delete-char 1)
       (insert open)
       (backward-char 1)
       (forward-sexp 1)
       (backward-delete-char 1)
       (insert close))
      (_
       (user-error "Don't know that paren shape")))))


;;; racket--beginning-of-defun

(defun racket--beginning-of-defun-function ()
  "Like `beginning-of-defun' but aware of Racket module forms."
  (let ((orig (point)))
    (racket--escape-string-or-comment)
    (pcase (racket--module-level-form-start)
      (`() (ignore-errors (backward-sexp 1)))
      (pos (goto-char pos)))
    (/= orig (point))))

(defun racket--module-level-form-start ()
  "Start position of the module-level form point is within.

A module-level form is the outermost form not nested in a Racket
module form.

If point is not within a module-level form, returns nil.

If point is already exactly at the start of a module-level form,
-- i.e. on the opening ?\( -- returns nil.

If point is within a string or comment, returns nil.

This is NOT suitable for the variable `syntax-begin-function'
because it (i) doesn't move point, and (ii) doesn't know how to
find the start of a string or comment."
  (save-excursion
    (ignore-errors
      (let ((pos nil)
            (parse-sexp-ignore-comments t))
        (while (ignore-errors
                 (goto-char (scan-lists (point) -1 1))
                 (unless (looking-at racket-module-forms)
                   (setq pos (point)))
                 t))
        (and pos
             (or (racket--sexp-comment-start pos)
                 pos))))))

(defun racket--sexp-comment-start (pos)
  "Start pos of sexp comment (if any) immediately before POS.

Allows #; to be followed by zero or more space or newline chars."
  (save-excursion
    (goto-char pos)
    (while (memq (char-before) '(32 ?\n))
      (goto-char (1- (point))))
    (when (string= "#;" (buffer-substring-no-properties (- (point) 2) (point)))
      (- (point) 2))))


;;; Misc

(defun racket--escape-string-or-comment ()
  "If point is in a string or comment, move to its start.

Note that this can be expensive, as it uses `syntax-ppss' which
parses from the start of the buffer. Although `syntax-ppss' uses
a cache, that is invalidated after any changes to the buffer. As
a result, the worst case would be to call this function after
every character is inserted to a buffer."
  (pcase (racket--ppss-string/comment-start (syntax-ppss))
    (`() nil)
    (pos (goto-char pos))))

(defun racket-backward-up-list ()
  "Like `backward-up-list' but works when point is in a string or comment.

Typically you should not use this command in Emacs Lisp --
especially not repeatedly. Instead, initially use
`racket--escape-string-or-comment' to move to the start of a
string or comment, if any, then use normal `backward-up-list'
repeatedly."
  (interactive)
  (racket--escape-string-or-comment)
  (backward-up-list 1))

(defun racket--open-paren (back-func)
  "Use BACK-FUNC to find an opening ( [ or { if any.
BACK-FUNC should be something like #'backward-sexp or #'backward-up-list."
  (save-excursion
    (ignore-errors
      (funcall back-func)
      (let ((ch (char-after)))
        (and (eq ?\( (char-syntax ch))
             ch)))))


(provide 'racket-common)

;; racket-common.el ends here

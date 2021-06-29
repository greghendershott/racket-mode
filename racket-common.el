;;; racket-common.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2021 by Greg Hendershott.
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
(require 'racket-parens)
(require 'racket-ppss)
(require 'racket-util)

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
    ;; Character literals. See:
    ;; <https://docs.racket-lang.org/reference/reader.html#(part._parse-character)>
    ((rx (group "#\\" (or "nul" "null"
                          "backspace"
                          "tab" "vtab"
                          "newline" "linefeed"
                          "page"
                          "return"
                          "space"
                          "rubout"
                          (** 3 3 (any (?0 . ?7)))
                          (seq ?u (** 1 4 hex-digit))
                          (seq ?U (** 1 6 hex-digit))
                          anything))
         (or (not alphabetic) eol))
     (1 "w"))
    ;; Treat "complex" reader literals as a single sexp for nav and
    ;; indent, by also marking as prefix syntax the stuff after the #.
    ;; Racket predefines reader literals like #"" #rx"" #px"" #hash()
    ;; #hasheq() #fx3(0 1 2) #s() and so on. I think these -- plus any
    ;; user defined reader extensions -- can all be covered with the
    ;; following general rx. Also it seems sufficient to look for just
    ;; the opening delimiter -- the ( [ { or " -- here.
    ((rx (not (any ?|))
         (group ?#
                (?? (not (any ?|        ;not comment #362
                              ?:        ;not keyword arg #448
                              space     ;not space #546
                              ?\\))
                    (*? (or (syntax symbol) (syntax word) (syntax punctuation)))))
         (any ?\" ?\( ?\[ ?\{))
     (1 "'"))
    ;; Syntax quoting
    ((rx ?# (or ?` ?' ?,))
     (0 "'"))
    ;; Treat '|symbol with spaces| as word syntax
    ((rx ?' ?| (*? (not (any ?\" ?\r ?\n))) ?|)
     (0 "w"))
    ;; Treat |identifier with spaces| -- but not #|comment|# -- as
    ;; word syntax
    ((rx (not (any ?#))
         (group ?| (*? (not (any ?\" ?\r ?\n))) ?|)
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
  (setq-local beginning-of-defun-function #'racket--beginning-of-defun-function))


;;; Insert lambda char (like DrRacket)

(defconst racket-lambda-char (make-char 'greek-iso8859-7 107)
  "Character inserted by `racket-insert-labmda'.")

(defun racket-insert-lambda ()
  "Insert λ.

To insert Unicode symbols generally, see `racket-unicode-input-method-enable'."
  (interactive)
  (insert-char racket-lambda-char 1))
(put 'racket-insert-lambda 'delete-selection t)


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


;;; racket--what-to-run

(defun racket--what-to-run ()
  (cons (racket--buffer-file-name)
        (racket--submod-path)))

(defun racket--submod-path ()
  (and (racket--lang-p)
       (racket--modules-at-point)))

(defun racket--lang-p ()
  "Is #lang the first sexpr in the file, after an optional shebang?"
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (forward-sexp)
      (backward-sexp)
      (when (looking-at (rx "#!"))
        (forward-line)
        (forward-sexp)
        (backward-sexp))
      (looking-at (rx "#lang")))))

(defun racket--modules-at-point ()
  "List of module names that point is within, from outer to inner.
Ignores module forms nested (at any depth) in any sort of plain
or syntax quoting, because those won't be valid Racket syntax."
  (let ((xs nil))
    (condition-case ()
        (save-excursion
          (save-match-data
            (racket--escape-string-or-comment)
            (while t
              (when (racket--looking-at-module-form)
                (push (intern (match-string-no-properties 1)) xs))
              (when (racket--looking-at-quoted-form)
                (push nil xs))
              (backward-up-list))))
      (scan-error xs))
    (racket--take-while xs #'identity)))

(defun racket--looking-at-module-form ()
  "Sets match data group 1 to the module name."
  (looking-at (rx ?\(
                  (or "module" "module*" "module+")
                  (1+ " ")
                  (group (+ (or (syntax symbol)
                                (syntax word)
                                (syntax punctuation)))))))

(defun racket--looking-at-quoted-form ()
  (or (memq (char-before) '(?\' ?\` ?\,))
      (and (eq (char-before (1- (point))) ?\,)
           (eq (char-before) ?\@))
      (looking-at
       (rx ?\(
           (or "quote" "quasiquote"
               "unquote" "unquote-splicing"
               "quote-syntax"
               "syntax" "syntax/loc"
               "quasisyntax" "quasisyntax/loc"
               "unsyntax" "unsyntax-splicing")
           " "))))

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

(provide 'racket-common)

;; racket-common.el ends here

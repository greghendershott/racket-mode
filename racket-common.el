;;; racket-common.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2023 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Things used by both racket-mode and racket-repl-mode

(require 'cl-extra)
(require 'thingatpt)
(require 'tramp)
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
    ((rx "#<<" (group (+? (not (any ?\n)))) (group ?\n))
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

This sets the open | syntax and sets a \"racket-here-string\"
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

;;; Insert lambda char (like DrRacket)

(defconst racket-lambda-char (make-char 'greek-iso8859-7 107)
  "Character inserted by `racket-insert-labmda'.")

(defun racket-insert-lambda ()
  "Insert λ.

To insert Unicode symbols generally, see `racket-input-mode'."
  (interactive)
  (insert-char racket-lambda-char 1))
(put 'racket-insert-lambda 'delete-selection t)


;;; racket--beginning-of-defun

(defun racket--beginning-of-defun-function ()
  "A value for `beginning-of-defun-function'.

Aware of `racket-module-forms' and sexp comment prefixes.

Note: This is the old flavor that takes no arguments and returns
a boolean whether it moved. As a result `beginning-of-defun-raw'
when given a negative argument will use `end-of-defun-function',
which we leave at the default, i.e., `forward-sexp'. AFAIK that's
been OK, so I don't want to deal with the newer, more complicated
flavor here."
  (let ((parse-sexp-ignore-comments t)
        (orig (point)))
    (racket--escape-string-or-comment)
    ;; Try to move up to outermost form, but stopping at or before any
    ;; module form.
    (while
        (condition-case _
            (let ((prev (point)))
              (goto-char (scan-lists (point) -1 1))
              (if (looking-at racket-module-forms)
                  ;; Stop -- either directly on this module form, or,
                  ;; back down from where we just came.
                  (if (= (1+ (point)) prev)
                      nil
                    (goto-char prev)
                    nil)
                ;; Continue moving up.
                t))
          (scan-error nil)))
    ;; Unless we moved, try a simple `backward-sexp': Maybe we're
    ;; already at the module level, and just need to move to the
    ;; previous module-level item.
    (unless (/= orig (point))
      (condition-case _ (backward-sexp 1) (scan-error nil)))
    ;; When we moved, also move before any preceding "#;".
    (when (/= orig (point))
      (when-let (sexp-comment-start
                 (save-excursion
                   (while (memq (char-before) '(32 ?\n))
                     (goto-char (1- (point))))
                   (let ((beg (- (point) 2)))
                     (when (<= (point-min) beg)
                       (when (string= "#;" (buffer-substring beg (point)))
                         beg)))))
        (goto-char sexp-comment-start)))
    (/= orig (point))))

;;; racket--what-to-run

(defun racket--what-to-run-p (v)
  "Predicate for a \"what-to-run\" value.

Either nil or a list, where the first element of the list is a
file name and the remainder are `symbolp' submodule names.

Note: Because for non-tramp file names this uses `file-exist-p',
it's good to `racket--save-if-changed' first, ensuring that a
new buffer has a file on-disk."
  (pcase v
    (`() t)
    (`(,file . ,subs)
     (and (stringp file)
          (or (tramp-tramp-file-p file)
              (file-exists-p file))
          (cl-every #'symbolp subs)))
    (_ nil)))

(defvar-local racket-submodules-at-point-function nil)

(defun racket--what-to-run ()
  (cons (racket--buffer-file-name)
        (and racket-submodules-at-point-function
             (funcall racket-submodules-at-point-function))))

(defun racket-submodules-at-point-text-sexp ()
  "A value for variable `racket--submodules-at-point-function',
which is suitable for `racket-mode' and possibly for
`racket-hash-lang-mode' when the hash-lang is like lang racket."
  (let ((mods (racket--modules-at-point)))
    (if (racket--lang-p)
        mods
      (cdr mods))))

(defun racket--lang-p ()
  "Is #lang the first sexpr in the file, after an optional shebang?"
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (forward-sexp)
      (backward-sexp)
      (when (looking-at-p (rx "#!"))
        (forward-line)
        (forward-sexp)
        (backward-sexp))
      (looking-at-p (rx "#lang")))))

(defun racket--modules-at-point ()
  "List of module names that point is within, from outer to inner.
Ignores module forms nested (at any depth) in any sort of plain
or syntax quoting, because those won't be valid Racket syntax."
  (save-excursion
    (let ((xs nil))
      (condition-case _
          (progn
            (racket--escape-string-or-comment)
            (while t
              (when-let (mod-name-sym (racket--looking-at-module-form))
                (push mod-name-sym xs))
              (when (racket--looking-at-quoted-form-p)
                (push nil xs))
              (backward-up-list)))
        ((scan-error user-error) xs))
      (racket--take-while xs #'identity))))

(defun racket--looking-at-module-form ()
  "When looking at a module form, return the mod name as a symbol."
  (save-match-data
    (when (looking-at (rx ?\(
                          (or "module" "module*" "module+")
                          (1+ " ")
                          (group (+ (or (syntax symbol)
                                        (syntax word)
                                        (syntax punctuation))))))
      (intern (match-string-no-properties 1)))))

(defun racket--looking-at-quoted-form-p ()
  (or (memq (char-before) '(?\' ?\` ?\,))
      (and (eq (char-before (1- (point))) ?\,)
           (eq (char-before) ?\@))
      (looking-at-p
       (rx ?\(
           (or "quote" "quasiquote"
               "unquote" "unquote-splicing"
               "quote-syntax"
               "syntax" "syntax/loc"
               "quasisyntax" "quasisyntax/loc"
               "unsyntax" "unsyntax-splicing")
           " "))))

;;; Misc

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

(defconst racket--plain-syntax-table
  (let ((table (make-syntax-table)))
    ;; Modify entries for characters for parens, strings, and
    ;; comments, setting them to word syntax instead. (For the these
    ;; raw syntax descriptor numbers, see Emacs Lisp Info: "Syntax
    ;; Table Internals".)
    (map-char-table (lambda (key value)
                      (when (memq (car value) '(4 5 7 10 11 12))
                        (aset table key '(2))))
                    table)
    table)
  "A syntax-table that makes no assumptions that characters are
delimiters for parens, quotes, comments, etc. Just whitespace and
word syntax, so the user has /some/ basic navigation as opposed
to it being one opaque blob.")

(provide 'racket-common)

;; racket-common.el ends here

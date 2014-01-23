;;; racket-mode.el --- Major mode for Racket language.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;;; Commentary:

;; Goals:
;; - Focus on Racket (not various Schemes).
;; - Fontify all Racket keywords, builtins, and so on.
;; - Fontify variations of define for functions and variables.
;; - Indent Racket forms (even `for/fold` and `for*/fold`).
;; - Follow DrRacket concepts where applicable.
;; - Compatible with Emacs 24.2+.
;;
;; Acknowledgements:
;;
;; - Obviously the existing Emacs Scheme mode and Inferior Scheme mode.
;;
;; - The source code for Neil Van Dyke's Quack provided a model for
;;   many of the scheme-indent-function settings, smart paren closing,
;;   and pretty lambda.
;;
;; Details: https://github.com/greghendershott/racket-mode

;;; Code:

(defconst racket-mode-copyright
  "Copyright (c) 2013-2014 by Greg Hendershott. Portions Copyright (c) Free Software Foundation and Copyright (c) 2002-2012 Neil Van Dyke.")

(defconst racket-mode-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  See
http://www.gnu.org/licenses/ for details.")

(defconst racket-mode-version "0.3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Things used by both racket-mode and racket-repl-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'lisp-mode)
(require 'racket-keywords-and-builtins)

(defgroup racket nil
  "A mode for Racket"
  :prefix "racket-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/greghendershott/racket-mode")
  :link '(emacs-commentary-link :tag "Commentary" "racket-mode"))

(defun racket--variables-for-both-modes ()
  ;; Set many things explicitly. We wouldn't need to set most of these
  ;; if our editing major mode, `racket-mode`, were derived from
  ;; `scheme-mode` instead of from `prog-mode`. So why do it this way?
  ;; Because of our `racket-repl-mode`. That needs to derive from
  ;; `comint-mode`, therefore it needs to set them explicitly. Setting them
  ;; all here ensures consistency. And in that case, racket-mode need not
  ;; derive from scheme-mode, it can derive from just prog-mode.
  (set-syntax-table racket-mode-syntax-table)
  (setq-local local-abbrev-table racket-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
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
                nil                            ;keywords-omly?
                nil                            ;case-fold?
                (("+-*/.<>=!?$%_&~^:" . "w")   ;syntax-alist
                 (?#. "w 14"))
                beginning-of-defun             ;syntax-begin
                ;; Additional variables:
                (font-lock-mark-block-function . mark-defun)
                (font-lock-syntactic-face-function
                 . racket-font-lock-syntactic-face-function)
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props syntax-table))))

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

    ;; ;; Make # and | symbol constituents.
    ;; (modify-syntax-entry ?# "_ p14bn" racket-mode-syntax-table)
    ;; (modify-syntax-entry ?| "_ 23bn"  racket-mode-syntax-table)

    st))

(defvar racket-mode-abbrev-table nil)
(define-abbrev-table 'racket-mode-abbrev-table ())

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

(defconst racket-sexp-comment-syntax-table
  (let ((st (make-syntax-table racket-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defcustom racket-mode-rackjure-indent t
  "Indent {} as in Clojure"
  :type 'boolean
  :group 'racket
  :safe 'booleanp)

(defun racket-indent-function (indent-point state)
  "Racket mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `racket-indent-function'
rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation.

The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `racket-indent-function' it specifies how to indent.  The property
value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (when (not (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp))
            (goto-char calculate-lisp-indent-last-sexp)
            (beginning-of-line)
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            (open-pos (elt state 1))
            (method nil))
        (setq method (get (intern-soft function) 'racket-indent-function))
        (cond ((or
                ;; a vector literal:  #( ... )
                (and (eq (char-after (- open-pos 1)) ?\#)
                     (eq (char-after open-pos) ?\())
                ;; a quoted '( ... ) or quasiquoted `( ...) list --
                ;; but NOT syntax #'( ... )
                (and (not (eq (char-after (- open-pos 2)) ?\#))
                     (memq (char-after (- open-pos 1)) '(?\' ?\`))
                     (eq (char-after open-pos) ?\())
                ;; #lang rackjure dict literal { ... }
                (and racket-mode-rackjure-indent
                    (eq (char-after open-pos) ?\{)))
               ;; Indent all aligned with first item:
               (goto-char open-pos)
               (1+ (current-column)))
              ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent))))))

;; copied from scheme-let-indent
(defun racket--indent-let (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

(defun racket--indent-for/fold (state indent-point normal-indent)
  ;; see http://community.schemewiki.org/?emacs-indentation
  (let ((containing-sexp-start (elt state 1))
        containing-sexp-point
        containing-sexp-column
        body-indent
        clause-indent)
    ;; Move to the start of containing sexp, calculate its
    ;; indentation, store its point and move past the function symbol
    ;; so that we can use 'parse-partial-sexp'.
    ;;
    ;; 'lisp-indent-function' guarantees that there is at least one
    ;; word or symbol character following open paren of containing
    ;; sexp.
    (forward-char 1)
    (goto-char containing-sexp-start)
    (setq containing-sexp-point (point))
    (setq containing-sexp-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-sexp-column))
    (forward-char 1)    ;Move past the open paren.
    (forward-sexp 2)    ;Move to the next sexp, past its close paren
    (backward-sexp 1)   ;Move to its start paren
    (setq clause-indent (current-column))
    (forward-sexp 1)    ;Move back past close paren
    ;; Now go back to the beginning of the line holding
    ;; the indentation point. Count the sexps on the way.
    (parse-partial-sexp (point) indent-point 1 t)
    (let ((n 1))
      (while (and (< (point) indent-point)
                  (condition-case ()
                      (progn
                        (setq n (+ 1 n))
                        (forward-sexp 1)
                        (parse-partial-sexp (point) indent-point 1 t))
                    (error nil))))
      (list (cond ((= 1 n) clause-indent)
                  (t body-indent))
            containing-sexp-point))))

(defun racket--set-indentation ()
  "Set indentation for various Racket forms.
Note that defines aren't listed here because `racket-indent-function'
handles those."
  (mapc (lambda (x)
          (put (car x) 'racket-indent-function (cadr x)))
        '((begin 0)
          (begin0 1)
          (c-declare 0)
          (c-lambda 2)
          (call-with-input-file 1)
          (call-with-input-file* 1)
          (call-with-output-file 1)
          (call-with-values 1)
          (case 1)
          (case-lambda 0)
          (catch 1)
          (class 'defun)
          (class* 'defun)
          (compound-unit/sig 0)
          (delay 0)
          (def 1)                    ;cheating: not actually in Racket
          (dict-set 1)
          (dict-set* 1)
          (do 2)
          (dynamic-wind 0)
          (fn 1)                     ;cheating: not actually in Racket
          (for 1)
          (for/list 1)
          (for/vector 1)
          (for/hash 1)
          (for/hasheq 1)
          (for/hasheqv 1)
          (for/and 1)
          (for/or 1)
          (for/lists 1)
          (for/first 1)
          (for/last 1)
          (for/fold racket--indent-for/fold)
          (for/flvector 1)
          (for/set 1)
          (for/sum 1)
          (for* 1)
          (for*/list 1)
          (for*/vector 1)
          (for*/hash 1)
          (for*/hasheq 1)
          (for*/hasheqv 1)
          (for*/and 1)
          (for*/or 1)
          (for*/lists 1)
          (for*/first 1)
          (for*/last 1)
          (for*/fold racket--indent-for/fold)
          (for*/flvector 1)
          (for*/set 1)
          (for*/sum 1)
          (instantiate 2)
          (interface 1)
          (lambda 1)
          (lambda/kw 1)
          (let racket--indent-let)
          (let* 1)
          (letrec 1)
          (let-values 1)
          (let*-values 1)
          (let+ 1)
          (let-values 1)
          (let-syntax 1)
          (letrec-syntax 1)
          (let/ec 1)
          (match 1)
          (match* 1)
          (match-let 1)
          (match-let* 1)
          (mixin 2)
          (module 2)
          (module+ 1)
          (module* 2)
          (opt-lambda 1)
          (parameterize 1)
          (parameterize-break 1)
          (parameterize* 1)
          (quasisyntax/loc 1)
          (receive 2)
          (send* 1)
          (sigaction 1)
          (splicing-syntax-parameterize 1)
          (struct 1)
          (syntax-case 2)
          (syntax-rules 1)
          (syntax-parse 1)
          (syntax-parameterize 1)
          (syntax/loc 1)
          (syntax-parse 1)
          (unit 'defun)
          (unit/sig 2)
          (unless 1)
          (when 1)
          (while 1)
          (with-handlers 1)
          (with-method 1)
          (with-float 1)
          (with-fixed 1)
          (with-input-from-file 1)
          (with-input-from-port 1)
          (with-syntax 0)
          (with-syntax* 0)
          (with-output-to-file 1)
          (with-output-to-port 1)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock

(defconst racket-keyword-argument-face 'racket-keyword-argument-face)
(defface racket-keyword-argument-face
  '((((background dark))
     (:foreground "IndianRed"))
    (((background light))
     (:foreground "Red3")))
  "Face for #:keyword arguments."
  :group 'racket)

(defconst racket-selfeval-face 'racket-selfeval-face)
(defface racket-selfeval-face
  '((t
     (:foreground "SeaGreen")))
  "Face for self-evaluating expressions."
  :group 'racket)

(defconst racket-paren-face 'racket-paren-face)
(defface racket-paren-face
  (let ((fg (face-foreground 'default)))
    `((t (:foreground ,fg))))
  "Face for parentheses () [] {}."
  :group 'racket)

(defconst racket-font-lock-keywords
  (eval-when-compile
    `(
      ;; #lang
      ("\\(\\(#lang\\)[ ]+\\([^\n]+\\)\\)"
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t))

      ;; keyword argument
      ("#:[^ )]+"                 . racket-keyword-argument-face)

      ;; symbol
      ("'\\sw+"                   . racket-selfeval-face)
      ("'|\\(\\sw\\| \\)+|"       . racket-selfeval-face)

      ;; #rx #px
      ("\\(#[pr]x\\)\"" (1 racket-selfeval-face))

      ;; literal char
      ("\\_<#\\\\\\([][-`~!@#$%&*()_+=^{}\;:'\"<>,.?/|\\\\]\\|\\sw+\\>\\)"
       . racket-selfeval-face)

      ;; paren
      ("[][(){}]"                 . racket-paren-face)

      (,(regexp-opt racket-builtins 'symbols) . font-lock-builtin-face)
      (,(regexp-opt racket-keywords 'symbols) . font-lock-keyword-face)

      ;; define -- vars
      ("(\\(define[ ]+\\([^ (]+\\)\\)" 2 font-lock-variable-name-face)
      ("(\\(def[ ]+\\([^ (]+\\)\\)"    2 font-lock-variable-name-face)
      ("(\\(define-values[ ]*(\\([^(]+\\))\\)" 2 font-lock-variable-name-face)

      ;; define -- functions
      ("(\\(define[^ ]*[ ]*([ ]*\\([^ ]+\\)\\)" 2 font-lock-function-name-face)
      ("(\\(defn-?[^ ]*[ ]*([ ]*\\([^ ]+\\)\\)" 2 font-lock-function-name-face)

      ;; module and module*
      ("(\\(module[*]?\\)[ ]+\\([^ ]+\\)[ ]+\\([^ ]+\\)"
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t)
       (3 font-lock-variable-name-face nil t))
      ;; module+
      ("(\\(module[+]\\)[ ]+\\([^ ]+\\)"
       (1 font-lock-keyword-face nil t)
       (2 font-lock-function-name-face nil t))

      ;; pretty lambda
      ("[[(]\\(case-\\|match-\\|opt-\\)?\\(lambda\\)\\>"
       2
       (progn (compose-region (match-beginning 2)
                              (match-end       2)
                              racket-lambda-char)
              nil))

      ;; #t #f
      (,(regexp-opt '("#t" "#f") 'symbols) . racket-selfeval-face)

      ;; From my Pygments lexer (maybe can simplify b/c unlike Pygments
      ;; we're not lexing for types like int vs. float).
      ;;
      ;; Numeric literals including Racket reader hash prefixes.
      ;; Caveat: None of these regexps attempt to exclude identifiers
      ;; that start with a number, such as a variable named
      ;; "100-Continue".

      ;; #d (or no hash prefix)
      ("\\_<\\(#d\\)?[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<\\(#d\\)?[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<\\(#d\\)?[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<\\(#d\\)?[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#d[^ ]*\\_>". font-lock-warning-face)

      ;; #x
      ("\\_<#x[-+]?[0-9a-fA-F]+\\.[0-9a-fA-F]+\\_>" . racket-selfeval-face)
      ;; the exponent variation (e.g. #x1e1) is N/A
      ("\\_<#x[-+]?[0-9a-fA-F]+/[0-9a-fA-F]+\\_>" . racket-selfeval-face)
      ("\\_<#x[-+]?[0-9a-fA-F]+\\_>" . racket-selfeval-face)
      ("\\_<#x[^ ]*\\_>" . font-lock-warning-face)

      ;; #b
      ("\\_<#b[-+]?[01]+\\.[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[01]+e[-+]?[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[-+]?[01]/[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[-+]?[01]+\\_>" . racket-selfeval-face)
      ("\\_<#b[^ ]*\\_>" . font-lock-warnng-face)

      ;; #e
      ("\\_<#e[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#e[^ ]*\\_>" . font-lock-warning-face)

      ;; #i
      ("\\_<#i[-+]?[0-9]+\\.[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[0-9]+e[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[-+]?[0-9]+/[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[-+]?[0-9]+\\_>" . racket-selfeval-face)
      ("\\_<#i[^ ]*\\_>" . font-lock-warning-face)

      ;; #o
      ("\\_<#o[-+]?[0-7]+\\.[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[0-7]+e[-+]?[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[-+]?[0-7]+/[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[-+]?[0-7]+\\_>" . racket-selfeval-face)
      ("\\_<#o[^ ]*\\_>" . font-lock-warning-face)

      ;; numeric constants
      (,(regexp-opt '("+inf.0" "-inf.0" "+nan.0") 'symbols)
       . racket-selfeval-face)

      ))
    "Font lock keywords for Racket mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert lambda char (like DrRacket)

(defconst racket-lambda-char (make-char 'greek-iso8859-7 107)
  "Character inserted by `racket-insert-labmda'.")

(defun racket-insert-lambda ()
  (interactive)
  (insert-char racket-lambda-char 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically insert matching \?) \?] or \?}

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
                (delete-backward-char 1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Racket mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)
(require 'hideshow)

(defun racket-run ()
  "Save and evaluate the buffer in REPL, like DrRacket's Run."
  (interactive)
  (save-buffer)
  (racket--eval (format ",run \"%s\"\n" (buffer-file-name))))

(defun racket-racket ()
  "Do `racket <file>` in *shell* buffer."
  (interactive)
  (racket--shell (concat racket-program
                         " "
                         (shell-quote-argument (buffer-file-name)))))

(defun racket-test ()
  "Do (require (submod \".\" test)) in *racket* buffer."
  (interactive)
  (racket-run) ;start fresh, so (require) will have an effect
  (racket--eval
   "(begin
 (displayln \"Running tests...\")
 (require (submod \".\" test))
 (flush-output (current-output-port)))\n"))

(defun racket-raco-test ()
  "Do `raco test -x <file>` in *shell* buffer.
To run <file>'s `test` submodule."
  (interactive)
  (racket--shell (concat (expand-file-name
                          "raco"
                          (file-name-directory racket-program))
                         " test -x "
                         (shell-quote-argument (buffer-file-name)))))

(defun racket-find-definition (&optional prefix)
  "Find definition of symbol at point. (EXPERIMENTAL)

Only works if you've Run the buffer so that its namespace is active."
  (interactive "P")
  (let ((sym (symbol-at-point-or-prompt prefix "Find definition of: ")))
    (when sym
      (racket--eval (format ",def %s\n\n" sym)))))

(defun racket-help (&optional prefix)
  "Find something in Racket's help."
  (interactive "P")
  (let ((sym (symbol-at-point-or-prompt prefix "Racket help for: ")))
    (when sym
      (racket--eval (format ",doc %s\n\n" sym)))))

(defun symbol-at-point-or-prompt (prefix prompt)
  "Helper for functions that want symbol-at-point, or, to prompt
when there is no symbol-at-point or prefix is true."
  (let ((sap (symbol-at-point)))
    (if (or prefix (not sap))
        (read-from-minibuffer prompt (if sap (symbol-name sap) ""))
      sap)))

;;----------------------------------------------------------------------------

(defun racket--eval (str)
  (racket-repl)
  (racket--repl-forget-errors)
  (comint-send-string (racket--get-repl-buffer-process) str)
  (racket--repl-show-and-move-to-end))

(defun racket--shell (cmd)
  (let ((w (selected-window)))
    (save-buffer)
    (let ((rw (get-buffer-window "*shell*")))
      (if rw
          (select-window rw)
        (other-window -1)))
    (message (concat cmd "..."))
    (shell)
    (racket-pop-to-buffer-same-window "*shell*")
    (comint-send-string "*shell*" (concat cmd "\n"))
    (select-window w)
    (sit-for 3)
    (message nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cr = cr + indent

(defun racket-cr ()
  (interactive)
  (newline)
  (lisp-indent-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code folding

;;;###autoload
(add-to-list 'hs-special-modes-alist
             '(racket-mode "(" ")" ";" nil nil))

(defun racket--for-all-tests (verb f)
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^(module[+*]? test" (point-max) t)
        (funcall f)
        (incf n)
        (goto-char (match-end 0)))
      (message "%s %d test submodules" verb n))))

(defun racket-fold-all-tests ()
  (interactive)
  (racket--for-all-tests "Folded" 'hs-hide-block))

(defun racket-unfold-all-tests ()
  (interactive)
  (racket--for-all-tests "Unfolded" 'hs-show-block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar racket-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (mapc (lambda (x)
            (define-key map (kbd (car x)) (cadr x)))
          '(("<f5>"      racket-run)
            ("M-C-<f5>"  racket-racket)
            ("C-<f5>"    racket-test)
            ("M-C-x"     racket-send-definition)
            ("C-x C-e"   racket-send-last-sexp)
            ("C-c C-r"   racket-send-region)
            ("C-c C-e x" racket-expand-definition)
            ("C-c C-e e" racket-expand-last-sexp)
            ("C-c C-e r" racket-expand-region)
            ("C-c C-e a" racket-expand-again)
            ("RET"       racket-cr)
            (")"         racket-insert-closing-paren)
            ("]"         racket-insert-closing-bracket)
            ("}"         racket-insert-closing-brace)
            ("M-C-y"     racket-insert-lambda)
            ("<f1>"      racket-help)
            ("C-c C-h"   racket-help)
            ("C-c C-d"   racket-find-definition)
            ("C-c C-f"   racket-fold-all-tests)
            ("C-c C-U"   racket-unfold-all-tests)))
    map)
  "Keymap for Racket mode. Inherits from `lisp-mode-shared-map'.")

(easy-menu-define racket-mode-menu racket-mode-map
  "Menu for Racket mode."
  '("Racket"
    ("Run"
     ["in REPL" racket-run]
     ["via `racket`" racket-racket])
    ("Tests"
     ["in REPL" racket-test]
     ["via `raco test`" racket-raco-test]
     "---"
     ["Fold All" racket-fold-all-tests]
     ["Unfold All" racket-unfold-all-tests])
    ("Eval"
     ["Region" racket-send-region :active (region-active-p)]
     ["Definition" racket-send-definition]
     ["Last S-Expression" racket-send-last-sexp])
    ("Macro Expand"
     ["Region" racket-expand-region  :active (region-active-p)]
     ["Definition" racket-expand-definition]
     ["Last S-Expression" racket-expand-last-sexp]
     "---"
     ["Again" racket-expand-again])
    "---"
    ["Comment" comment-dwim]
    ["Insert λ" racket-insert-lambda]
    ["Indent Region" indent-region]
    "---"
    ["Find Definition" racket-find-definition]
    ["Help" racket-help]
    ["Next Error or Link" next-error]
    ["Previous Error" previous-error]
    "---"
    ["Customize..." customize-mode]))

;;;###autoload
(define-derived-mode racket-mode prog-mode
  "Racket"
  "Major mode for editing Racket.
\\{racket-mode-map}"
  (racket--variables-for-both-modes)
  (hs-minor-mode t))

;;;###autoload
(setq auto-mode-alist
      (append '(("\\.rkt\\'" . racket-mode)
                ("\\.rktd\\'" . racket-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Racket REPL mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comint)
(require 'compile)

(defconst racket--repl-buffer-name/raw
  "Racket REPL"
  "The base buffer name, NOT surrounded in *stars*")
(defconst racket--repl-buffer-name
  (concat "*" racket--repl-buffer-name/raw "*")
  "The actual buffer name as created by comint-mode")
(defun racket--get-repl-buffer-process ()
  (get-buffer-process racket--repl-buffer-name))

(defvar racket-repl-mode-map
  (make-sparse-keymap)
  "Keymap for Racket REPL mode.")
(define-key racket-repl-mode-map "\r"  'racket-repl-cr)
(define-key racket-repl-mode-map ")"   'racket-insert-closing-paren)
(define-key racket-repl-mode-map "]"   'racket-insert-closing-bracket)
(define-key racket-repl-mode-map "}"   'racket-insert-closing-brace)
(define-key racket-repl-mode-map "\M-\C-y"    'racket-insert-lambda)
(define-key racket-repl-mode-map (kbd "<f1>") 'racket-help)
(define-key racket-repl-mode-map "\C-c\C-h"   'racket-help)
(define-key racket-repl-mode-map "\C-c\C-d"   'racket-find-definition)

(defcustom racket-repl-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'racket)

(defun racket-input-filter (str)
  "Don't save anything matching `racket-repl-filter-regexp'."
  (not (string-match racket-repl-filter-regexp str)))

(defun racket-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;; I don't want comint-mode clobbering our font-lock with
;; comint-highlight-input face. Changing that face not to be bold
;; isn't enough: The original fonts would still get clobbered, just
;; clobbered with non-bold default face.
;;
;; So far, the least-pukey way I can figure out how to do this is to
;; copy-pasta comint-send-input and modify that one tiny bit.
;; Blech. If anyone reading this knows a better way, please let me
;; know!
(defun racket--comint-send-input (&optional no-newline artificial)
  "Like comint-send-input but does NOT change the input text to use the comint-highlight-input face."
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send (end-of-line))
                               (buffer-substring pmark (point)))
                      (let ((copy (funcall comint-get-old-input)))
                        (goto-char pmark)
                        (insert copy)
                        copy)))
             (input (if (not (eq comint-input-autoexpand 'input))
                        ;; Just whatever's already there.
                        intxt
                      ;; Expand and leave it visible in buffer.
                      (comint-replace-by-expanded-history t pmark)
                      (buffer-substring pmark (point))))
             (history (if (not (eq comint-input-autoexpand 'history))
                          input
                        ;; This is messy 'cos ultimately the original
                        ;; functions used do insertion, rather than return
                        ;; strings.  We have to expand, then insert back.
                        (comint-replace-by-expanded-history t pmark)
                        (let ((copy (buffer-substring pmark (point)))
                              (start (point)))
                          (insert input)
                          (delete-region pmark start)
                          copy))))

        (unless no-newline
          (insert ?\n))

        (comint-add-to-input-history history)

        (run-hook-with-args 'comint-input-filter-functions
                            (if no-newline input
                              (concat input "\n")))

        (let ((beg (marker-position pmark))
              (end (if no-newline (point) (1- (point))))
              (inhibit-modification-hooks t))
          (when (> end beg)
            ;; ;; The following is what we don't want -- what we're
            ;; ;; copying the entire rest of this function, just to
            ;; ;; omit. Sigh.
            ;; (add-text-properties beg end
            ;;                      '(front-sticky t
            ;;                        font-lock-face comint-highlight-input))
            (unless comint-use-prompt-regexp
              ;; Give old user input a field property of `input', to
              ;; distinguish it from both process output and unsent
              ;; input.  The terminating newline is put into a special
              ;; `boundary' field to make cursor movement between input
              ;; and output fields smoother.
              (add-text-properties
               beg end
               '(mouse-face highlight
                 help-echo "mouse-2: insert after prompt as new input"))))
          (unless (or no-newline comint-use-prompt-regexp)
            ;; Cover the terminating newline
            (add-text-properties end (1+ end)
                                 '(rear-nonsticky t
                                   field boundary
                                   inhibit-line-move-field-capture t))))

        (comint-snapshot-last-prompt)

        (setq comint-save-input-ring-index comint-input-ring-index)
        (setq comint-input-ring-index nil)
        ;; Update the markers before we send the input
        ;; in case we get output amidst sending the input.
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker (process-mark proc) (point))
        ;; clear the "accumulation" marker
        (set-marker comint-accum-marker nil)
        (let ((comint-input-sender-no-newline no-newline))
          (funcall comint-input-sender proc input))

        ;; Optionally delete echoed input (after checking it).
        (when (and comint-process-echoes (not artificial))
          (let ((echo-len (- comint-last-input-end
                             comint-last-input-start)))
            ;; Wait for all input to be echoed:
            (while (and (> (+ comint-last-input-end echo-len)
                           (point-max))
                        (accept-process-output proc)
                        (zerop
                         (compare-buffer-substrings
                          nil comint-last-input-start
                          (- (point-max) echo-len)
                          ;; Above difference is equivalent to
                          ;; (+ comint-last-input-start
                          ;;    (- (point-max) comint-last-input-end))
                          nil comint-last-input-end (point-max)))))
            (if (and
                 (<= (+ comint-last-input-end echo-len)
                     (point-max))
                 (zerop
                  (compare-buffer-substrings
                   nil comint-last-input-start comint-last-input-end
                   nil comint-last-input-end
                   (+ comint-last-input-end echo-len))))
                ;; Certain parts of the text to be deleted may have
                ;; been mistaken for prompts.  We have to prevent
                ;; problems when `comint-prompt-read-only' is non-nil.
                (let ((inhibit-read-only t))
                  (delete-region comint-last-input-end
                                 (+ comint-last-input-end echo-len))
                  (when comint-prompt-read-only
                    (save-excursion
                      (goto-char comint-last-input-end)
                      (comint-update-fence)))))))

        ;; This used to call comint-output-filter-functions,
        ;; but that scrolled the buffer in undesirable ways.
        (run-hook-with-args 'comint-output-filter-functions "")))))

(defun racket-repl-cr ()
  "If complete sexpr, do comint cr. Else just newline and indent."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
        (user-error "Current buffer has no process")
      (condition-case nil
          (progn
            (save-excursion
              (goto-char (process-mark proc))
              (forward-list)) ;will error unless complete sexpr
            (racket--comint-send-input))
        (error (racket-cr))))))

(defvar racket-sandbox-rkt
  (let ((elisp-dir
         (file-name-directory (or load-file-name (buffer-file-name)))))
    (expand-file-name "sandbox.rkt" elisp-dir))
  "Path to sandbox.rkt")

(defcustom racket-program "racket"
  "/path/to/racket."
  :type '(file :must-match t)
  :group 'racket)

;;;###autoload
(defun racket-repl ()
  "Run a Racket REPL in a comint buffer.
Runs the hook `racket-repl-mode-hook' \(after the `comint-mode-hook'
is run)."
  (interactive)
  (let ((original-window (selected-window)))
    ;; If REPL process already visible in a window, use that window.
    (let ((rw (get-buffer-window racket--repl-buffer-name)))
      (if rw
          (select-window rw)
        (other-window 1)))
    (unless (comint-check-proc racket--repl-buffer-name)
      (set-buffer (make-comint racket--repl-buffer-name/raw ;w/o *stars*
                               racket-program
                               nil
                               racket-sandbox-rkt))
      (racket-repl-mode))
    (select-window original-window)))

(defun racket-send-region (start end)
  "Send the current region to the Racket REPL."
  (interactive "r")
  (racket--repl-forget-errors)
  (comint-send-region (racket--get-repl-buffer-process) start end)
  (comint-send-string (racket--get-repl-buffer-process) "\n")
  (racket--repl-show-and-move-to-end))

(defun racket-send-definition ()
  "Send the current definition to the Racket REPL."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (racket--repl-forget-errors)
     (racket-send-region (point) end)
     (racket--repl-show-and-move-to-end))))

(defun racket-send-last-sexp ()
  "Send the previous sexp to the Racket REPL."
  (interactive)
  (racket--repl-forget-errors)
  (racket-send-region (save-excursion (backward-sexp) (point)) (point))
  (racket--repl-show-and-move-to-end))

(defun racket-expand-region (start end &optional prefix)
  "Like `racket-send-region', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "rP")
  (racket--repl-send-expand-command prefix)
  (racket-send-region start end))

(defun racket-expand-definition (&optional prefix)
  "Like `racket-send-definition', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-definition))

(defun racket-expand-last-sexp (&optional prefix)
  "Like `racket-send-last-sexp', but macro expand.

With C-u prefix, expands fully.

Otherwise, expands once. You may use `racket-expand-again'."
  (interactive "P")
  (racket--repl-send-expand-command prefix)
  (racket-send-last-sexp))

(defun racket--repl-send-expand-command (prefix)
  (comint-send-string (racket--get-repl-buffer-process)
                      (if prefix ",exp!" ",exp ")))

(defun racket-expand-again ()
  "Macro expand again the previous expansion done by one of:
- `racket-expand-region'
- `racket-expand-definition'
- `racket-expand-last-sexp'
- `racket-expand-again'"
  (interactive)
  (comint-send-string (racket--get-repl-buffer-process) ",exp+\n"))

(defun racket--repl-forget-errors ()
  "Forget existing compilation mode errors in the REPL.
Although they remain clickable, `next-error' and `previous-error'
will ignore them."
  (with-current-buffer racket--repl-buffer-name
    (compilation-forget-errors)))

(defun racket--repl-show-and-move-to-end ()
  "Make the Racket REPL visible, and move point to end.
Keep original window selected."
  (let ((w (selected-window)))
    (pop-to-buffer racket--repl-buffer-name t)
    (select-window (get-buffer-window racket--repl-buffer-name))
    (with-current-buffer racket--repl-buffer-name
      (goto-char (point-max)))
    (select-window w)))

(define-derived-mode racket-repl-mode comint-mode "Racket-REPL"
  "Major mode for interacting with Racket process.
\\{racket-repl-mode-map}"
  (racket--variables-for-both-modes)
  ;; (setq-local comint-prompt-regexp "^[^>\n]*>+ *")
  ;; (setq-local comint-use-prompt-regexp t)
  ;; (setq-local comint-prompt-read-only t)
  (setq-local mode-line-process nil)
  (setq-local comint-input-filter (function racket-input-filter))
  (compilation-setup t)
  (setq-local
   compilation-error-regexp-alist
   '(("^;?[ ]*\\([^ :]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)   ;errs, defns
     ("^;?[ ]*at:[ ]+\\([^ :]+\\):\\([0-9]+\\)[.]\\([0-9]+\\)$" 1 2 3) ;contract
     ("#<path:\\([^>]+\\)> \\([0-9]+\\) \\([0-9]+\\)" 1 2 3)   ;rackunit
     ("#<path:\\([^>]+\\)>" 1 nil nil 0)                       ;path struct
     ;; profile. similar to errs,defns but at EOL and not starting ellided name
     ("[ ]+\\([^. ]+[^ :]+\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3 0)))
  (setq-local comint-get-old-input (function racket-get-old-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs version compatibility
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In case not Emacs 24.1+, define equivalent of its
;; `pop-to-buffer-same-window'.
(defun racket-pop-to-buffer-same-window
  (&optional buffer-or-name norecord label)
  "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
  (if (fboundp 'pop-to-buffer-same-window)
      (funcall
       'pop-to-buffer-same-window buffer-or-name norecord)
    (funcall 'switch-to-buffer buffer-or-name norecord)))

;; In case not Emacs 24.3+, define equivalent of its `setq-local'.
(eval-and-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      `(set (make-local-variable ',var) ,val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'racket-mode)

;;; racket-mode.el ends here

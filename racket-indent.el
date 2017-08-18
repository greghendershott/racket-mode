;;; racket-indent.el  -*- lexical: t; -*-

;; Copyright (c) 2013-2017 by Greg Hendershott.
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
(require 'racket-util)
(require 'racket-custom)
(require 'racket-ppss)

;; The two top-level commands we care about are:
;;   1. `prog-indent-sexp' C-M-q
;;   2. `indent-region' C-M-\
;;
;; 1. `prog-indent-sexp' thinly wraps `indent-region'.
;;
;; 2. `indent-region' calls `indent-according-to-mode', which in turn
;; calls the mode-specific `indent-line-function'. In lisp-mode that's
;; `lisp-indent-line', which in turn calls `calculate-lisp-indent'.
;; That in turn calls the mode-specific `indent-function'; in
;; lisp-mode that's `lisp-indent-function'.
;;
;; However `calculate-lisp-indent' is complicated and doesn't always
;; behave the way we want. So we use a simplified version of that
;; (`racket--calculate-indent') in our `indent-line-function',
;; `racket-indent-line'. That just directly calls
;; `racket-indent-function'.

;; Having said all that, we still have the matter of `paredit-mode'.
;; It directly calls `lisp-indent-line' instead of `indent-function'.
;; And, it directly calls `indent-sexp' instead of `prog-indent-sep'.
;; Therefore it gets `lisp-mode' indent, not ours. To address this,
;; advise those two functions to do the right thing when one of our
;; major modes is active.
(defun racket--lisp-indent-line-advice (orig &rest args)
  "When `racket--mode-edits-racket-p' instead use `racket-indent-line'."
  (apply (if (racket--mode-edits-racket-p) #'racket-indent-line orig)
         args))
(defun racket--indent-sexp-advice (orig &rest args)
  "When `racket--mode-edits-racket-p' instead use `prog-indent-sexp'."
  (apply (if (racket--mode-edits-racket-p) #'prog-indent-sexp orig)
         args))
;; I don't want to muck with the old `defadvice' for this. Instead use
;; `advice-add' in Emacs 24.4+. Although we still support Emacs 24.3,
;; not sure how much longer; I'm OK having it silently not work.
(when (fboundp 'advice-add)
  (advice-add 'lisp-indent-line :around #'racket--lisp-indent-line-advice)
  (advice-add 'indent-sexp :around #'racket--indent-sexp-advice))

(defun racket-indent-line (&optional whole-exp)
  "Indent current line as Racket code.

This behaves like `lisp-indent-line', except that whole-line
comments are treated the same regardless of whether they start
with single or double semicolons.

- Automatically indents forms that start with `begin` in the usual
  way that `begin` is indented.

- Automatically indents forms that start with `def` or `with-` in the
  usual way that `define` is indented.

- Has rules for many specific standard Racket forms.

To extend, use your Emacs init file to

    (put SYMBOL 'racket-indent-function INDENT)

where `SYMBOL` is the name of the Racket form (e.g. `'test-case`)
and `INDENT` is an integer or the symbol `'defun`. When `INDENT`
is an integer, the meaning is the same as for
`lisp-indent-function` and `scheme-indent-function`: Indent the
first `n` arguments specially and then indent any further
arguments like a body.

For example in your `.emacs` file you could use:

    (put 'test-case 'racket-indent-function 1)

to change the indent of `test-case` from this:

    (test-case foo
               blah
               blah)

to this:

    (test-case foo
      blah
      blah)

If `racket-indent-function` has no property for a symbol,
`scheme-indent-function` is also considered (although the with-x
indents defined by `scheme-mode` are ignored). This is only to
help people who may have extensive `scheme-indent-function`
settings, particularly in the form of file or dir local
variables. Otherwise prefer `racket-indent-function`."
  (interactive)
  (pcase (racket--calculate-indent)
    (`()  nil)
    ;; When point is within the leading whitespace, move it past the
    ;; new indentation whitespace. Otherwise preserve its position
    ;; relative to the original text.
    (amount (let ((pos (- (point-max) (point)))
                  (beg (progn (beginning-of-line) (point))))
              (skip-chars-forward " \t")
              (unless (= amount (current-column))
                (delete-region beg (point))
                (indent-to amount))
              (when (< (point) (- (point-max) pos))
                (goto-char (- (point-max) pos)))))))

(defun racket--calculate-indent ()
  "Return appropriate indentation for current line as Lisp code.

In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

This is `calculate-lisp-indent' distilled to what we actually
need."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          (state        nil))
      (racket--plain-beginning-of-defun)
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      (let ((strp (racket--ppss-string-p state))
            (last (racket--ppss-last-sexp state))
            (cont (racket--ppss-containing-sexp state)))
        (cond
         (strp                  nil)
         ((and state last cont) (racket-indent-function indent-point state))
         (cont                  (goto-char (1+ cont)) (current-column))
         (t                     (current-column)))))))

(defun racket--plain-beginning-of-defun ()
  "Like default/plain `beginning-of-function'.
Our `racket--beginning-of-defun-function' is aware of module
forms and tailored to using C-M-a to navigate interactively. But
it is too slow to be used here -- especially in \"degenerate\"
cases like a 3000 line file consisting of one big `module` or
`library` sexpr."
  (when (re-search-backward (rx bol (syntax open-parenthesis))
                            nil
                            'move)
    (goto-char (1- (match-end 0)))))

(defun racket-indent-function (indent-point state)
  "Called by `racket--calculate-indent' to get indent column.

INDENT-POINT is the position at which the line being indented begins.
STATE is the `parse-partial-sexp' state for that position.

There is special handling for:
  - forms that begin with a #:keyword (as found in contracts)
  - forms like #hasheq()
  - data sequences when `racket-indent-sequence-depth' is > 0
  - {} forms when `racket-indent-curly-as-sequence' is not nil

See `racket-indent-line' for more information about users setting
the `racket-indent-function` property."
  (goto-char (racket--ppss-containing-sexp state))
  (let ((body-indent (+ (current-column) lisp-body-indent)))
    (forward-char 1)
    (if (or (racket--hash-literal-or-keyword-p)
            (racket--data-sequence-p))
        (progn (backward-prefix-chars) (current-column))
      (let* ((head   (buffer-substring (point) (progn (forward-sexp 1) (point))))
             (method (racket--get-indent-function-method head)))
        (cond ((integerp method)
               (racket--indent-special-form method indent-point state))
              ((eq method 'defun)
               body-indent)
              (method
               (funcall method indent-point state))
              ((string-match (rx bos (or "def" "with-")) head)
               body-indent) ;just like 'defun
              ((string-match (rx bos "begin") head)
               (racket--indent-special-form 0 indent-point state))
              (t
               (racket--normal-indent indent-point state)))))))

(defun racket--hash-literal-or-keyword-p ()
  "Looking at things like #fl() #hash() or #:keyword ?
The last occurs in Racket contract forms, e.g. (->* () (#:kw kw)).
Returns nil for #% identifiers like #%app."
  (looking-at (rx ?\# (or ?\:
                          (not (any ?\%))))))

(defun racket--data-sequence-p ()
  "Looking at \"data\" sequences where we align under head item?

These sequences include '() `() #() -- and {} when
`racket-indent-curly-as-sequence' is t -- but never #'() #`() ,()
,@().

To handle nested items, we search `backward-up-list' up to
`racket-indent-sequence-depth' times."
  (and (< 0 racket-indent-sequence-depth)
       (save-excursion
         (ignore-errors
           (let ((answer 'unknown)
                 (depth racket-indent-sequence-depth))
             (while (and (eq answer 'unknown)
                         (< 0 depth))
               (backward-up-list)
               (cl-decf depth)
               (cond ((or
                       ;; a quoted '( ) or quasiquoted `( ) list --
                       ;; but NOT syntax #'( ) or quasisyntax #`( )
                       (and (memq (char-before (point)) '(?\' ?\`))
                            (eq (char-after (point)) ?\()
                            (not (eq (char-before (1- (point))) ?#)))
                       ;; a vector literal: #( )
                       (and (eq (char-before (point)) ?#)
                            (eq (char-after  (point)) ?\())
                       ;; { }
                       (and racket-indent-curly-as-sequence
                            (eq (char-after (point)) ?{)))
                      (setq answer t))
                     (;; unquote or unquote-splicing
                      (and (or (eq (char-before (point)) ?,)
                               (and (eq (char-before (1- (point))) ?,)
                                    (eq (char-before (point))      ?@)))
                           (eq (char-after (point)) ?\())
                      (setq answer nil))))
             (eq answer t))))))

(defun racket--normal-indent (indent-point state)
  ;; Credit: Substantially borrowed from clojure-mode
  (goto-char (racket--ppss-last-sexp state))
  (backward-prefix-chars)
  (let ((last-sexp nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match (rx (not blank))
                               (buffer-substring (line-beginning-position)
                                                 (point)))
            (setq last-sexp (prog1 (point)
                              (forward-sexp -1))))
          t)
        ;; Here we've found an arg before the arg we're indenting
        ;; which is at the start of a line.
        (current-column)
      ;; Here we've reached the start of the enclosing sexp (point is
      ;; now at the function name), so the behavior depends on whether
      ;; there's also an argument on this line.
      (when (and last-sexp
                 (< last-sexp (line-end-position)))
        ;; There's an arg after the function name, so align with it.
        (goto-char last-sexp))
      (current-column))))

(defun racket--indent-special-form (method indent-point state)
  "METHOD must be a nonnegative integer -- the number of
  \"special\" args that get extra indent when not on the first
  line. Any additinonl args get normal indent."
  ;; Credit: Substantially borrowed from clojure-mode
  (let ((containing-column (save-excursion
                             (goto-char (racket--ppss-containing-sexp state))
                             (current-column)))
        (pos -1))
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (forward-sexp 1)
          (cl-incf pos))
      ;; If indent-point is _after_ the last sexp in the current sexp,
      ;; we detect that by catching the `scan-error'. In that case, we
      ;; should return the indentation as if there were an extra sexp
      ;; at point.
      (scan-error (cl-incf pos)))
    (cond ((= method pos)               ;first non-distinguished arg
           (+ containing-column lisp-body-indent))
          ((< method pos)               ;more non-distinguished args
           (racket--normal-indent indent-point state))
          (t                            ;distinguished args
           (+ containing-column (* 2 lisp-body-indent))))))

(defun racket--conditional-indent (indent-point state looking-at-regexp true false)
  (skip-chars-forward " \t")
  (let ((n (if (looking-at looking-at-regexp) true false)))
    (racket--indent-special-form n indent-point state)))

(defconst racket--identifier-regexp
  (rx (or (syntax symbol) (syntax word) (syntax punctuation)))
  "A regexp matching valid Racket identifiers.")

(defun racket--indent-maybe-named-let (indent-point state)
  "Indent a let form, handling named let (let <id> <bindings> <expr> ...)"
  (racket--conditional-indent indent-point state
                              racket--identifier-regexp
                              2 1))

(defun racket--indent-for (indent-point state)
  "Indent function for all for/ and for*/ forms EXCEPT
for/fold and for*/fold.

Checks for either of:
  - maybe-type-ann e.g. (for/list : T ([x xs]) x)
  - for/vector optional length, (for/vector #:length ([x xs]) x)"
  (racket--conditional-indent indent-point state
                              (rx (or ?\: ?\#))
                              3 1))

(defun racket--indent-for/fold (indent-point state)
  "Indent function for for/fold and for*/fold."
  ;; check for maybe-type-ann e.g. (for/fold : T ([n 0]) ([x xs]) x)
  (skip-chars-forward " \t\n")
  (if (looking-at ":")
      (racket--indent-special-form 4 indent-point state)
    (racket--indent-for/fold-untyped indent-point state)))

(defun racket--indent-for/fold-untyped (indent-point state)
  (let* ((containing-sexp-start  (racket--ppss-containing-sexp state))
         (_                      (goto-char containing-sexp-start))
         (containing-sexp-column (current-column))
         (containing-sexp-line   (line-number-at-pos))
         (body-indent            (+ containing-sexp-column lisp-body-indent))
         (clause-indent          nil))
    ;; Move to the open paren of the first, accumulator sexp
    (forward-char 1)    ;past the open paren
    (forward-sexp 2)    ;to the next sexp, past its close paren
    (backward-sexp 1)   ;back to its open paren
    ;; If the first, accumulator sexp is not on the same line as
    ;; `for/fold`, then this is simply specform 2.
    (if (/= (line-number-at-pos) containing-sexp-line) ;expensive?
        (racket--indent-special-form 2 indent-point state)
      (setq clause-indent (current-column))
      (forward-sexp 1)    ;past close paren
      ;; Now go back to the beginning of the line holding
      ;; the indentation point. Count the sexps on the way.
      (parse-partial-sexp (point) indent-point 1 t)
      (let ((n 1))
        (while (and (< (point) indent-point)
                    (ignore-errors
                      (cl-incf n)
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))))
        (if (= 1 n) clause-indent body-indent)))))

(defun racket--get-indent-function-method (head)
  "Get property of racket- or scheme-indent-function.

Ignores certain with-xxx indents defined by scheme-mode --
because we automatically indent with- forms just like def forms.
However if a _user_ has defined their own legacy scheme-mode
indents for _other_ with- forms, those _will_ be used. We only
ignore a short list defined by scheme-mode itself."
  (let ((sym (intern-soft head)))
    (or (get sym 'racket-indent-function)
        (and (not (memq sym '(call-with-values
                              with-mode
                              with-input-from-file
                              with-input-from-port
                              with-output-to-file
                              with-output-to-port
                              with-input-from-string
                              with-output-to-string
                              with-values)))
             (get sym 'scheme-indent-function)))))

(defun racket--set-indentation ()
  "Set indentation for various Racket forms.

Note that `beg*`, `def*` and `with-*` aren't listed here because
`racket-indent-function' handles those.

Note that indentation is set for the symbol alone, and also with
a : suffix for legacy Typed Racket. For example both `let` and
`let:`. Although this is overzealous in the sense that Typed
Racket does not define its own variant of all of these, it
doesn't hurt to do so."
  (mapc (lambda (x)
          (put (car x) 'racket-indent-function (cadr x))
          (let ((typed (intern (format "%s:" (car x)))))
            (put typed 'racket-indent-function (cadr x))))
        '(;; begin* forms default to 0 unless otherwise specified here
          (begin0 1)
          (c-declare 0)
          (c-lambda 2)
          (call-with-input-file defun)
          (call-with-input-file* defun)
          (call-with-output-file defun)
          (call-with-output-file* defun)
          (case 1)
          (case-lambda 0)
          (catch 1)
          (class defun)
          (class* defun)
          (compound-unit/sig 0)
          (cond 0)
          ;; def* forms default to 'defun unless otherwise specified here
          (delay 0)
          (do 2)
          (dynamic-wind 0)
          (fn 1) ;alias for lambda (although not officially in Racket)
          (for 1)
          (for/list racket--indent-for)
          (for/vector racket--indent-for)
          (for/hash racket--indent-for)
          (for/hasheq racket--indent-for)
          (for/hasheqv racket--indent-for)
          (for/and racket--indent-for)
          (for/or racket--indent-for)
          (for/lists racket--indent-for/fold)
          (for/first racket--indent-for)
          (for/last racket--indent-for)
          (for/fold racket--indent-for/fold)
          (for/flvector racket--indent-for)
          (for/set racket--indent-for)
          (for/seteq racket--indent-for)
          (for/seteqv racket--indent-for)
          (for/sum racket--indent-for)
          (for/product racket--indent-for)
          (for* 1)
          (for*/list racket--indent-for)
          (for*/vector racket--indent-for)
          (for*/hash racket--indent-for)
          (for*/hasheq racket--indent-for)
          (for*/hasheqv racket--indent-for)
          (for*/and racket--indent-for)
          (for*/or racket--indent-for)
          (for*/lists racket--indent-for/fold)
          (for*/first racket--indent-for)
          (for*/last racket--indent-for)
          (for*/fold racket--indent-for/fold)
          (for*/flvector racket--indent-for)
          (for*/set racket--indent-for)
          (for*/seteq racket--indent-for)
          (for*/seteqv racket--indent-for)
          (for*/sum racket--indent-for)
          (for*/product racket--indent-for)
          (instantiate 2)
          (interface 1)
          (λ 1)
          (lambda 1)
          (lambda/kw 1)
          (let racket--indent-maybe-named-let)
          (let* 1)
          (letrec 1)
          (letrec-values 1)
          (let-values 1)
          (let*-values 1)
          (let+ 1)
          (let-syntax 1)
          (let-syntaxes 1)
          (letrec-syntax 1)
          (letrec-syntaxes 1)
          (letrec-syntaxes+values racket--indent-for/fold-untyped)
          (local 1)
          (let/cc 1)
          (let/ec 1)
          (match 1)
          (match* 1)
          (match-define defun)
          (match-lambda 0)
          (match-lambda* 0)
          (match-let 1)
          (match-let* 1)
          (match-let*-values 1)
          (match-let-values 1)
          (match-letrec 1)
          (match-letrec-values 1)
          (match/values 1)
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
          (require/typed 1)
          (require/typed/provide 1)
          (send* 1)
          (shared 1)
          (sigaction 1)
          (splicing-let 1)
          (splicing-letrec 1)
          (splicing-let-values 1)
          (splicing-letrec-values 1)
          (splicing-let-syntax 1)
          (splicing-letrec-syntax 1)
          (splicing-let-syntaxes 1)
          (splicing-letrec-syntaxes 1)
          (splicing-letrec-syntaxes+values racket--indent-for/fold-untyped)
          (splicing-local 1)
          (splicing-syntax-parameterize 1)
          (struct defun)
          (syntax-case 2)
          (syntax-case* 3)
          (syntax-rules 1)
          (syntax-id-rules 1)
          (syntax-parse 1)
          (syntax-parser 0)
          (syntax-parameterize 1)
          (syntax/loc 1)
          (syntax-parse 1)
          (test-begin 0)
          (test-case 1)
          (unit defun)
          (unit/sig 2)
          (unless 1)
          (when 1)
          (while 1)
          ;; with- forms default to 1 unless otherwise specified here
          )))

(provide 'racket-indent)

;; racket-indent.el ends here

;;; racket-indent.el -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'subr-x)
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

(defun racket-indent-line (&optional _whole-exp)
  "Indent current line as Racket code.

Normally you don't invoke this command directly. Instead, because
it is used as the value for the variable `indent-line-function'
in `racket-mode' and `racket-repl-mode' buffers, it is used
automatically when you press keys like RET or TAB. However you
might refer to it when configuring custom indentation, explained
below.

Following the tradition of `lisp-mode' and `scheme-mode', the
primary way to determine the indentation of a form is to look for
a rule stored as a `racket-indent-function' property.

To extend, use your Emacs init file to

#+BEGIN_SRC emacs-lisp
    (put SYMBOL \\='racket-indent-function INDENT)
#+END_SRC

SYMBOL is the name of the Racket form like \"test-case\" and
INDENT is an integer or the symbol \"defun\". When INDENT is an
integer, the meaning is the same as for lisp-indent-function and
scheme-indent-function: Indent the first INDENT arguments
specially and indent any further arguments like a body. (The
number may be negative; see discussion below.)

For example:

#+BEGIN_SRC emacs-lisp
    (put \\='test-case \\='racket-indent-function 1)
#+END_SRC

This will change the indent of `test-case` from this:

#+BEGIN_SRC racket
    (test-case foo
               blah
               blah)
#+END_SRC

to this:

#+BEGIN_SRC racket
    (test-case foo
      blah
      blah)
#+END_SRC

For backward compatibility, if `racket-indent-function' has no
property for a symbol, a scheme-indent-function property is also
considered, although the \"with-\" indents defined by scheme-mode
are ignored. This is only to help people who may have extensive
scheme-indent-function settings, particularly in the form of file
or dir local variables. Otherwise prefer putting properties on
`racket-indent-function'.

If no explicit rules match, regular expressions are used for a
couple special cases:

- Forms that start with \"begin\" indent like \"begin\".

- Forms that start with \"def\" or \"with-\" indent like
  \"define\".

On the one hand this is convenient when you create your own
\"DRY\" macros; they will indent as expected without you needing
to make custom indent rules. On the other hand there can be false
matches; for example a function or form named \"defer\" will
indent like \"define\". This is a known drawback and is unlikely
to be fixed unless/until Racket macros someday support a protocol
to communicate how they should be indented.

There is also automatic handling for:

- Forms that begin with a #:keyword (as found in contracts)

- Literal forms like #hasheq()

- Quoted forms when the variable `racket-indent-sequence-depth'
  is > 0.

- {} forms when the variable `racket-indent-curly-as-sequence' is
  not nil.

Finally and otherwise, a form will be indented as if it were a
procedure application.

--- --- ---

Note: Racket Mode extends the traditional Emacs lisp indent spec
to allow a /negative/ integer, which means that all distinguished
forms should align with the first one. This style originated with
\"for/fold\", which has two distinguished forms. Traditionally
those would indent like this:

#+BEGIN_SRC racket
    (for/fold ([x xs])
        ([y ys])            ; twice body indent
      body)
#+END_SRC

However the popularly desired indent is:

#+BEGIN_SRC racket
    (for/fold ([x xs])
              ([y ys])      ; same as first distingushed form
      body)
#+END_SRC

This idea extends to optional distinguished forms, such as Typed
Racket annotation \"prefixes\" in \"for/fold\", \"for/x\", and
even \"let\" forms:

#+BEGIN_SRC racket
    (for/fold : Type
              ([x xs])
              ([y ys])      ; same as first distingushed form
      body)
#+END_SRC
"
  (interactive)
  (when-let (amount (racket--calculate-indent))
    ;; When point is within the leading whitespace, move it past the
    ;; new indentation whitespace. Otherwise preserve its position
    ;; relative to the original text.
    (let ((pos (- (point-max) (point)))
          (beg (progn (beginning-of-line) (point))))
      (skip-chars-forward " \t")
      (unless (= amount (current-column))
        (delete-region beg (point))
        (indent-to amount))
      (when (< (point) (- (point-max) pos))
        (goto-char (- (point-max) pos))))))

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
      (racket--escape-string-or-comment)
      (condition-case _ (backward-up-list 1) (scan-error nil))
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
            (racket--data-sequence-p)
            (racket--all-hyphens-p))
        (progn (backward-prefix-chars) (current-column))
      (let* ((head   (buffer-substring (point) (progn (forward-sexp 1) (point))))
             (method (racket--get-indent-function-method head)))
        (cond ((integerp method)
               (racket--indent-special-form method indent-point state))
              ((eq method 'defun)
               (racket--indent-defun indent-point body-indent))
              (method
               (funcall method indent-point state))
              ((string-match-p (rx bos (or "def" "with-")) head)
               (racket--indent-defun indent-point body-indent)) ;like 'defun
              ((string-match-p (rx bos "begin") head)
               (racket--indent-special-form 0 indent-point state))
              ((string-match-p (rx bos (or "for/" "for*/")) head)
               (racket--indent-for indent-point state))
              (t
               (racket--normal-indent indent-point state)))))))

(defun racket--indent-defun (indent-point body-indent)
  (save-excursion
    (goto-char indent-point)
    ;; When a line starts with ":", indent with previous sexp if that
    ;; is a list. Handles a Typed Racket result type on its own line
    ;; after list of formal parameters. (Although the following test
    ;; matches ":" elsewhere, the start of the previous list sexp is
    ;; the same as body-indent -- what we'd do anyway.)
    (or (and (looking-at-p "[ ]*:")
             (ignore-errors
               (backward-sexp 1)
               (and (eq ?\( (char-syntax (char-after)))
                    (current-column))))
        body-indent)))

(defun racket--hash-literal-or-keyword-p ()
  "Looking at things like #fl() #hash() or #:keyword ?
The last occurs in Racket contract forms, e.g. (->* () (#:kw kw)).
Returns nil for #% identifiers like #%app."
  (looking-at-p (rx ?\# (or ?\:
                            (not (any ?\%))))))

(defun racket--all-hyphens-p ()
  "Magic for redex like what DrRacket does."
  (looking-at-p (rx (>= 3 ?-) (and (not (syntax word))
                                   (not (syntax symbol))
                                   (not (syntax punctuation))))))

(defun racket--data-sequence-p ()
  "Looking at \"data\" sequences where we align under head item?

These sequences include \\='() \\=`() #() -- and {} when
`racket-indent-curly-as-sequence' is t -- but never #\\='()
#\\=`() ,() ,@().

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

(defun racket--normal-indent (_indent-point state)
  ;; Credit: Substantially borrowed from clojure-mode
  (goto-char (racket--ppss-last-sexp state))
  (backward-prefix-chars)
  (let ((last-sexp nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match-p (rx (not blank))
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
  "Indent a special form starting with METHOD distinguished forms.

METHOD must be an integer, the absolute value of which is the
number of distinguished forms. When a distinguished form is on
its own line (not on the first line) it gets special indent:

- When METHOD is positive: Twice `lisp-body-indent',
  which is the \"classic\" lisp behavior.

- When METHOD is negative: Same as first distinguished form.

Any additional, non-distinguished forms get normal indent."
  ;; Credit: Substantially borrowed from clojure-mode --- although the
  ;; concept of the "negative" number of distinguished forms is ours,
  ;; introduced to handle some Racket forms like for/fold and the
  ;; optional annotations of Typed Racket's let.
  (let ((distinguished (abs method))
        (containing-column (save-excursion
                             (goto-char (racket--ppss-containing-sexp state))
                             (current-column)))
        (first-form-column (save-excursion
                             (skip-chars-forward " \t\n")
                             (current-column)))
        (count -1))
    (condition-case _
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (forward-sexp 1)
          (cl-incf count))
      ;; If indent-point is _after_ the last sexp in the current sexp,
      ;; we detect that by catching the `scan-error'. In that case, we
      ;; should return the indentation as if there were an extra sexp
      ;; at point.
      (scan-error (cl-incf count)))
    (cond ((= distinguished count)      ;first non-distinguished form
           (+ containing-column lisp-body-indent))
          ((< distinguished count)      ;other non-distinguished form
           (racket--normal-indent indent-point state))
          (t                            ;distinguished form
           (if (<= 0 method)
               (+ containing-column (* 2 lisp-body-indent))
             (if (zerop count)          ;this _is_ the first form
                 (+ containing-column (* 2 lisp-body-indent))
               first-form-column))))))

(defun racket--indent-let (indent-point state)
  "Indent a let form.

We handle plain and named let, as well as the grammar for Typed
Racket let."
  (skip-chars-forward " \t")
  (let ((distinguished-forms
         (if (looking-at-p (rx (or "#:forall" "#:∀") (any " \t")))
             -3
           (if (looking-at-p (rx (syntax open-parenthesis)))
               1
             (save-excursion
               (forward-sexp 1)
               (skip-chars-forward " \t\n")
               (if (looking-at-p (rx ?: (any " \t")))
                   -4
                 2))))))
    (racket--indent-special-form distinguished-forms
                                 indent-point
                                 state)))

(defun racket--indent-for (indent-point state)
  "All for/ and for*/ forms except for/fold and for*/fold.

Checks for either of:
  - maybe-type-ann e.g. (for/list : T ([x xs]) x)
  - for/vector optional length, (for/vector #:length ([x xs]) x)"
  (skip-chars-forward " \t\n")
  (racket--indent-special-form (if (looking-at-p (rx (or ?\: ?\#))) -3 -1)
                               indent-point
                               state))

(defun racket--indent-for/fold (indent-point state)
  "Indent function for for/fold and for*/fold.

Checks for maybe-type-ann e.g. (for/fold : T ([x xs]) ([y ys]) x) "
  ;; check for maybe-type-ann e.g. (for/fold : T ([n 0]) ([x xs]) x)
  (skip-chars-forward " \t\n")
  (racket--indent-special-form (if (looking-at-p (rx ?\:)) -4 -2)
                               indent-point
                               state))

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

;; Set 'racket-indent-line property value on symbols corresponding to
;; various Racket syntax.
;;
;; Note that `racket-indent-function' handles some forms -- e.g.
;; `begin*`, `def*` `for*`, `with-*` -- with regexp matches for
;; anything not explicitly listed here.
(dolist (spec
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
           ;; Note: Things matching (rx bos (or "for/" "for*/")) default to
           ;; racket--indent-for unless otherwise specified here.
           (for racket--indent-for) ;so the rx can match more strictly
           (for/lists racket--indent-for/fold)
           (for/fold racket--indent-for/fold)
           (for/foldr racket--indent-for/fold)
           (for* racket--indent-for) ;so the rx can match more strictly
           (for*/lists racket--indent-for/fold)
           (for*/fold racket--indent-for/fold)
           (for*/foldr racket--indent-for/fold)
           (instantiate 2)
           (interface 1)
           (λ defun)
           (lambda defun)
           (lambda/kw defun)
           (let racket--indent-let)
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
           (letrec-syntaxes+values -2)
           (local 1)
           (let/cc defun)
           (let/ec defun)
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
           (place 1)
           (place/context 1)
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
           (splicing-letrec-syntaxes+values -2)
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
           ))
  ;; Set property for the plain symbol and also set for the symbol
  ;; with a : suffix. The latter is for legacy Typed Racket (e.g. both
  ;; `let` and `let:`). Although Typed Racket doesn't define such a
  ;; variant for all of these, it's harmless to set the property.
  (pcase-let* ((`(,plain-sym ,val) spec)
               (typed-sym (intern (format "%s:" plain-sym))))
    (dolist (sym (list plain-sym typed-sym))
      (put sym 'racket-indent-function val))))

(defun racket--escape-string-or-comment ()
  "If point is in a string or comment, move to its start.

Note that this can be expensive, as it uses `syntax-ppss' which
parses from the start of the buffer. Although `syntax-ppss' uses
a cache, that is invalidated after any changes to the buffer. As
a result, the worst case would be to call this function after
every character is inserted to a buffer."
  (when-let (pos (racket--ppss-string/comment-start (syntax-ppss)))
    (goto-char pos)))

(provide 'racket-indent)

;; racket-indent.el ends here

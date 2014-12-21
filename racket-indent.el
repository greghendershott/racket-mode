;;; racket-indent.el

;; Copyright (c) 2013-2014 by Greg Hendershott.
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

(defcustom racket-mode-rackjure-indent t
  "Indent {} for #lang rackjure dictionaries?"
  :tag "{} indentation style"
  :type 'boolean
  :group 'racket
  :safe 'booleanp)

(defvar calculate-lisp-indent-last-sexp)

;; Copied from lisp-mode but heavily modified
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
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
             (open-pos (elt state 1))
             (method (or (get (intern-soft function) 'racket-indent-function)
                         (get (intern-soft function) 'scheme-indent-function))))
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
                        (>= (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((and (null method)
                    (> (length function) 5)
                    (string-match "\\`with-" function))
               (lisp-indent-specform 1 state
                                     indent-point normal-indent))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

(defun racket--conditional-indent (state indent-point normal-indent
                                   looking-at-regexp true false)
  (skip-chars-forward " \t")
  (let ((n (if (looking-at looking-at-regexp) true false)))
    (lisp-indent-specform n state indent-point normal-indent)))

(defun racket--indent-let (state indent-point normal-indent)
  ;; check for named let
  (racket--conditional-indent state indent-point normal-indent
                              "[-a-zA-Z0-9+*/?!@$%^&_:~]" 2 1))

(defun racket--indent-for (state indent-point normal-indent)
  "Indent function for all for/ and for*/ forms EXCEPT
for/fold and for*/fold."
  ;; check for either of:
  ;; - maybe-type-ann e.g. (for/list : T ([x xs]) x)
  ;; - for/vector optional length, (for/vector #:length ([x xs]) x)
  (racket--conditional-indent state indent-point normal-indent
                              "[:#]" 3 1))

(defun racket--indent-for/fold (state indent-point normal-indent)
  "Indent function for for/fold and for*/fold."
  ;; check for maybe-type-ann e.g. (for/fold : T ([n 0]) ([x xs]) x)
  (skip-chars-forward " \t")
  (if (looking-at ":")
      (lisp-indent-specform 4 state indent-point normal-indent)
    (racket--indent-for/fold-untyped state indent-point normal-indent)))

(defun racket--indent-for/fold-untyped (state indent-point normal-indent)
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

Note that `def*` and `with-*` aren't listed here because
`racket-indent-function' handles those.

Note that indentation is set for the symbol alone, and also
with : appended, for Typed Racket. For example both `let` and
`let:`. Although this is overzealous in the sense that Typed
Racket does not define its own variant of all of these, it
doesn't hurt to do so."
  (mapc (lambda (x)
          (put (car x) 'racket-indent-function (cadr x))
          (let ((typed (intern (format "%s:" (car x)))))
            (put typed 'racket-indent-function (cadr x))))
        '((begin 0)
          (begin-for-syntax 0)
          (begin0 1)
          (c-declare 0)
          (c-lambda 2)
          (call-with-input-file 1)
          (call-with-input-file* 1)
          (call-with-semaphore 1)
          (call-with-output-file 1)
          (call-with-values 1)
          (case 1)
          (case-lambda 0)
          (catch 1)
          (class defun)
          (class* defun)
          (compound-unit/sig 0)
          (delay 0)
          (def 1)                    ;cheating: not actually in Racket
          (dict-set 1)
          (dict-set* 1)
          (do 2)
          (dynamic-wind 0)
          (fn 1)                     ;cheating: not actually in Racket
          (for 1)
          (for/list racket--indent-for)
          (for/vector racket--indent-for)
          (for/hash racket--indent-for)
          (for/hasheq racket--indent-for)
          (for/hasheqv racket--indent-for)
          (for/and racket--indent-for)
          (for/or racket--indent-for)
          (for/lists racket--indent-for)
          (for/first racket--indent-for)
          (for/last racket--indent-for)
          (for/fold racket--indent-for/fold)
          (for/flvector racket--indent-for)
          (for/set racket--indent-for)
          (for/sum racket--indent-for)
          (for* 1)
          (for*/list racket--indent-for)
          (for*/vector racket--indent-for)
          (for*/hash racket--indent-for)
          (for*/hasheq racket--indent-for)
          (for*/hasheqv racket--indent-for)
          (for*/and racket--indent-for)
          (for*/or racket--indent-for)
          (for*/lists racket--indent-for)
          (for*/first racket--indent-for)
          (for*/last racket--indent-for)
          (for*/fold racket--indent-for/fold)
          (for*/flvector racket--indent-for)
          (for*/set racket--indent-for)
          (for*/sum racket--indent-for)
          (instantiate 2)
          (interface 1)
          (λ 1)
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
          (require/typed 1)
          (send* 1)
          (sigaction 1)
          (splicing-syntax-parameterize 1)
          (struct 1)
          (syntax-case 2)
          (syntax-case* 3)
          (syntax-rules 1)
          (syntax-parse 1)
          (syntax-parser 0)
          (syntax-parameterize 1)
          (syntax/loc 1)
          (syntax-parse 1)
          (unit defun)
          (unit/sig 2)
          (unless 1)
          (when 1)
          (while 1)
          ;; `with-` forms given 1 automatically by our indent function
          )))

(defun racket-indent-line (&optional whole-exp)
  "Indent current line as Racket code.

This behaves like `lisp-indent-line', except that whole-line
comments are treated the same regardless of whether they start
with single or double semicolons."
  (interactive)
  (let ((indent (calculate-lisp-indent))
	(pos (- (point-max) (point)))
	(beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
	;; Don't alter indentation of a ;;; comment line
	;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
	(goto-char (- (point-max) pos))
      (when (listp indent)
        (setq indent (car indent)))
      (unless (zerop (- indent (current-column)))
        (delete-region beg (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(provide 'racket-indent)

;; racket-indent.el ends here

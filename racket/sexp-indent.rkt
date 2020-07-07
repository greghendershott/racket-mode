#lang racket/base

;; WIP example of an indenter for sexp langs, implemented on a
;; token-map.

(require racket/match
         "token-map.rkt"
         "util.rkt")

(provide indent-amount)

;; This assumes that the token-map has been updated through at least
;; the `indent-pos` -- but need not be further -- to reflect user
;; edits at the time they want to indent.
#;
(define (indent tm indent-pos)
  (define new-amount (indent-amount tm indent-pos))
  (define bol (beg-of-line tm indent-pos))
  (define cur (or (forward-whitespace tm bol) bol))
  ;;(log-racket-mode-debug "~v" (list new-amount bol cur))
  (define old-amount (- cur bol))
  (define diff (- new-amount old-amount))
  (unless (zero? diff)
    (update tm bol old-amount (make-string new-amount #\space)))
  diff)

;; token-map? positive-integer? -> nonnegative-integer?
(define (indent-amount tm indent-pos)
  (match (backward-up tm (beg-of-line tm indent-pos))
    [(? number? open-pos)
     (define id-pos (forward-whitespace tm (add1 open-pos)))
     (define id-name (token-text tm id-pos))
     (log-racket-mode-debug "indent-amount id-name is ~v at ~v" id-name id-pos)
     (match (hash-ref ht-methods
                      id-name
                      (λ ()
                        (match id-name
                          [(regexp "^def|with-") 'define]
                          [(regexp "^begin")     'begin]
                          [_                     #f])))
       [(? exact-nonnegative-integer? n)
        (special-form tm open-pos id-pos indent-pos n)]
       ['begin
        (special-form tm open-pos id-pos indent-pos 0)]
       ['define
        (define containing-column (- open-pos (beg-of-line tm open-pos)))
        (+ containing-column 2)]
       [(? procedure? proc)
        (proc tm open-pos id-pos indent-pos)]
       [_
        (default-amount tm id-pos)])]
    [#f
     (log-racket-mode-debug "indent-amount no containing sexp found")
     0]))

(define (special-form tm open-pos id-pos indent-pos special-args)
  ;; Up to special-args get extra indent, the remainder get normal
  ;; indent.
  (define containing-column (- open-pos (beg-of-line tm open-pos)))
  (define args
    (let loop ([arg-end (forward-sexp tm (forward-sexp tm id-pos))]
               [count 0])
      (log-racket-mode-debug "special-form ~v" (list (token-text tm (sub1 arg-end)) arg-end id-pos))
      (if (<= arg-end indent-pos)
          (match (forward-sexp tm arg-end)
            [(? integer? n) (loop n (add1 count))]
            [#f (add1 count)])
          count)))
  ;;(println (list special-args args))
  (+ containing-column
     (if (< args special-args) 4 2)))

(define (indent-for tm open-pos id-pos indent-pos)
  ;; TODO
  0)

(define (indent-for/fold tm open-pos id-pos indent-pos)
  ;; TODO
  0)

(define (indent-for/fold-untyped tm open-pos id-pos indent-pos)
  ;; TODO
  0)

(define (indent-maybe-named-let tm open-pos id-pos indent-pos)
  ;; TODO
  0)

(define (default-amount tm 1st-sexp)
  (define bol (beg-of-line tm 1st-sexp))
  (define eol (end-of-line tm 1st-sexp))
  (define 2nd-sexp (beg-of-next-sexp tm 1st-sexp))
  (if (and 2nd-sexp (< 2nd-sexp eol)) ;2nd on same line as 1st?
      (- 2nd-sexp bol)
      (- 1st-sexp bol)))

(define (beg-of-next-sexp tm pos)
  (backward-sexp tm
                 (forward-sexp tm
                               (forward-sexp tm pos))))

#;(require racket/trace)
#;
(trace indent-amount
       special-form
       default-amount
       beg-of-next-sexp)

(module+ test
  (require rackunit
           racket/pretty)
  (define str "#lang racket\n(foo\n  bar\nbaz)\n(foo bar baz\nbap)\n(define (f x)\nx)\n(begin0\n42\n1\n2)")
  ;;           1234567890123 45678 901234 56789 012345678 901234567 89012345678901 234 56789012 345 67 89
  ;;                    1           2           3          4          5         6           7
  (define tm (create str))
  (let* ()
    (check-equal? (indent-amount tm  1) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 14) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 15) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 22) 1
                  "bar should indent with foo")
    (check-equal? (indent-amount tm 25) 1
                  "baz should indent with bar (assumes bar not yet re-indented)")
    (check-equal? (indent-amount tm 29) 1
                  "close paren after baz is on same line as it, same result")
    (check-equal? (indent-amount tm 30) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 31) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 43) 5
                  "bap should indent with the 2nd sexp on the same line i.e. bar")
    (check-equal? (indent-amount tm 62) 2
                  "define body should indent 2")
    (check-equal? (indent-amount tm 73) 4
                  "begin0 result should indent 4")
    (check-equal? (indent-amount tm 76) 2
                  "begin0 other should indent 2")
    (check-equal? (indent-amount tm 78) 2
                  "begin0 other should indent 2"))
  (let* ([str "#lang racket\n(print 12"]
         ;;    1234567890123 4567890123
         ;;             1           2
         [tm (create str)])
    (update tm 23 0 "\n")
    (check-equal? (indent-amount tm  1) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 22) 0
                  "not within any sexpr, should indent 0")))

(module+ example-5
  (define str "#lang racket\n123\n(print 123)\n")
  ;;           1234567890123 4567 890123456789 0
  ;;                    1           2          3
  (define tm (create str))
  tm
  (indent-amount tm 18)
  (update tm 28 0 "\n")
  tm
  (indent-amount tm 29))

;;; Hardwired macro indents

;; This is analogous to the Emacs Lisp code. Instead of being
;; hardwired, we'd like to get this indent information from the module
;; that exports the macro.

(define ht-methods
  (hash "begin0" 1
        ;; begin* forms default to 0 unless otherwise specified here
        "begin0" 1
        "c-declare" 0
        "c-lambda" 2
        "call-with-input-file" 'define
        "call-with-input-file*" 'define
        "call-with-output-file" 'define
        "call-with-output-file*" 'define
        "case" 1
        "case-lambda" 0
        "catch" 1
        "class" 'define
        "class*" 'define
        "compound-unit/sig" 0
        "cond" 0
        ;; def* forms default to 'define unless otherwise specified here
        "delay" 0
        "do" 2
        "dynamic-wind" 0
        "fn" 1 ;alias for lambda (although not officially in Racket)
        ;; for/ and for*/ forms default to racket--indent-for unless
        ;; otherwise specified here
        "for" 1
        "for/list" indent-for
        "for/lists" indent-for/fold
        "for/fold" indent-for/fold
        "for*" 1
        "for*/lists" indent-for/fold
        "for*/fold" indent-for/fold
        "instantiate" 2
        "interface" 1
        "λ" 1
        "lambda" 1
        "lambda/kw" 1
        "let" indent-maybe-named-let
        "let*" 1
        "letrec" 1
        "letrec-values" 1
        "let-values" 1
        "let*-values" 1
        "let+" 1
        "let-syntax" 1
        "let-syntaxes" 1
        "letrec-syntax" 1
        "letrec-syntaxes" 1
        "letrec-syntaxes+values" indent-for/fold-untyped
        "local" 1
        "let/cc" 1
        "let/ec" 1
        "match" 1
        "match*" 1
        "match-define" 'define
        "match-lambda" 0
        "match-lambda*" 0
        "match-let" 1
        "match-let*" 1
        "match-let*-values" 1
        "match-let-values" 1
        "match-letrec" 1
        "match-letrec-values" 1
        "match/values" 1
        "mixin" 2
        "module" 2
        "module+" 1
        "module*" 2
        "opt-lambda" 1
        "parameterize" 1
        "parameterize-break" 1
        "parameterize*" 1
        "quasisyntax/loc" 1
        "receive" 2
        "require/typed" 1
        "require/typed/provide" 1
        "send*" 1
        "shared" 1
        "sigaction" 1
        "splicing-let" 1
        "splicing-letrec" 1
        "splicing-let-values" 1
        "splicing-letrec-values" 1
        "splicing-let-syntax" 1
        "splicing-letrec-syntax" 1
        "splicing-let-syntaxes" 1
        "splicing-letrec-syntaxes" 1
        "splicing-letrec-syntaxes+values" indent-for/fold-untyped
        "splicing-local" 1
        "splicing-syntax-parameterize" 1
        "struct" 'define
        "syntax-case" 2
        "syntax-case*" 3
        "syntax-rules" 1
        "syntax-id-rules" 1
        "syntax-parse" 1
        "syntax-parser" 0
        "syntax-parameterize" 1
        "syntax/loc" 1
        "syntax-parse" 1
        "test-begin" 0
        "test-case" 1
        "unit" 'define
        "unit/sig" 2
        "unless" 1
        "when" 1
        "while" 1
        ;; with- forms default to 1 unless otherwise specified here
        ))

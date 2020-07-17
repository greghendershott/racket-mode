#lang racket/base

;; WIP example of an indenter for sexp langs, implemented on a
;; token-map.
;;
;; This prototype uses the Emacs idea of indent specifications for
;; macros, where the spec is a number of distinguished forms, 'defun,
;; or an arbitrary Emacs Lisp function. Here so far we support the
;; number, 'defun renamed 'define, add 'begin, and an arbitary Racket
;; function. However this is currently hardcoded here. We do NOT yet
;; try to support the idea of a #lang -- or even a simple provided
;; syntax transformer -- supplying such a specification. However the
;; idea here is that an indenter like this would be supplied by a
;; #lang's get-info. So this code is more "for example", or /maybe/
;; could be part of a convenience library package. But it is not
;; intended to live inside Racket Mode per se. It simply exists here,
;; for now, to research the feasibility of doing this with Emacs.

(require racket/match
         "token-map.rkt"
         "util.rkt")

(provide indent-amount)

(module+ test
  (require rackunit))

;; token-map? positive-integer? -> nonnegative-integer?
(define (indent-amount tm indent-pos)
  (match (backward-up tm indent-pos)
    [(and (? number? open-pos)
          (app (end-of-hash-literal/keyword tm)
               (? number? end)))
     (- end (beg-of-line tm open-pos))]
    [(? number? open-pos)
     (define id-pos (forward-whitespace tm (add1 open-pos)))
     (define id-name (token-text tm id-pos))
     (log-racket-mode-debug "indent-amount id-name is ~v at ~v" id-name id-pos)
     (match (hash-ref ht-methods
                      id-name
                      (λ ()
                        (match id-name
                          [(regexp "^def|with-")  'define]
                          [(regexp "^begin")      'begin]
                          [(regexp "^for/|for*/") indent-for]
                          [_                      #f])))
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

(define ((end-of-hash-literal/keyword tm) pos)
  (match (classify tm pos)
    [(bounds+token _beg end
                   (or (token:misc _ _ 'hash-keyword)
                       (token:expr:open (regexp "^#(.*?)\\(") _ _ _)))
     end]
    [_ #f]))

(define (default-amount tm id-pos [def #f])
  (define bol (beg-of-line tm id-pos))
  (define eol (end-of-line tm id-pos))
  (define 1st-arg-pos (beg-of-next-sexp tm id-pos))
  (if (and 1st-arg-pos (< 1st-arg-pos eol)) ;on same line?
      (- 1st-arg-pos bol)
      (or def (- id-pos bol))))

(define (special-form tm open-pos id-pos indent-pos special-args [special-indent 4])
  ;; Up to special-args get extra indent (+4), the remainder get body
  ;; indent (+2).
  (define open-column (- open-pos (beg-of-line tm open-pos)))
  (cond
    [(zero? special-args)
     (default-amount tm id-pos (+ open-column 2))]
    [else
     (define indent-pos-bol (beg-of-line tm indent-pos))
     (define (first-arg-on-indent-line? pos)
       (= (beg-of-line tm pos) indent-pos-bol))
     (define args
       (let loop ([arg-end (forward-sexp tm (forward-sexp tm id-pos))]
                  [count 0])
         (if (and arg-end
                  (<= arg-end indent-pos))
             (match (forward-sexp tm arg-end)
               [(? integer? n) #:when (first-arg-on-indent-line? n) (add1 count)]
               [(? integer? n) (loop n (add1 count))]
               [#f (add1 count)])
             count)))
     (+ open-column (if (< args special-args) special-indent 2))]))

(define (indent-maybe-named-let tm open-pos id-pos indent-pos)
  (special-form tm open-pos id-pos indent-pos
                (match (classify tm (beg-of-next-sexp tm id-pos))
                  [(bounds+token _beg _end (? token:misc? t))
                   #:when (eq? (token:misc-kind t) 'symbol)
                   2]
                  [_ 1])))

(module+ test
  (let ([tm (create "#lang racket\n(let ()\n  a\n  b)")])
    (check-equal? (indent-amount tm 24) 2)
    (check-equal? (indent-amount tm 28) 2))
  (let ([tm (create "#lang racket\n(let loop ()\n  a\n  b)")])
    (check-equal? (indent-amount tm 29) 2)
    (check-equal? (indent-amount tm 33) 2)))

(define (indent-for tm open-pos id-pos indent-pos)
  ;; Default indent function for "for/" and "for*/" forms not
  ;; otherwise specified ( such as for/fold and for*/fold).
  ;;
  ;; Checks for either of:
  ;;   - maybe-type-ann e.g. (for/list : T ([x xs]) x)
  ;;   - for/vector optional length, (for/vector #:length ([x xs]) x)
  (special-form tm open-pos id-pos indent-pos
                (match (beg-of-next-sexp tm id-pos)
                  [(bounds+token _beg _end (? token:misc? t))
                   #:when (or (eq? (token:misc-kind t) 'hash-colon-keyword)
                              (and (eq? (token:misc-kind t) 'symbol)
                                   (equal? (token-lexeme t) ":")))
                   3]
                  [_ 1])))

(module+ test
  (let ([tm (create "#lang racket\n(for/list ()\n  a\n b)")])
    (check-equal? (indent-amount tm 29) 2)
    (check-equal? (indent-amount tm 32) 2))
  (let ([tm (create "#lang racket\n(for/list : T ()\n  a\n b)")])
    (check-equal? (indent-amount tm 33) 2)
    (check-equal? (indent-amount tm 37) 2))
  (let ([tm (create "#lang racket\n(for/vector #:length 2 ()\n  a\n b)")])
    (check-equal? (indent-amount tm 42) 2)
    (check-equal? (indent-amount tm 45) 2))
  (let ([tm (create "#lang racket\n(if a\n    (for/list ([x xs])\n      (cond 1\n            2))\n    b)")])
    (check-equal? (indent-amount tm 24) 4)
    (check-equal? (indent-amount tm 77) 4))
  (let ([tm (create "#lang racket\n\n(if a\n    (for/list ([x xs])\n      (cond 1\n            2)))")])
    (check-equal? (indent-amount tm 73) 4)
    (check-equal? (indent-amount tm 74) 0)
    (check-equal? (update tm 74 0 "\n")
                  (list (bounds+token 74 75 (token:misc "\n" 0 'end-of-line))))
    (local-require (submod "token-map.rkt" test))
    (check-valid? tm)
    (check-equal? (indent-amount tm 75) 0)))

(define (indent-for/fold tm open-pos id-pos indent-pos)
  ;; check for maybe-type-ann e.g. (for/fold : T ([n 0]) ([x xs]) x)
  (match (beg-of-next-sexp tm id-pos)
    [(bounds+token _beg _end (? token:misc? t))
     #:when (or (eq? (token:misc-kind t) 'hash-colon-keyword)
                (and (eq? (token:misc-kind t) 'symbol)
                     (equal? (token-lexeme t) ":")))
     (special-form tm open-pos id-pos indent-pos 4)]
    [_ (indent-for/fold-untyped tm open-pos id-pos indent-pos)]))

(define (indent-for/fold-untyped tm open-pos id-pos indent-pos)
  ;; If the first, accumulator form is NOT on the same line as id-pos,
  ;; then this is simply special-form with 2 distinguished forms.
  ;; Otherwise, we want to indent the second form with the first (like
  ;; a normal procedure argument) and of course all other forms get
  ;; body indent.
  (define bol (beg-of-line tm id-pos))
  (define eol (end-of-line tm id-pos))
  (define 1st-arg-pos (beg-of-next-sexp tm id-pos))
  (if (and 1st-arg-pos (< 1st-arg-pos eol)) ;on same line?
      (special-form tm open-pos id-pos indent-pos 2 (- 1st-arg-pos bol))
      (special-form tm open-pos id-pos indent-pos 2)))

(module+ test
  (let ([tm (create "#lang racket\n(for/fold ()\n         ()\n  a)")])
    ;;               1234567890123 4567890123456 789012345678 9012
    ;;                        1          2          3          4
    (check-equal? (indent-amount tm 36) 10)
    (check-equal? (indent-amount tm 41) 2))
  (let ([tm (create "#lang racket\n(for/fold\n    ()\n    ()\n  a)")])
    ;;               1234567890123 4567890123 4567890 12345678 9012
    ;;                        1          2          3          4
    (check-equal? (indent-amount tm 28) 4)
    (check-equal? (indent-amount tm 35) 4)
    (check-equal? (indent-amount tm 40) 2)))

(define (beg-of-next-sexp tm pos)
  (backward-sexp tm
                 (forward-sexp tm
                               (forward-sexp tm pos))))

#;(require racket/trace)
#;(trace indent-amount
       special-form
       default-amount
       beg-of-next-sexp)

(module+ test
  (let ()
    (define str "#lang racket\n(foo\n  bar\nbaz)\n(foo bar baz\nbap)\n(define (f x)\nx)\n")
    ;;           1234567890123 45678 901234 56789 012345678 901234567 89012345678901 234 56789012 345 67 89
    ;;                    1           2           3          4          5         6           7
    (define tm (create str))
    (check-equal? (indent-amount tm  1) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 14) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 15) 1
                  "should indent with foo")
    (check-equal? (indent-amount tm 22) 1
                  "bar should indent with foo")
    (check-equal? (indent-amount tm 25) 1
                  "baz should indent with bar (assumes bar not yet re-indented)")
    (check-equal? (indent-amount tm 28) 1
                  "close paren after baz is on same line as it, same result")
    (check-equal? (indent-amount tm 30) 0
                  "not within any sexpr, should indent 0")
    (check-equal? (indent-amount tm 43) 5
                  "bap should indent with the 2nd sexp on the same line i.e. bar")
    (check-equal? (indent-amount tm 62) 2
                  "define body should indent 2"))
  (let ()
    (define str "#lang racket\n(begin0\n42\n1\n2)\n")
    ;;           1234567890123 45678901 234 56 789 0
    ;;                    1          2             3
    (define tm (create str))
    (check-equal? (indent-amount tm 22) 4
                  "begin0 result should indent 4")
    (check-equal? (indent-amount tm 25) 2
                  "begin0 first other expr should indent 2")
    (update tm 25 0 "  ") ;actually indent that, for next check...
    (check-equal? (indent-amount tm 27) 2
                  "begin0 second other expr should indent same as first other"))
  (let ()
    (define str "#lang racket\n(print 12")
    ;;           1234567890123 4567890123
    ;;                    1           2
    (define tm (create str))
    (update tm 23 0 "\n")
    (check-equal? (indent-amount tm 22) 7)
    (check-equal? (indent-amount tm 23) 7))
  (let ()
    (define str "#lang racket\n(if 123\n)\n(do)")
    ;;           1234567890123 45678901 23 456
    ;;                    1          2
    (define tm (create str))
    (update tm 21 0 "\n")
    (check-equal? (indent-amount tm 22) 4
                  "position is exactly on close token"))
  (let ()
    (define str "#lang racket\n(cond [#t #t]\n[#f #f])")
    ;;           1234567890123 45678901234567 89012345
    ;;                    1          2          3
    (check-equal? (indent-amount (create str) 28)
                  6
                  "second cond clause indented properly"))
  (let ()
    (define str "#lang racket\n(cond\n  [#t #t]\n  [#f #f])")
    ;;           1234567890123 456789 0123456789 012345
    ;;                    1           2          3
    (define tm (create str))
    (check-equal? (indent-amount tm 22)
                  2
                  "first cond clause indented properly")
    (check-equal? (indent-amount tm 32)
                  2
                  "second cond clause indented properly"))
  (let ()
    (define str "#lang racket\n#hasheq((1 . 2)\n(3. 4))\n")
    ;;           1234567890123 4567890123456789 012345
    ;;                    1          2          3
    (define tm (create str))
    (check-equal? (indent-amount tm 30)
                  8
                  "#hasheq indented properly")))

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

(module+ example-6
  )

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

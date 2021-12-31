#lang racket/base

(require rackunit
         framework
         racket/class
         racket/dict
         racket/async-channel
         racket/match
         racket/port
         net/url
         "../../racket/lang-info.rkt")

(define hash-lang%
  (with-handlers ([exn:fail:filesystem:missing-module?
                   (λ _
                     (displayln "hash-lang% NOT available: SKIPPING hash-lang tests")
                     (exit 0))])
    (dynamic-require "../../racket/hash-lang.rkt" 'hash-lang%)))

(displayln "hash-lang% IS available: running hash-lang tests")

(define racket-lexer  (dynamic-require 'syntax-color/racket-lexer 'racket-lexer))
(define racket-lexer* (dynamic-require 'syntax-color/racket-lexer 'racket-lexer*))

;; To test async updates from the updater thread, we supply an
;; on-changed-tokens override method that puts some of them to a
;; "results" channel.
(define result-channel (make-async-channel))
(define (test-create str #:other-lang-source [other-lang-source #f])
  (define our-hash-lang%
    (class hash-lang%
      (super-new [other-lang-source other-lang-source])
      (define/override (on-changed-tokens gen beg end)
        (async-channel-put result-channel (list gen beg end)))))
  (define o (new our-hash-lang%))
  (test-update! o 1 0 0 str)
  o)
(define (test-update! o gen pos old-len str)
  (send o update! gen pos old-len str)
  (match (async-channel-get result-channel)
    [(list gen beg end)
     (send o get-tokens gen beg end)]))

;;; Various tests of tokenizing and updating

(let* ([str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar"]
       ;;    0123456789012 345678901234 567890 12345678901234567890123456
       ;;             1          2          3          4         5
       [o (test-create str)])
  (check-equal? (send o get-tokens 1)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 15 'constant)
                 (list 15 16 'white-space)
                 (list 16 17 'parenthesis)
                 (list 17 22 'symbol)
                 (list 22 23 'white-space)
                 (list 23 30 'string)
                 (list 30 31 'parenthesis)
                 (list 31 32 'white-space)
                 (list 32 38 'symbol)
                 (list 38 39 'parenthesis)
                 (list 39 44 'symbol)
                 (list 44 45 'parenthesis)
                 (list 45 46 'white-space)
                 (list 46 47 'constant)
                 (list 47 50 'symbol)
                 (list 50 51 'white-space)
                 (list 51 56 'hash-colon-keyword)))
  (check-equal? (send o -get-content) str)
  (check-equal? (send o -get-modes)
                `((0 12 (,racket-lexer* . #f))
                  (12 1 (,racket-lexer* . ,(void)))
                  (13 2 (,racket-lexer* . ,(void)))
                  (15 1 (,racket-lexer* . ,(void)))
                  (16 1 (,racket-lexer* . ,(void)))
                  (17 5 (,racket-lexer* . ,(void)))
                  (22 1 (,racket-lexer* . ,(void)))
                  (23 7 (,racket-lexer* . ,(void)))
                  (30 1 (,racket-lexer* . ,(void)))
                  (31 1 (,racket-lexer* . ,(void)))
                  (32 6 (,racket-lexer* . ,(void)))
                  (38 1 (,racket-lexer* . ,(void)))
                  (39 5 (,racket-lexer* . ,(void)))
                  (44 1 (,racket-lexer* . ,(void)))
                  (45 1 (,racket-lexer* . ,(void)))
                  (46 1 (,racket-lexer* . ,(void)))
                  (47 3 (,racket-lexer* . ,(void)))
                  (50 1 (,racket-lexer* . ,(void)))
                  (51 5 (,racket-lexer* . ,(void))))
                "racket-lexer* used for #lang racket")
  (check-equal? (test-update! o 2 51 5 "'bar")
                '((50 51 white-space)
                  (51 52 constant)
                  (52 55 symbol)))
  (check-equal? (test-update! o 3 46 4 "'bar")
                '()
                "Although lexeme changed from 'foo' to 'bar', the token bounds did not change nor did the type 'symbol nor the backup")

  (check-equal? (test-update! o 4 23 7 "'hell")
                '((22 23 white-space)
                  (23 24 constant)
                  (24 28 symbol)))
  (check-equal? (test-update! o 5 13 2 "99999")
                '((13 18 constant)))
  ;; Double check final result of the edits
  (check-equal? (send o -get-content)
                "#lang racket\n99999 (print 'hell) @print{Hello} 'bar 'bar")
  ;;             0123456789012 34567890123456789012345678901234567890123456
  ;;                       1          2         3         4         5
  (check-equal? (send o get-tokens 1)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 18 'constant)
                 (list 18 19 'white-space)
                 (list 19 20 'parenthesis)
                 (list 20 25 'symbol)
                 (list 25 26 'white-space)
                 (list 26 27 'constant)
                 (list 27 31 'symbol)
                 (list 31 32 'parenthesis)
                 (list 32 33 'white-space)
                 (list 33 39 'symbol)
                 (list 39 40 'parenthesis)
                 (list 40 45 'symbol)
                 (list 45 46 'parenthesis)
                 (list 46 47 'white-space)
                 (list 47 48 'constant)
                 (list 48 51 'symbol)
                 (list 51 52 'white-space)
                 (list 52 53 'constant)
                 (list 53 56 'symbol))))

(let* ([str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar"]
       [o (test-create str)])
  (check-equal? (send o get-tokens 1)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 19 'symbol)
                 (list 19 20 'white-space)
                 (list 20 22 'constant)
                 (list 22 23 'white-space)
                 (list 23 24 'parenthesis)
                 (list 24 29 'symbol)
                 (list 29 30 'white-space)
                 (list 30 37 'string)
                 (list 37 38 'parenthesis)
                 (list 38 39 'white-space)
                 (list 39 40 'parenthesis) ;;??
                 (list 40 45 'symbol)
                 (list 45 46 'parenthesis)
                 (list 46 59 'text)
                 (list 59 60 'parenthesis)
                 (list 60 61 'white-space)
                 (list 61 62 'constant)
                 (list 62 65 'symbol)
                 (list 65 66 'white-space)
                 (list 66 71 'hash-colon-keyword)))
  (for ([(_ mode) (in-dict (send o -get-modes))])
    (check-false (equal? mode racket-lexer)
                 "racket-lexer NOT used for #lang at-exp")
    (check-false (equal? mode racket-lexer*)
                 "racket-lexer* NOT used for #lang at-exp"))
  (check-equal? (send o -get-content) str)
  (check-equal? (send o classify 1 (sub1 (string-length str)))
                (list 66 71 'hash-colon-keyword)))

(let* ([str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword"]
       [o (test-create str)])
  (check-equal? (send o get-tokens 1)
                (list
                 (list 0  19 'text)
                 (list 19 20 'white-space)
                 (list 20 26 'text)
                 (list 26 27 'parenthesis) ;;??
                 (list 27 28 'parenthesis)
                 (list 28 33 'symbol)
                 (list 33 34 'white-space)
                 (list 34 41 'string)
                 (list 41 42 'parenthesis)
                 (list 42 43 'text)
                 (list 43 44 'parenthesis)
                 (list 44 49 'symbol)
                 (list 49 50 'parenthesis)
                 (list 50 63 'text)
                 (list 63 64 'parenthesis)
                 (list 64 80 'text)))
  (for ([(_ mode) (in-dict (send o -get-modes))])
    (check-false (equal? mode racket-lexer)
                 "racket-lexer NOT used for #lang at-exp")
    (check-false (equal? mode racket-lexer*)
                 "racket-lexer* NOT used for #lang at-exp"))
  (check-equal? (send o -get-content) str))

(let* ([str "#lang racket\n(λ () #t)"]
       ;;    0123456789012 34567890
       ;;              1          2
       [o  (test-create str)])
  (check-equal? (send o classify 1 14)
                (list 14 15 'symbol))
  (check-equal? (test-update! o 2 17 0 "a")
                '((17 18 symbol)))
  (check-equal? (send o classify 2 17)
                (list 17 18 'symbol)))

(let ([o (test-create "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")])
  (check-equal? (send o get-tokens 1)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 22 'string)
                 (list 22 23 'white-space)
                 (list 23 25 'parenthesis)
                 (list 25 26 'constant)
                 (list 26 27 'white-space)
                 (list 27 28 'constant)
                 (list 28 29 'white-space)
                 (list 29 30 'constant)
                 (list 30 31 'parenthesis)
                 (list 31 32 'white-space)
                 (list 32 34 'constant)
                 (list 34 35 'parenthesis)
                 (list 35 36 'constant)
                 (list 36 37 'white-space)
                 (list 37 38 'constant)
                 (list 38 39 'white-space)
                 (list 39 40 'constant)
                 (list 40 41 'parenthesis))))

(let ([o (test-create "#lang racket\n#<<HERE\nblah blah\nblah blah\nHERE\n")])
  (check-equal? (send o get-tokens 1)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 45 'string)
                 (list 45 46 'white-space))))


(let ()
  (define str "#lang racket\n")
  ;;           1234567890123 45678901234 567890 12345678901234567890123456
  ;;                    1          2          3          4         5
  (define o (test-create str))
  (test-update! o 2 13 0 "()")
  (test-update! o 3 14 0 "d")
  (test-update! o 4 15 0 "o")
  (test-update! o 5 14 0 "1")
  (test-update! o 6 15 0 "2")
  (test-update! o 7 16 0 " ")
  (void))

(let* ([str "#lang racket\n"]
       ;;    1234567890123 4
       ;;             1
       [o (test-create str)])
  (test-update! o 2 13 0 "d")
  (test-update! o 3 14 0 "o")
  (check-equal? (send o -get-content) "#lang racket\ndo")
  (check-equal? (send o get-tokens 3)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 15 'symbol))))

(let* ([str "#lang racket\n"]
       ;;    0123456789012 3
       ;;              1
       [o (test-create str)])
  (test-update! o 2 13 0 "1") ;initially lexed as 'constant
  (test-update! o 3 14 0 "x") ;should re-lex "1x" as 'symbol
  (check-equal? (send o -get-content) "#lang racket\n1x")
  (check-equal? (send o get-tokens 3)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 15 'symbol))))
(let* ([str "#lang racket\n"]
       ;;    0123456789012 34
       ;;              1
       [o (test-create str)])
  (test-update! o 2 13 0 "1") ;initially lexed as 'constant
  (test-update! o 3 14 0 "x") ;should re-lex "1x" as 'symbol
  (test-update! o 4 15 0 "1") ;still symbol
  (test-update! o 5 14 1 "")  ;deleting the "x" should re-lex the "11" as constant
  (check-equal? (send o -get-content) "#lang racket\n11")
  (check-equal? (send o get-tokens 5)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 15 'constant))))

(let* ([str "#lang racket\n"]
       ;;    1234567890123 4
       ;;             1
       [o (test-create str)])
  ;; as if paredit etc. were enabled
  (test-update! o 2 13 0 "(")
  (test-update! o 3 14 0 ")")
  (test-update! o 4 14 0 "h")
  (test-update! o 5 15 0 "i")
  (check-equal? (send o -get-content) "#lang racket\n(hi)")
  (check-equal? (send o get-tokens 5)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 14 'parenthesis)
                 (list 14 16 'symbol)
                 (list 16 17 'parenthesis))))

;; Exercise calling update! from various threads and out-of-order
;; wrt the generation number.
(let* ([str "#lang racket\n"]
       ;;    0123456789012 3
       ;;              1
       [o (new hash-lang%)])
  (send o update! 1 0 0 str)
  (thread (λ () (send o update! 2 13 0 "(")))
  (thread (λ () (send o update! 4 14 0 "h")))
  (thread (λ () (send o update! 5 15 0 "i")))
  (thread (λ () (send o update! 3 14 0 ")")))
  (check-equal? (send o get-tokens 5)
                (list
                 (list  0 12 'other)
                 (list 12 13 'white-space)
                 (list 13 14 'parenthesis)
                 (list 14 16 'symbol)
                 (list 16 17 'parenthesis))))

(let* ([str "#lang racket\n#hash\n"]
       ;;    0123456789012 3456789
       ;;              1
       [o (test-create str)])
  (check-equal? (send o classify 1 13)
                (list 13 18 'error))
  (check-equal? (test-update! o 2 18 0 "(")
                '((13 19 parenthesis))
                "Adding parens after #hash re-lexes from an error to an open")
  (check-equal? (send o classify 2 13)
                (list 13 19 'parenthesis)))

(let* ([str "#lang racket\n\n(1 2)"]
       ;;    0123456789012 3 456789
       ;;              1
       [o (test-create str)])
  (check-equal? (test-update! o 2 13 0 "(")
                '((12 13 white-space)
                  (13 14 parenthesis)
                  (14 15 white-space))
                "Update that splits an existing token does not produce execessive notifications.")
  (check-equal? (test-update! o 3 13 0 ")")
                '((13 14 parenthesis))))

(let* ([str "#lang racket\n\n#;(1 2)"]
       ;;    0123456789012 3 4567890
       ;;              1           2
       [o (test-create str)])
  (check-equal? (send o get-tokens 1)
                (list
                 (list  0 12 'other)
                 (list 12 14 'white-space)
                 (list 14 16 'sexp-comment)
                 (list 16 17 '#hash((comment? . #t) (type . parenthesis)))
                 (list 17 18 '#hash((comment? . #t) (type . constant)))
                 (list 18 19 '#hash((comment? . #t) (type . white-space)))
                 (list 19 20 '#hash((comment? . #t) (type . constant)))
                 (list 20 21 '#hash((comment? . #t) (type . parenthesis))))))

(let* ([str "#lang scribble/manual\n\ntext text\ntext text\n"]
       ;;    0123456789012345678901 2 3456789012 3456789012
       ;;              1         2           3          4
       [o (test-create str)])
  (check-equal? (send o get-tokens 1)
                '((0 21 text)
                  (21 23 white-space)
                  (23 32 text)
                  (32 33 white-space)
                  (33 42 text)
                  (42 43 white-space))
                "#lang scribble/manual: Initial lex is just lines of text tokens.")
  (check-equal? (test-update! o 2 22 0 "@(1 a 3")
                '((21 22 white-space)
                  (22 23 parenthesis)
                  (23 24 parenthesis)
                  (24 25 constant)
                  (25 26 white-space)
                  (26 27 symbol)
                  (27 28 white-space)
                  (28 29 constant)
                  (29 30 white-space)
                  (30 34 symbol)
                  (34 35 white-space)
                  (35 39 symbol)
                  (39 40 white-space)
                  (40 44 symbol)
                  (44 45 white-space)
                  (45 49 symbol))
                "#lang scribble/manual: adding \"@(1 a 3\" with no close paren causes text tokens to become symbol and white-space tokens, i.e. as if part of the new s-expression.")
  (check-equal? (send o -get-content)
                "#lang scribble/manual\n@(1 a 3\ntext text\ntext text\n")
  ;;             0123456789012345678901 23456789 0123456789 0123456789
  ;;                       1         2           3          4
  (check-equal? (test-update! o 3 29 0 ")")
                '((29 30 parenthesis)
                  (30 31 white-space)
                  (31 40 text)
                  (40 41 white-space)
                  (41 50 text))
                "#lang scribble/manual: adding a ) to close an unmatched @( causes things after the ) to be tokenzied back to text.")
  (check-equal? (send o -get-content)
                "#lang scribble/manual\n@(1 a 3)\ntext text\ntext text\n")
  ;;             0123456789012345678901 234567890 1234567890 1234567890 1
  ;;                       1         2          3          4          5
  )

(let* ([o (test-create "" #:other-lang-source "#lang scribble/manual")])
  (test-update! o 2 0 0 "blah blah blah @racket[x]")
  (check-equal? (send o get-tokens 1)
                '((0 15 text) ;; "blah blah blah"
                  (15 16 parenthesis)
                  (16 22 symbol)
                  (22 23 parenthesis)
                  (23 24 symbol)
                  (24 25 parenthesis))
                "other-lang-source used to tokenize using #lang scribble/manual instead of default #lang racket"))

(unless (getenv "CI") ;needs github.com:mflatt/shrubbery-rhombus-0.git
  (let* ([o (test-create "#lang rhombus\n@//{block comment}")]
         ;;               01234567890123 4567890123456789012
         ;;                         1          2         3
         [gen-1-tokens (send o get-tokens 1)])
    (check-equal? (test-update! o 2 16 0 " ")
                  '((14 15 #hasheq((rhombus-type . at) (type . at)))
                    (15 16 #hasheq((rhombus-type . operator) (type . operator)))
                    (16 17 #hasheq((rhombus-type . whitespace) (type . white-space)))
                    (17 18 #hasheq((rhombus-type . operator) (type . operator)))
                    (18 19 #hasheq((rhombus-type . opener) (type . parenthesis)))
                    (19 24 #hasheq((rhombus-type . identifier) (type . symbol)))
                    (24 25 #hasheq((rhombus-type . whitespace) (type . white-space)))
                    (25 32 #hasheq((rhombus-type . identifier) (type . symbol)))
                    (32 33 #hasheq((rhombus-type . closer) (type . parenthesis))))
                  "non-zero backup amounts are used: edit removes block comment")
    (check-equal? (test-update! o 3 16 1 "")
                  '((14 17 #hasheq((rhombus-type . at-comment) (type . comment)))
                    (17 18 #hasheq((comment? . #t) (rhombus-type . at-opener) (type . parenthesis)))
                    (18 31 #hasheq((comment? . #t) (rhombus-type . at-content) (type . text)))
                    (31 32 #hasheq((comment? . #t) (rhombus-type . at-closer) (type . parenthesis))))
                  "non-zero backup amounts are used: edit restores block comment")
    (check-equal? gen-1-tokens
                  (send o get-tokens 3)
                  "non-zero backup amounts are used: edits remove and restore block comment")))

(let ([o (test-create "#lang scribble/manual\ntext @|foo| @foo|{text}| @;{comment} @;comment")]
      ;;               0123456789012345678901 23456789012345678901234567890123456789012345678
      ;;                         1          2         3         4         5         6
      )
  (check-equal? (test-update! o 2 38 0 " ")
                '((38 48 text))
                "non-zero backup amounts are used"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test equivalance of our text%-like methods to those of racket:text%
;;;

(define (check-string str
                      #:check-motion? check-motion?
                      #:check-indent? check-indent?)
  (define what (string-append (substring str 0 20) "..."))
  ;; Create an object of our class.
  (define o (test-create str))
  ;; Create an object of racket:text%, which also implements the
  ;; color:text<%> interface. Since our class reads lang info to get
  ;; things like the initial lexer and paren-matches, give those
  ;; values from our object to color:text<%> `start-colorer`.
  (define t (new racket:text%))
  (send t start-colorer symbol->string
        (lang-info-lexer (send o get-lang-info))
        (lang-info-paren-matches (send o get-lang-info)))
  (send t insert str)
  (send t freeze-colorer)
  (send t thaw-colorer)

  (define lp (string-length str))
  (check-equal? (send o last-position)
                lp)
  (check-equal? (send o last-position)
                (send t last-position))

  ;; Test that our implementation of skip-whitespace is equivalent
  ;; to racket:text%.
  (for ([pos (in-range 0 (string-length str))])
    (for* ([dir (in-list '(forward backward))]
           [comments? (in-list '(#f #t))])
      (check-equal? (send o skip-whitespace pos dir comments?)
                    (send t skip-whitespace pos dir comments?)
                    (format "skip-whitespace ~v ~v ~v in ~a" pos dir comments? what))))

  ;; Test that our implementation of position-paragraph is
  ;; equivalent to racket:text%.
  (for ([pos (in-range 0 (string-length str))])
    (check-equal? (send o position-paragraph pos)
                  (send t position-paragraph pos)
                  (format "position-paragraph ~v in ~a" pos what)))

  ;; Test that our implementation of paragraph-start-position and
  ;; paragraph-end-position are equivalent to racket:text%.
  (define num-paras (add1 (for/sum ([c (in-string str)])
                            (if (char=? c #\newline) 1 0))))
  (for ([para (in-range 0 num-paras)])
    (check-equal? (send o paragraph-start-position para)
                  (send t paragraph-start-position para)
                  (format "paragraph-start-position ~v in ~a" para what)))
  (check-exn exn:fail?
             (λ () (send o paragraph-start-position (add1 num-paras))))
  (for ([para (in-range 0 (+ num-paras 2))]) ;should work for excess para #s
    (check-equal? (send o paragraph-end-position para)
                  (send t paragraph-end-position para)
                  (format "paragraph-end-position ~v in ~a" para what)))

  (when check-motion?
    ;; Test that our implementations of {forward backward}-match and
    ;; backward-containing-sexp are equivalent to those of
    ;; racket:text%.
    (for ([pos (in-range 0 (string-length str))])
      (send t set-position pos pos)
      (check-equal? (send o forward-match pos lp)
                    (send t forward-match pos lp)
                    (format "forward-match ~v ~v in ~a" pos lp what))
      (check-equal? (send o backward-match pos 0)
                    (send t backward-match pos 0)
                    (format "backward-match ~v ~v in ~a" pos 0 what))
      (check-equal? (send o backward-containing-sexp pos 0)
                    (send t backward-containing-sexp pos 0)
                    (format "backward-containing-sexp ~v ~v in ~a" pos 0 what))))

  (when check-indent?
    ;; Test that we supply enough color-text% methods, and that they
    ;; behave equivalently to those from racket-text%, as needed by a
    ;; lang-supplied drracket:indentation a.k.a. determine-spaces
    ;; function. (After all, this is our motivation to provide
    ;; text%-like methods; otherwise we wouldn't bother.)
    (define line-indent (lang-info-line-indenter (send o get-lang-info)))
    (when line-indent
      (for ([pos (in-range 0 (string-length str))])
        (when (or (= pos 0)
                  (char=? (string-ref str (sub1 pos)) #\newline))
          (check-equal? (line-indent o pos)
                        (line-indent t pos)
                        (format "~v ~v in ~a" line-indent pos what)))))

    ;; Test range-indent.
    (define range-indent (lang-info-range-indenter (send o get-lang-info)))
    (when range-indent
      (define len (string-length str))
      (check-equal? (range-indent o 0 len)
                    (range-indent t 0 len)))))

(let ([str "#lang racket\n(1) #(2) #hash((1 . 2))\n@racket[]{\n#(2)\n}\n"]
      ;;    0123456789012 345678901234567890123456 78901234567 89012 34
      ;;              1          2         3          4          5
      )
  (check-string str
                #:check-motion? #t
                #:check-indent? #t))

(let ([str "#lang at-exp racket\n(1) #(2) #hash((1 . 2))\n@racket[]{\n#(2)\n}\n"]
      ;;    01234567890123456789 012345678901234567890123 45678901234 56789 01
      ;;              1          2         3         4          5           6
      )
  (check-string str
                #:check-motion? #t
                ;; This needs a newer at-exp from Racket 8.3.0.8+,
                ;; which avoids using the `find-up-sexp` method.
                #:check-indent? #t))

(check-string (call/input-url (string->url "https://raw.githubusercontent.com/mflatt/shrubbery-rhombus-0/master/demo.rkt") get-pure-port port->string)
              #:check-motion? #f ;large file & we already test motion above
              #:check-indent? #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Benchmarks
;;;

(when #f

  (define (cpu-time proc)
    (define-values (_results cpu _real _gc) (time-apply proc null))
    cpu)

  (define (bench what str)
    (newline)
    (displayln (make-string 76 #\-))
    (printf "BENCHMARK: ~v\n" what)
    (println (string-append (substring str 0 20) "..."))

    ;; Create an object of our class.
    (define o (new hash-lang%))
    (send o update! 1 0 0 str)

    ;; Create an object of racket:text%, which also implements the
    ;; color:text<%> interface. Since our class reads lang info to get
    ;; things like the initial lexer and paren-matches, give those
    ;; values from our object to color:text<%> `start-colorer`.
    (define t (new racket:text%))
    (send t start-colorer symbol->string
          (lang-info-lexer (send o get-lang-info))
          (lang-info-paren-matches (send o get-lang-info)))
    (send t insert str)
    (send t freeze-colorer)
    (send t thaw-colorer)

    (define (compare what reps proc)
      (newline)
      (displayln what)
      (define o-time (cpu-time (λ () (for ([_ reps]) (proc o)))))
      (define t-time (cpu-time (λ () (for ([_ reps]) (proc t)))))
      (define factor (/ (* 1.0 o-time) t-time))
      (printf "~v ~v\n~v ~v\n~v\n"
              o-time o
              t-time t
              factor))

    (compare "paragraph methods"
             10
             (λ (t)
               (for ([pos (in-range 0 (string-length str))])
                 (define para (send t position-paragraph pos))
                 (send t paragraph-start-position para)
                 (send t paragraph-end-position para))))

    (compare "classify-position*"
             10
             (λ (t)
               (for ([pos (in-range 0 (string-length str))])
                 (send t classify-position* pos))))

    (compare "get-token-range"
             10
             (λ (t)
               (for ([pos (in-range 0 (string-length str))])
                 (send t get-token-range pos))))

    (compare "skip-whitespace 'forward"
             10
             (λ (t)
               (for ([pos (in-range 0 (string-length str))])
                 (send t skip-whitespace pos 'forward #t))))
    (compare "skip-whitespace 'backward"
             10
             (λ (t)
               (for ([pos (in-range 0 (string-length str))])
                 (send t skip-whitespace pos 'backward #t))))

    (compare "backward-match"
             1
             (λ (t)
               (for ([pos (in-range 0 (string-length str))])
                 (send t backward-match pos 0))))
    (compare "forward-match"
             1
             (λ (t)
               (define lp (send t last-position))
               (for ([pos (in-range 0 (string-length str))])
                 (send t forward-match pos lp))))

    (compare "backward-containing-sexp"
             1
             (λ (t)
               (for ([pos (in-range 0 (string-length str) 1000)])
                 (send t backward-containing-sexp pos 0))))

    (define line-indent (lang-info-line-indenter (send o get-lang-info)))
    (when line-indent
      (compare line-indent
               1
               (λ (t)
                 (for ([pos (in-range 0 (string-length str))])
                   (when (or (= pos 0)
                             (char=? (string-ref str (sub1 pos)) #\newline))
                     (line-indent t pos))))))

    (define range-indent (lang-info-range-indenter (send o get-lang-info)))
    (when range-indent
      (compare range-indent
               10
               (λ (t) (range-indent t 0 (string-length str))))))

  (displayln (make-string 76 #\=))

  (let* ([uri "https://raw.githubusercontent.com/racket/racket/448b77a6629c68659e1360fbe9f9e1ecea078f9c/pkgs/racket-doc/scribblings/reference/class.scrbl"]
         [str (call/input-url (string->url uri) get-pure-port port->string)])
    (bench uri str))

  (let* ([uri "https://raw.githubusercontent.com/mflatt/shrubbery-rhombus-0/master/demo.rkt"]
         [str (call/input-url (string->url uri) get-pure-port port->string)])
    (bench uri str))

  (newline))

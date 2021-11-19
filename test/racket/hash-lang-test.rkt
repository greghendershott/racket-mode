#lang racket/base

(require rackunit
         framework
         racket/class
         racket/dict
         racket/async-channel
         racket/match
         racket/port
         net/url
         syntax-color/racket-lexer
         "../../racket/hash-lang.rkt")

;; hash-lang.rkt will set hash-lang% to #f on a version of Racket
;; and/or syntax-color-lib that is too old to support
;; color-textoid<%>. In that case we skip tests.
(unless hash-lang%
  (displayln "syntax-color/color-textoid NOT available: SKIPPING hash-lang tests"))

;; Otherwise hash-lang% is a class.
(when hash-lang%
  (displayln "syntax-color/color-textoid is available: running hash-lang tests")
  ;; To test async notifications from the updater thread, we supply an
  ;; on-notify that puts them to a "gathering" channel, which
  ;; accumulates ('begin-update change ... 'end-update) sequences and
  ;; posts each list of changes to a "result" channel for test-create
  ;; and test-update to return as value for use with check-equal?.
  (define gathering-channel (make-async-channel))
  (define result-channel (make-async-channel))
  (void
   (thread
    (λ () (let loop ([xs null])
            (match (async-channel-get gathering-channel)
              ['(begin-update) (loop null)]
              ['(end-update)
               (async-channel-put result-channel (reverse xs))
               (loop null)]
              [(list _paren-matches beg end token)
               (loop (cons (list beg end (token-type token) (token-paren token))
                           xs))])))))
  (define (test-create str)
    (define o (new hash-lang% [on-notify (λ args (async-channel-put gathering-channel args))]))
    (test-update! o 1 1 0 str)
    o)
  (define (test-update! o gen pos old-len str)
    (send o update! gen pos old-len str)
    (async-channel-get result-channel))

  ;;; Various tests of tokenizing and updatng

  (let* ([str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar"]
         ;;    1234567890123 45678901234 567890 12345678901234567890123456
         ;;             1          2          3          4         5
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "42" 'constant #f 0))
                   (list 16 17 (token " " 'white-space #f 0))
                   (list 17 18 (token "(" 'parenthesis '\( 0))
                   (list 18 23 (token "print" 'symbol #f 0))
                   (list 23 24 (token " " 'white-space #f 0))
                   (list 24 31 (token "\"hello\"" 'string #f 0))
                   (list 31 32 (token ")" 'parenthesis '\) 0))
                   (list 32 33 (token " " 'white-space #f 0))
                   (list 33 39 (token "@print" 'symbol #f 0))
                   (list 39 40 (token "{" 'parenthesis '\{ 0))
                   (list 40 45 (token "Hello" 'symbol #f 0))
                   (list 45 46 (token "}" 'parenthesis '\} 0))
                   (list 46 47 (token " " 'white-space #f 0))
                   (list 47 48 (token "'" 'constant #f 0))
                   (list 48 51 (token "foo" 'symbol #f 0))
                   (list 51 52 (token " " 'white-space #f 0))
                   (list 52 57 (token "#:bar" 'hash-colon-keyword #f 0))))
    (check-equal? (send tm -get-content) str)
    (check-equal? (dict->list (send tm -get-modes))
                  `(((1 . 13) . #f)
                    ((13 . 14) . ,racket-lexer)
                    ((14 . 16) . ,racket-lexer)
                    ((16 . 17) . ,racket-lexer)
                    ((17 . 18) . ,racket-lexer)
                    ((18 . 23) . ,racket-lexer)
                    ((23 . 24) . ,racket-lexer)
                    ((24 . 31) . ,racket-lexer)
                    ((31 . 32) . ,racket-lexer)
                    ((32 . 33) . ,racket-lexer)
                    ((33 . 39) . ,racket-lexer)
                    ((39 . 40) . ,racket-lexer)
                    ((40 . 45) . ,racket-lexer)
                    ((45 . 46) . ,racket-lexer)
                    ((46 . 47) . ,racket-lexer)
                    ((47 . 48) . ,racket-lexer)
                    ((48 . 51) . ,racket-lexer)
                    ((51 . 52) . ,racket-lexer)
                    ((52 . 57) . ,racket-lexer)))
    (check-equal? (test-update! tm 2 52 5 "'bar")
                  '((52 53 constant #f)
                    (53 56 symbol #f)))
    (check-equal? (test-update! tm 3 47 4 "'bar")
                  '((48 51 symbol #f)))
    (check-equal? (test-update! tm 4 24 7 "'hell")
                  '((24 25 constant #f)
                    (25 29 symbol #f)))
    (check-equal? (test-update! tm 5 14 2 "99999")
                  '((14 19 constant #f)))
    ;; Double check final result of the edits
    (check-equal? (send tm -get-content)
                  "#lang racket\n99999 (print 'hell) @print{Hello} 'bar 'bar")
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 19 (token "99999" 'constant #f 0))
                   (list 19 20 (token " " 'white-space #f 0))
                   (list 20 21 (token "(" 'parenthesis '\( 0))
                   (list 21 26 (token "print" 'symbol #f 0))
                   (list 26 27 (token " " 'white-space #f 0))
                   (list 27 28 (token "'" 'constant #f 0))
                   (list 28 32 (token "hell" 'symbol #f 0))
                   (list 32 33 (token ")" 'parenthesis '\) 0))
                   (list 33 34 (token " " 'white-space #f 0))
                   (list 34 40 (token "@print" 'symbol #f 0))
                   (list 40 41 (token "{" 'parenthesis '\{ 0))
                   (list 41 46 (token "Hello" 'symbol #f 0))
                   (list 46 47 (token "}" 'parenthesis '\} 0))
                   (list 47 48 (token " " 'white-space #f 0))
                   (list 48 49 (token "'" 'constant #f 0))
                   (list 49 52 (token "bar" 'symbol #f 0))
                   (list 52 53 (token " " 'white-space #f 0))
                   (list 53 54 (token "'" 'constant #f 0))
                   (list 54 57 (token "bar" 'symbol #f 0))))
    (check-equal? (dict->list (send tm -get-modes))
                  `(((1 . 13) . #f)
                    ((13 . 14) . ,racket-lexer)
                    ((14 . 19) . ,racket-lexer)
                    ((19 . 20) . ,racket-lexer)
                    ((20 . 21) . ,racket-lexer)
                    ((21 . 26) . ,racket-lexer)
                    ((26 . 27) . ,racket-lexer)
                    ((27 . 28) . ,racket-lexer)
                    ((28 . 32) . ,racket-lexer)
                    ((32 . 33) . ,racket-lexer)
                    ((33 . 34) . ,racket-lexer)
                    ((34 . 40) . ,racket-lexer)
                    ((40 . 41) . ,racket-lexer)
                    ((41 . 46) . ,racket-lexer)
                    ((46 . 47) . ,racket-lexer)
                    ((47 . 48) . ,racket-lexer)
                    ((48 . 49) . ,racket-lexer)
                    ((49 . 52) . ,racket-lexer)
                    ((52 . 53) . ,racket-lexer)
                    ((53 . 54) . ,racket-lexer)
                    ((54 . 57) . ,racket-lexer))))

  (let* ([str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar"]
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (list  1 13 (token "#lang at-exp" 'other #f 0))
                   (list 13 14 (token " " 'white-space #f 0))
                   (list 14 20 (token "racket" 'symbol #f 0))
                   (list 20 21 (token "\n" 'white-space #f 0))
                   (list 21 23 (token "42" 'constant #f 0))
                   (list 23 24 (token " " 'white-space #f 0))
                   (list 24 25 (token "(" 'parenthesis '\( 0))
                   (list 25 30 (token "print" 'symbol #f 0))
                   (list 30 31 (token " " 'white-space #f 0))
                   (list 31 38 (token "\"hello\"" 'string #f 0))
                   (list 38 39 (token ")" 'parenthesis '\) 0))
                   (list 39 40 (token " " 'white-space #f 0))
                   (list 40 41 (token "@" 'parenthesis #f 0)) ;;??
                   (list 41 46 (token "print" 'symbol #f 0))
                   (list 46 47 (token "{" 'parenthesis '\{ 0))
                   (list 47 60 (token "Hello (there)" 'text #f 0))
                   (list 60 61 (token "}" 'parenthesis '\} 0))
                   (list 61 62 (token " " 'white-space #f 0))
                   (list 62 63 (token "'" 'constant #f 0))
                   (list 63 66 (token "foo" 'symbol #f 0))
                   (list 66 67 (token " " 'white-space #f 0))
                   (list 67 72 (token "#:bar" 'hash-colon-keyword #f 0))))
    (check-equal? (send tm -get-content) str)
    (check-equal? (send tm classify 1 (sub1 (string-length str)))
                  (list 67 72 (token "#:bar" 'hash-colon-keyword #f 0))))

  (let* ([str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword"]
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (list 1  20 (token "#lang scribble/text" 'text #f 0))
                   (list 20 21 (token "\n" 'white-space #f 0))
                   (list 21 27 (token "Hello " 'text #f 0))
                   (list 27 28 (token "@" 'parenthesis #f 0)) ;;??
                   (list 28 29 (token "(" 'parenthesis '\( 0))
                   (list 29 34 (token "print" 'symbol #f 0))
                   (list 34 35 (token " " 'white-space #f 0))
                   (list 35 42 (token "\"hello\"" 'string #f 0))
                   (list 42 43 (token ")" 'parenthesis '\) 0))
                   (list 43 44 (token " " 'text #f 0))
                   (list 44 45 (token "@" 'parenthesis #f 0))
                   (list 45 50 (token "print" 'symbol #f 0))
                   (list 50 51 (token "{" 'parenthesis '\{ 0))
                   (list 51 64 (token "Hello (there)" 'text #f 0))
                   (list 64 65 (token "}" 'parenthesis '\} 0))
                   (list 65 81 (token " #:not-a-keyword" 'text #f 0))))
    (check-equal? (send tm -get-content) str))

  (let* ([str "#lang racket\n(λ () #t)"]
         [tm  (test-create str)])
    (check-equal? (send tm classify 1 15)
                  (list 15 16 (token "λ" 'symbol #f 0)))
    (check-equal? (test-update! tm 2 18 0 "a")
                  '((18 19 symbol #f)))
    (check-equal? (send tm classify 2 18)
                  (list 18 19 (token "a" 'symbol #f 0))))

  (let ([o (test-create "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")])
    (check-equal? (send o get-tokens)
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 23 (token "#rx\"1234\"" 'string #f 0))
                   (list 23 24 (token "\n" 'white-space #f 0))
                   (list 24 26 (token "#(" 'parenthesis '\( 0))
                   (list 26 27 (token "1" 'constant #f 0))
                   (list 27 28 (token " " 'white-space #f 0))
                   (list 28 29 (token "2" 'constant #f 0))
                   (list 29 30 (token " " 'white-space #f 0))
                   (list 30 31 (token "3" 'constant #f 0))
                   (list 31 32 (token ")" 'parenthesis '\) 0))
                   (list 32 33 (token "\n" 'white-space #f 0))
                   (list 33 35 (token "#'" 'constant #f 0))
                   (list 35 36 (token "(" 'parenthesis '\( 0))
                   (list 36 37 (token "1" 'constant #f 0))
                   (list 37 38 (token " " 'white-space #f 0))
                   (list 38 39 (token "2" 'constant #f 0))
                   (list 39 40 (token " " 'white-space #f 0))
                   (list 40 41 (token "3" 'constant #f 0))
                   (list 41 42 (token ")" 'parenthesis '\) 0)))))

  (let ([o (test-create "#lang racket\n#<<HERE\nblah blah\nblah blah\nHERE\n")])
    (check-equal? (send o get-tokens)
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 46 (token "#<<HERE\nblah blah\nblah blah\nHERE" 'string #f 0))
                   (list 46 47 (token "\n" 'white-space #f 0)))))


  (let ()
    (define str "#lang racket\n")
    ;;           1234567890123 45678901234 567890 12345678901234567890123456
    ;;                    1          2          3          4         5
    (define tm (test-create str))
    (test-update! tm 2 14 0 "()")
    (test-update! tm 3 15 0 "d")
    (test-update! tm 4 16 0 "o")
    (test-update! tm 5 15 0 "1")
    (test-update! tm 6 16 0 "2")
    (test-update! tm 7 17 0 " ")
    (void))


  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (test-create str)])
    (test-update! tm 2 14 0 "d")
    (test-update! tm 3 15 0 "o")
    (check-equal? (send tm -get-content) "#lang racket\ndo")
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "do" 'symbol #f 0))))
    (check-equal? (dict->list (send tm -get-modes))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 16) racket-lexer))))
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (test-create str)])
    (test-update! tm 2 14 0 "1") ;initially lexed as 'constant
    (test-update! tm 3 15 0 "x") ;should re-lex "1x" as 'symbol
    (check-equal? (send tm -get-content) "#lang racket\n1x")
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "1x" 'symbol #f 0))))
    (check-equal? (dict->list (send tm -get-modes))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 16) racket-lexer))))
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (test-create str)])
    (test-update! tm 2 14 0 "1") ;initially lexed as 'constant
    (test-update! tm 3 15 0 "x") ;should re-lex "1x" as 'symbol
    (test-update! tm 4 16 0 "1") ;still symbol
    (test-update! tm 5 15 1 "")  ;deleting the "x" should re-lex the "11" as constant
    (check-equal? (send tm -get-content) "#lang racket\n11")
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "11" 'constant #f 0))))
    (check-equal? (dict->list (send tm -get-modes))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 16) racket-lexer))))
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (test-create str)])
    ;; as if paredit etc. were enabled
    (test-update! tm 2 14 0 "(")
    (test-update! tm 3 15 0 ")")
    (test-update! tm 4 15 0 "h")
    (test-update! tm 5 16 0 "i")
    (check-equal? (send tm -get-content) "#lang racket\n(hi)")
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 15 (token "(" 'parenthesis '\( 0))
                   (list 15 17 (token "hi" 'symbol #f 0))
                   (list 17 18 (token ")" 'parenthesis '\) 0))))
    (check-equal? (dict->list (send tm -get-modes))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 15) racket-lexer)
                   (cons '(15 . 17) racket-lexer)
                   (cons '(17 . 18) racket-lexer))))
  (let* ([str "#lang racket\n#hash\n"]
         ;;    1234567890123 456789
         ;;             1
         [tm (test-create str)])
    (check-equal? (send tm classify 1 14)
                  (list 14 19 (token "#hash" 'error #f 0)))
    (check-equal? (test-update! tm 2 19 0 "(")
                  '((14 20 parenthesis \())
                  "Adding parens after #hash re-lexes from an error to an open")
    (check-equal? (send tm classify 2 14)
                  (list 14 20 (token "#hash(" 'parenthesis '\( 0))))

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
    (send t start-colorer symbol->string (send o -get-lexer) (send o -get-paren-matches))
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
      (define determine-spaces (send o -get-line-indenter))
      (when determine-spaces
        (for ([pos (in-range 0 (string-length str))])
          (when (or (= pos 0)
                    (char=? (string-ref str (sub1 pos)) #\newline))
            (check-equal? (determine-spaces o pos)
                          (determine-spaces t pos)
                          (format "~v ~v in ~a" determine-spaces pos what)))))
      ;; Test range-indent.
      (define range-indent (send o -get-range-indenter))
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
                #:check-indent? #t))

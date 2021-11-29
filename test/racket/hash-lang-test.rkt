#lang racket/base

(require rackunit
         framework
         racket/class
         racket/dict
         racket/file
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
  ;; accumulates ('begin-update 'token ... 'end-update) sequences and
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
              [(cons 'lang _) (loop null)]
              [(list 'token beg end token)
               (loop (cons (list beg end (token-attribs token) (token-paren token))
                           xs))])))))
  (define (test-create str)
    (define o (new hash-lang% [on-notify
                               (λ args
                                 (async-channel-put gathering-channel args))]))
    (test-update! o 1 0 0 str)
    o)
  (define (test-update! o gen pos old-len str)
    (send o update! gen pos old-len str)
    (async-channel-get result-channel))

  ;;; Various tests of tokenizing and updatng

  (let* ([str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar"]
         ;;    1234567890123 45678901234 567890 12345678901234567890123456
         ;;             1          2          3          4         5
         [o (test-create str)])
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 15 (token 'constant #f))
                   (list 15 16 (token 'white-space #f))
                   (list 16 17 (token 'parenthesis '\())
                   (list 17 22 (token 'symbol #f))
                   (list 22 23 (token 'white-space #f))
                   (list 23 30 (token 'string #f))
                   (list 30 31 (token 'parenthesis '\)))
                   (list 31 32 (token 'white-space #f))
                   (list 32 38 (token 'symbol #f))
                   (list 38 39 (token 'parenthesis '\{))
                   (list 39 44 (token 'symbol #f))
                   (list 44 45 (token 'parenthesis '\}))
                   (list 45 46 (token 'white-space #f))
                   (list 46 47 (token 'constant #f))
                   (list 47 50 (token 'symbol #f))
                   (list 50 51 (token 'white-space #f))
                   (list 51 56 (token 'hash-colon-keyword #f))))
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
                  '((51 52 constant #f)
                    (52 55 symbol #f)))
    (check-equal? (test-update! o 3 46 4 "'bar")
                  '()
                  "Although lexeme changed from 'foo' to 'bar', the token bounds did not change nor did the type 'symbol nor the backup")

    (check-equal? (test-update! o 4 23 7 "'hell")
                  '((23 24 constant #f)
                    (24 28 symbol #f)))
    (check-equal? (test-update! o 5 13 2 "99999")
                  '((13 18 constant #f)))
    ;; Double check final result of the edits
    (check-equal? (send o -get-content)
                  "#lang racket\n99999 (print 'hell) @print{Hello} 'bar 'bar")
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 18 (token 'constant #f))
                   (list 18 19 (token 'white-space #f))
                   (list 19 20 (token 'parenthesis '\())
                   (list 20 25 (token 'symbol #f))
                   (list 25 26 (token 'white-space #f))
                   (list 26 27 (token 'constant #f))
                   (list 27 31 (token 'symbol #f))
                   (list 31 32 (token 'parenthesis '\)))
                   (list 32 33 (token 'white-space #f))
                   (list 33 39 (token 'symbol #f))
                   (list 39 40 (token 'parenthesis '\{))
                   (list 40 45 (token 'symbol #f))
                   (list 45 46 (token 'parenthesis '\}))
                   (list 46 47 (token 'white-space #f))
                   (list 47 48 (token 'constant #f))
                   (list 48 51 (token 'symbol #f))
                   (list 51 52 (token 'white-space #f))
                   (list 52 53 (token 'constant #f))
                   (list 53 56 (token 'symbol #f)))))

  (let* ([str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar"]
         [o (test-create str)])
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 19 (token 'symbol #f))
                   (list 19 20 (token 'white-space #f))
                   (list 20 22 (token 'constant #f))
                   (list 22 23 (token 'white-space #f))
                   (list 23 24 (token 'parenthesis '\())
                   (list 24 29 (token 'symbol #f))
                   (list 29 30 (token 'white-space #f))
                   (list 30 37 (token 'string #f))
                   (list 37 38 (token 'parenthesis '\)))
                   (list 38 39 (token 'white-space #f))
                   (list 39 40 (token 'parenthesis #f)) ;;??
                   (list 40 45 (token 'symbol #f))
                   (list 45 46 (token 'parenthesis '\{))
                   (list 46 59 (token 'text #f))
                   (list 59 60 (token 'parenthesis '\}))
                   (list 60 61 (token 'white-space #f))
                   (list 61 62 (token 'constant #f))
                   (list 62 65 (token 'symbol #f))
                   (list 65 66 (token 'white-space #f))
                   (list 66 71 (token 'hash-colon-keyword #f))))
    (for ([(_ mode) (in-dict (send o -get-modes))])
      (check-false (equal? mode racket-lexer)
                   "racket-lexer NOT used for #lang at-exp")
      (check-false (equal? mode racket-lexer*)
                   "racket-lexer* NOT used for #lang at-exp"))
    (check-equal? (send o -get-content) str)
    (check-equal? (send o classify 1 (sub1 (string-length str)))
                  (list 66 71 (token 'hash-colon-keyword #f))))

  (let* ([str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword"]
         [o (test-create str)])
    (check-equal? (send o get-tokens)
                  (list
                   (list 0  19 (token 'text #f))
                   (list 19 20 (token 'white-space #f))
                   (list 20 26 (token 'text #f))
                   (list 26 27 (token 'parenthesis #f)) ;;??
                   (list 27 28 (token 'parenthesis '\())
                   (list 28 33 (token 'symbol #f))
                   (list 33 34 (token 'white-space #f))
                   (list 34 41 (token 'string #f))
                   (list 41 42 (token 'parenthesis '\)))
                   (list 42 43 (token 'text #f))
                   (list 43 44 (token 'parenthesis #f))
                   (list 44 49 (token 'symbol #f))
                   (list 49 50 (token 'parenthesis '\{))
                   (list 50 63 (token 'text #f))
                   (list 63 64 (token 'parenthesis '\}))
                   (list 64 80 (token 'text #f))))
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
                  (list 14 15 (token 'symbol #f)))
    (check-equal? (test-update! o 2 17 0 "a")
                  '((17 18 symbol #f)))
    (check-equal? (send o classify 2 17)
                  (list 17 18 (token 'symbol #f))))

  (let ([o (test-create "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")])
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 22 (token 'string #f))
                   (list 22 23 (token 'white-space #f))
                   (list 23 25 (token 'parenthesis '\())
                   (list 25 26 (token 'constant #f))
                   (list 26 27 (token 'white-space #f))
                   (list 27 28 (token 'constant #f))
                   (list 28 29 (token 'white-space #f))
                   (list 29 30 (token 'constant #f))
                   (list 30 31 (token 'parenthesis '\)))
                   (list 31 32 (token 'white-space #f))
                   (list 32 34 (token 'constant #f))
                   (list 34 35 (token 'parenthesis '\())
                   (list 35 36 (token 'constant #f))
                   (list 36 37 (token 'white-space #f))
                   (list 37 38 (token 'constant #f))
                   (list 38 39 (token 'white-space #f))
                   (list 39 40 (token 'constant #f))
                   (list 40 41 (token 'parenthesis '\))))))

  (let ([o (test-create "#lang racket\n#<<HERE\nblah blah\nblah blah\nHERE\n")])
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 45 (token 'string #f))
                   (list 45 46 (token 'white-space #f)))))


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
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 15 (token 'symbol #f)))))

  (let* ([str "#lang racket\n"]
         ;;    0123456789012 3
         ;;              1
         [o (test-create str)])
    (test-update! o 2 13 0 "1") ;initially lexed as 'constant
    (test-update! o 3 14 0 "x") ;should re-lex "1x" as 'symbol
    (check-equal? (send o -get-content) "#lang racket\n1x")
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 15 (token 'symbol #f)))))
  (let* ([str "#lang racket\n"]
         ;;    0123456789012 34
         ;;              1
         [o (test-create str)])
    (test-update! o 2 13 0 "1") ;initially lexed as 'constant
    (test-update! o 3 14 0 "x") ;should re-lex "1x" as 'symbol
    (test-update! o 4 15 0 "1") ;still symbol
    (test-update! o 5 14 1 "")  ;deleting the "x" should re-lex the "11" as constant
    (check-equal? (send o -get-content) "#lang racket\n11")
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 15 (token 'constant #f)))))

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
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 14 (token 'parenthesis '\())
                   (list 14 16 (token 'symbol #f))
                   (list 16 17 (token 'parenthesis '\))))))

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
                   (list  0 12 (token 'other #f))
                   (list 12 13 (token 'white-space #f))
                   (list 13 14 (token 'parenthesis '\())
                   (list 14 16 (token 'symbol #f))
                   (list 16 17 (token 'parenthesis '\))))))

  (let* ([str "#lang racket\n#hash\n"]
         ;;    1234567890123 456789
         ;;             1
         [o (test-create str)])
    (check-equal? (send o classify 1 14)
                  (list 13 18 (token 'error #f)))
    (check-equal? (test-update! o 2 18 0 "(")
                  '((14 20 parenthesis \())
                  "Adding parens after #hash re-lexes from an error to an open")
    (check-equal? (send o classify 2 14)
                  (list 14 20 (token 'parenthesis '\())))

  (let* ([str "#lang racket\n\n(1 2)"]
         ;;    1234567890123 4 56789
         ;;             1
         [o (test-create str)])
    (check-equal? (test-update! o 2 13 0 "(")
                  '((13 14 parenthesis \())
                  "Update that splits an existing token does not produce execessive notifications.")
    (check-equal? (test-update! o 3 13 0 ")")
                  '((13 14 parenthesis \)))))

  (let* ([str "#lang racket\n\n#;(1 2)"]
         ;;    0123456789012 3 4567890
         ;;              1           2
         [o (test-create str)])
    (check-equal? (send o get-tokens)
                  (list
                   (list  0 12 (token 'other #f))
                   (list 12 14 (token 'white-space #f))
                   (list 14 16 (token 'sexp-comment #f))
                   (list 16 17 (token '#hash((comment? . #t) (type . parenthesis)) '\())
                   (list 17 18 (token '#hash((comment? . #t) (type . constant)) #f))
                   (list 18 19 (token '#hash((comment? . #t) (type . white-space)) #f))
                   (list 19 20 (token '#hash((comment? . #t) (type . constant)) #f))
                   (list 20 21 (token '#hash((comment? . #t) (type . parenthesis)) '\))))))

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
      (define line-indent (send o -get-line-indenter))
      (when line-indent
        (for ([pos (in-range 0 (string-length str))])
          (when (or (= pos 0)
                    (char=? (string-ref str (sub1 pos)) #\newline))
            (check-equal? (line-indent o pos)
                          (line-indent t pos)
                          (format "~v ~v in ~a" line-indent pos what)))))

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
     (define o (new hash-lang% [on-notify void]))
     (send o update! 1 0 0 str)

     ;; Create an object of racket:text%, which also implements the
     ;; color:text<%> interface. Since our class reads lang info to get
     ;; things like the initial lexer and paren-matches, give those
     ;; values from our object to color:text<%> `start-colorer`.
     (define t (new racket:text%))
     (send t start-colorer symbol->string (send o -get-lexer) (send o -get-paren-matches))
     (send t insert str)
     (send t freeze-colorer)
     (send t thaw-colorer)

     (define (compare what reps proc)
       (newline)
       (displayln what)
       (define o-time (cpu-time (λ () (for ([_ reps]) (proc o)))))
       (define t-time (cpu-time (λ () (for ([_ reps]) (proc t)))))
       (define factor (/ (* 1.0 o-time) t-time))
       (printf "~v ~v\n~v ~v\n~v times\n"
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

     (define line-indent (send o -get-line-indenter))
     (when line-indent
       (compare line-indent
                1
                (λ (t)
                  (for ([pos (in-range 0 (string-length str))])
                    (when (or (= pos 0)
                              (char=? (string-ref str (sub1 pos)) #\newline))
                      (line-indent t pos))))))

     (define range-indent (send o -get-range-indenter))
     (when range-indent
       (compare range-indent
                10
                (λ (t) (range-indent t 0 (string-length str))))))

   (displayln (make-string 76 #\=))

   (let* ([file "/home/greg/src/racket-lang/pkgs/racket-doc/scribblings/guide/class.scrbl"]
          [str  (file->string file)])
     (bench file str))

   (let* ([uri "https://raw.githubusercontent.com/mflatt/shrubbery-rhombus-0/master/demo.rkt"]
          [str (call/input-url (string->url uri) get-pure-port port->string)])
     (bench uri str))

   (newline)))

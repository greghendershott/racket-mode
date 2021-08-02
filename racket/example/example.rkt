;; -*- racket-indent-sequence-depth: 100; racket-indent-curly-as-sequence: t; -*-

;;; NOTE: After changing this file you will need to M-x faceup-write-file
;;; to regenerate the .faceup test comparison file.
;;;
;;; NOTE: You may need to disable certain features -- for example
;;; global-paren-face-mode -- during the M-x faceup-write-file.

#lang racket

(require xml)
(provide valid-bucket-name?)

;; Various def* forms are font-locked:

(define (function foo)
  #t)

(define ((curried-function x) y)
  (list x y))

(define a-var 10)

(define/contract (f2 x)
  (any/c . -> . any)
  #t)

(define-values (1st-var 2nd-var) (values 1 2))

(define-thing foo)  ;bug 276

;; let: font-lock identifiers

(let ([foo 10]
      [bar 20])
  foo)

(let loop ([x 10])
  (unless (zero? x)
    (loop (sub1 x))))

(let* ([foo 10]
       [bar 20])
  foo)

(let-values ([(a b) (values 1 2)])
  (values a b))

(let*-values ([(a b) (values 1 2)])
  (values a b))

(letrec-values ([(a b) (values 1 2)])
  (values a b))

(let-syntax ([foo #'foo])
  foo)

(letrec-syntax ([foo #'foo])
  foo)

(let-syntaxes ([(foo) #'foo])
  foo)

(letrec-syntaxes ([(foo) #'foo])
  foo)

(letrec-syntaxes+values ([(foo) #'foo])
                        ([(a b) (values 1 2)])
  foo)

;; for/fold is indented correctly:
(for/fold ([str ""])
          ([ss '("a" "b" "c")])
  (string-append str ss))

;; Auto-converts word `lambda` to `λ`:
(lambda (x) #t)

;; Or use M-C-y to insert to insert `λ` char.

;; Smart indentation for quoted lists:
'(1 2
  3 4)

;; Smart indentation for vector literals:
#(1 2
  3 4)

;; Smart indentation for Rackjure dict literals:
(module x rackjure
  {'a 0
   'b 2})

;; Silly test submodule example.
;; Try using C-c C-f to Fold (hide) it, and C-c C-u to Unfold it.
(module+ test
  (require rackunit)
  (check-true #t))

;; Single line comment

#|

Multi-line
comment

|#

;; Issue 362

#|aaa() |#

#|(hello)|#

#;(sexpr comment)

;; Nested sexpr comments

(list 2
      #;2)

(list 1
      #;4
      #;(3))

(let (#;[x #;1]
      [y 2])
  y)

;; Issue 388
1 ; #;
2

;; Issue 408

"#;"whatever
"#;"(whatever)
"#;"
(whatever)

;; Issue 432

#; #; 'comment-me 'comment-me 'but-not-me

#;#; 'comment-me 'comment-me 'but-not-me

#; #; #; 'comment-me 'comment-me 'comment-me 'but-not-me

#;#;#; 'comment-me 'comment-me 'comment-me 'but-not-me

#; ;; comment
;; comment
#; #| comment |#
'comment-me
'comment-me
'but-not-me


(define x #<<FOO
asdfasdf
asdfasdf
asdfasdf
FOO
  )

#;(define x #<<BAR
asdfasdf
asdfasdf
asdfasdf
BAR
    )

|identifier with spaces|

|;no comment|

| #|no comment|# |

(define (a-function x #:keyword [y 0])
  (and (append (car '(1 2 3))))
  (regexp-match? #rx"foobar" "foobar")
  (regexp-match? #px"foobar" "foobar")
  (define a 1)
  (let ([a "foo"]
        [b "bar"])
    (displayln b))
  (let* ([a "foo"]
         [b "bar"])
    (displayln b))
  (let-values ([(a b) (values 1 2)])
    #t)
  (for/list ([x (in-list (list 1 2 (list 3 4)))])
    (cond [(pair? x) (car x)]
          [else x])))

;; Issue 261
"@|widget-id|" @|foo|

;; Issue 298
(define x (begin "|" '\|))

;; Issue 376
(define || (|list|))

(define (foo)
  (let ([x 10])
    #t)

  (let ([x 1]
        [y 2])
    #t)

  (define 1/2-the-way 0)
  (define less-than-1/2 0)

  ;; Self-eval examples
  (values
   1/2-the-way                            ;should NOT be self-eval
   less-than-1/2                          ;should NOT be self-eval
   +inf.0
   -inf.0
   +nan.0
   #t
   #f
   1
   1.0
   1/2
   -1/2
   #b100
   #o123
   #d123
   #x7f7f
   'symbol
   '|symbol with spaces|
   '|;no comment|
   '| #|no comment|# |
   'symbol-with-no-alpha/numeric-chars
   #\c
   #\space
   #\newline

   ;; Literal number examples

   ;; #b
   #b1.1
   #b-1.1
   #b1e1
   #b0/1
   #b1/1
   #b1e-1
   #b101

   ;; #d
   #d-1.23
   #d1.123
   #d1e3
   #d1e-22
   #d1/2
   #d-1/2
   #d1
   #d-1

   ;; No # reader prefix -- same as #d
   -1.23
   1.123
   1e3
   1e-22
   1/2
   -1/2
   1
   -1

   ;; #e
   #e-1.23
   #e1.123
   #e1e3
   #e1e-22
   #e1
   #e-1
   #e1/2
   #e-1/2

   ;; #i always float
   #i-1.23
   #i1.123
   #i1e3
   #i1e-22
   #i1/2
   #i-1/2
   #i1
   #i-1

   ;; #o
   #o777.777
   #o-777.777
   #o777e777
   #o777e-777
   #o3/7
   #o-3/7
   #o777
   #o-777

   ;; #x
   #x-f.f
   #xf.f
   #x-f
   #xf

   ;; exact complex, e.g. issue #445
   1+2i
   1/2+3/4i
   1.0+3.0e7i

   ;; negative exponent, e.g. issue #442
   2.0e1
   -2.0e2
   -1e-1
   ))

(define/contract (valid-bucket-name? s #:keyword [dns-compliant? #t])
  ((string?) (#:keyword boolean?) . ->* . boolean?)
  (cond
    [dns-compliant?
     (and (<= 3 (string-length s)) (<= (string-length s) 63)
          (not (regexp-match #px"\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" s))
          (for/and ([s (regexp-split #rx"\\." s)])
            (define (valid-first-or-last? c)
              (or (char-lower-case? (string-ref s 0))
                  (char-numeric? (string-ref s 0))))
            (define (valid-mid? c)
              (or (valid-first-or-last? c)
                  (equal? c #\-)))
            (define len (string-length s))
            (and (< 0 len)
                 (valid-first-or-last? (string-ref s 0))
                 (valid-first-or-last? (string-ref s (sub1 len)))
                 (or (<= len 2)
                     (for/and ([c (substring s 1 (sub1 len))])
                       (valid-mid? c))))))]
    [else
     (and (<= (string-length s) 255)
          (for/and ([c s])
            (or (char-numeric? c)
                (char-lower-case? c)
                (char-upper-case? c)
                (equal? c #\.)
                (equal? c #\-)
                (equal? c #\_))))]))

(displayln "I'm running!")

;; Issue 366
#"1"
#"22"
#"333"

;; Issue 448
(fun #:1 #"a")
(fun #:12 #"a")
(fun #:123 #"a")
(fun #:1234 #"a")
(fun #:1 #px"a")
(fun #:12 #px"a")
(fun #:123 #px"a")
(fun #:1234 #px"a")

;; Issue 463
(or (equal? c #\") (equal? c #\'))
#\" #\"
#\" ;comment
#\' #\'
#\' ;comment
#\nul #\null #\backspace #\tab #\vtab #\newline #\linefeed
#\page #\return #\space #\rubout
#\012
#\uF
#\uFF
#\uFFF
#\uFFFF
#\Ufffff
#\Uffffff
#\a #\z
#\λ

;; Issue 478
(#|blah blah blah|# begin)

;; Issue 534
(define foo‾bar 42)
(let ([foo‾bar 42]) foo‾bar)

;; Issue 546
'C# (add1 1)

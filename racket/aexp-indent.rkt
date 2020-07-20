#lang racket/base

(require racket/match
         (only-in "sexp-indent.rkt"
                  [indent-amount sexp:indent-amount])
         "token-map.rkt"
         "util.rkt")

(provide indent-amount)

(module+ test (require rackunit))

(define (indent-amount tm indent-pos)
  (or (determine-spaces tm indent-pos)
      (sexp:indent-amount tm indent-pos)))

;; FIXME: Pressing RET twice will result in indented blank lines. But
;; this doesn't happen with sexp-indent. Why?
(define (determine-spaces tm indent-pos)
  (log-racket-mode-debug "determine-spaces")
  (match (backward-up tm indent-pos)
    [(? number? pos)
     (match (beg-of-@sym tm pos)
       [(? number? at-pos)
        (match (classify tm pos)
          [(bounds+token _beg open-end
                         (token:expr:open (or "{" "[") _backup _open _close))
           (define bol (beg-of-line tm open-end))
           (match (forward-sexp tm open-end)
             ;; When any sexp on same line as open { or [, indent
             ;; others with +1 the { or [, too.
             [(? number? pos)
              #:when (= bol (beg-of-line tm pos))
              (- open-end (beg-of-line tm pos))]
             ;; Else indent +2 from the @ if any.
             [_ (- (+ at-pos 2) (beg-of-line tm at-pos))])])]
       [_ #f])]
    [#f 0]))

;; Given position of { or [ token, find the associated @ if any
(define (beg-of-@sym tm pos)
  (match (classify tm (backward-sexp tm pos))
    ;; Treat @( as normal sexp
    [(bounds+token _beg _end (token:misc "@" _backup 'symbol))
     #f]
    ;; Is it optional []
    [(bounds+token beg _end (token:expr:open "[" _ _ _))
     (beg-of-@sym tm beg)]
    ;; Is it @sym(
    [(bounds+token beg end (token:misc _lexeme _backup 'symbol))
     (match (classify tm (backward-sexp tm beg))
       [(bounds+token beg end (token:misc "@" _backup 'symbol))
        beg]
       [_ #f])]
    [_ #f]))

(module+ test
  (let ([tm (create "#lang scribble/base\n@foo{\nbar\nbaz")])
    (check-equal? (indent-amount tm 27) 2)
    (check-equal? (indent-amount tm 31) 2))
  (let ([tm (create "#lang scribble/base\n@foo{bar\nbaz\nblah")])
    (check-equal? (indent-amount tm 30) 5)
    (check-equal? (indent-amount tm 34) 5))

  (let ([tm (create "#lang scribble/base\n@foo[]{\nbar\nbaz")])
    (check-equal? (indent-amount tm 29) 2)
    (check-equal? (indent-amount tm 33) 2))
  (let ([tm (create "#lang scribble/base\n@foo[]{bar\nbaz\nblah")])
    (check-equal? (indent-amount tm 32) 7)
    (check-equal? (indent-amount tm 36) 7)))

#;
(module+ test
  ;; Some tests copied/adapted from scribble/private/indentation.rkt
  (define tm1 (create "#lang scribble/base\n@f{\n @a\n@b\n}"))
  (check-equal? (determine-spaces tm1 15) #f)
  (check-equal? (determine-spaces tm1 21) #f)
  (define tm2 (create "#lang scribble/base\n@f{\n @a\n@b\n}"))
  (check-equal? (determine-spaces tm2 25) 1)
  (check-equal? (determine-spaces tm2 28) 1)
  (define tm3 (create "#lang scribble/base\n@f[@x\n@y\n]"))
  (check-equal? (determine-spaces tm3 24) #f)
  (check-equal? (determine-spaces tm3 27) 3)
  (define tm4 (create "#lang scribble/base\n@itemlist[@item{item1}\n@item{item2}\n]"))
  (check-equal? (determine-spaces tm4 22) #f)
  (check-equal? (determine-spaces tm4 44) 10)
  (define tm5 (create "#lang scribble/base\n@boldlist{@me{item1}\n@me{item2}\n}"))
  (check-equal? (determine-spaces tm5 31) #f)
  (check-equal? (determine-spaces tm5 46) 1)
  (define tm6 (create "@list{@me{item1}\n\n@me{item2}\n}"))
  (check-equal? (determine-spaces tm6 16) #f)
  (check-equal? (determine-spaces tm6 17) #f);empty line!
  (check-equal? (determine-spaces tm6 18) 1)
  (define tm7 (create "@(define (foo . a)\n(bar b))"))
  (check-equal? (determine-spaces tm7 19) #f)
  (define tm8 (create "@a{me}\n@b[\n@c{@d{e} f\ng\nh}\n"))
  ;;(check-equal? (count-parens tm8 22) 2)
  ;;(check-equal? (count-parens tm8 13) 2);;include current parenthesis
  (check-equal? (determine-spaces tm8 22) 2)
  (check-equal? (determine-spaces tm8 12) 1)
  (define tm9 (create "@a[\n(b c)\n(d\n[(e) f]\n[g h])\n]\n"))
  (check-equal? (determine-spaces tm9 13) #f)
  (check-equal? (determine-spaces tm9 4) 1)
  (define tm10 (create "#lang scribble/base\n@a{b\n  }  \n"))
  (check-equal? (determine-spaces tm10 26) 0)
  (define tm11 (create "#lang scribble/base\n@a{b\n@{\n}}\n"))
  (check-equal? (determine-spaces tm11 30) 0))

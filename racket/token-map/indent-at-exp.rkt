#lang racket/base

(require racket/match
         (only-in "indent-sexp.rkt"
                  [indent-amount sexp:indent-amount])
         "main.rkt")

(provide indent-amount)

(module+ test (require rackunit))

(define (indent-amount tm indent-pos)
  (or (determine-spaces tm indent-pos)
      (sexp:indent-amount tm indent-pos)))

(define (determine-spaces tm indent-pos)
  (match (backward-up tm indent-pos)
    [(? number? open-beg)
     (match (beg-of-at+sym tm open-beg)
       [(? number? at-pos)
        (match (classify tm open-beg)
          [(bounds+token _beg open-end
                         (token:open (and (or "{" "[") open-lexeme)
                                     _backup _close))
           (match (forward-sexp tm open-end)
             ;; When a sexp on same line as open { or [, indent
             ;; following sexps 1 column after the { or [
             [(? number? pos)
              #:when (= (beg-of-line tm open-end)
                        (beg-of-line tm pos))
              (- open-end (beg-of-line tm pos))]
             [_
              (define amount
                (match (classify tm
                                 (forward-whitespace tm
                                                     (beg-of-line tm indent-pos)))
                  ;; If the line of indent-pos is only whitespace
                  ;; followed by the close token matching open-pos,
                  ;; indent exactly with the @ column.
                  [(bounds+token _beg end (? token:close?))
                   #:when (equal? (beg-of-at+sym tm (backward-sexp tm end))
                                  at-pos)
                   0]
                  [_
                   (match open-lexeme
                               ["{" 0]
                               ["[" 1])]))
              (- (+ at-pos amount)
                 (beg-of-line tm at-pos))])])]
       [_ #f])]
    [#f 0]))

;; Given position of an open token, find the associated @ if any.
;; That is, handle cases like "@(" "@foo{", "@foo[" "@foo[]{".
(define (beg-of-at+sym tm pos)
  (match (classify tm (backward-sexp tm pos))
    ;; Treat @( as normal sexp
    [(bounds+token _beg _end (token:misc "@" _backup 'symbol))
     #f]
    ;; Did we back over {} to []? Keep going.
    [(bounds+token beg _end (token:open "[" _ _))
     (beg-of-at+sym tm beg)]
    ;; Is it @sym lexed as one token? Sometimes this happens!
    [(bounds+token beg _end (token:misc (pregexp "^@") _backup 'symbol))
     beg]
    ;; Is it @sym lexed as two tokens, @ and sym ?
    [(bounds+token beg _end (token:misc _lexeme _backup 'symbol))
     (match (classify tm (backward-sexp tm beg))
       [(bounds+token beg _end (token:misc "@" _backup 'symbol))
        beg]
       [_ #f])]
    [_ #f]))

(module+ test
  (let ([tm (create "#lang scribble/base\n@foo{\nbar\nbaz\n}")])
    (check-equal? (indent-amount tm 27) 0)
    (check-equal? (indent-amount tm 31) 0)
    (check-equal? (indent-amount tm 35) 0))
  (let ([tm (create "#lang scribble/base\n@foo{bar\nbaz\nblah\n}")])
    (check-equal? (indent-amount tm 30) 5)
    (check-equal? (indent-amount tm 34) 5)
    (check-equal? (indent-amount tm 39) 5))

  (let ([tm (create "#lang scribble/base\n@foo[\nbar\nbaz\n]")])
    (check-equal? (indent-amount tm 27) 1)
    (check-equal? (indent-amount tm 31) 1)
    (check-equal? (indent-amount tm 35) 0))
  (let ([tm (create "#lang scribble/base\n@foo[bar\nbaz\nblah\n]")])
    (check-equal? (indent-amount tm 30) 5)
    (check-equal? (indent-amount tm 34) 5)
    (check-equal? (indent-amount tm 39) 0))

  (let ([tm (create "#lang scribble/base\n@foo[]{\nbar\nbaz")])
    (check-equal? (indent-amount tm 29) 0)
    (check-equal? (indent-amount tm 33) 0))
  (let ([tm (create "#lang scribble/base\n@foo[]{bar\nbaz\nblah")])
    (check-equal? (indent-amount tm 32) 7)
    (check-equal? (indent-amount tm 36) 7))

  (let ([tm (create "#lang scribble/base\n@defstruct[traverse-block ([traverse block-traverse-procedure/c])]{\nfoo\n}")])
    (check-equal? (indent-amount tm 92) 0)
    (check-equal? (indent-amount tm 93) 0)))

;; The following tests adapted from scribble/private/indentation.rkt.
;; These tests mostly don't agree with my implementation, but either
;; (a) it is a difference in returning #f for the sexp indenter to
;; give the same, expected result so AFAICT N/A, or (b) my
;; implementation seems to match real-world practice including
;; ironically scribble's core.scrbl doc. For now I am leaving these
;; tests commented out.
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

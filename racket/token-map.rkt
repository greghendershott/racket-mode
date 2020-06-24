#lang racket/base

(require (only-in data/interval-map
                  make-interval-map
                  interval-map-set!
                  interval-map-ref
                  interval-map-remove!)
         racket/match
         syntax-color/module-lexer)

(provide token-map?
         create
         update
         tokens
         classify
         (struct-out token)
         (struct-out token:expr)
         (struct-out token:expr:open)
         (struct-out token:expr:close)
         (struct-out token:misc)
         beginning-of-line
         end-of-line
         backward-up
         forward-whitespace/comment
         backward-whitespace/comment
         forward-sexp
         backward-sexp)

(struct token-map ([str #:mutable] im) #:transparent)

;; Because interval-map-ref/bounds is not available in older Racket,
;; we redundantly store the interval in the value as the beg end
;; members.
(struct token (beg end backup) #:transparent)
(struct token:expr token (open close) #:transparent)
(struct token:expr:open token:expr () #:transparent)
(struct token:expr:close token:expr () #:transparent)
(struct token:misc token (kind) #:transparent)

(define (create s)
  (define im (make-interval-map))
  (tokenize-string im s 0 #f)
  (token-map s im))

(define (update tm pos old-len after)
  (match-define (token-map old-str im) tm)
  (define str (string-append (substring old-str 0 pos)
                             after
                             (substring old-str
                                        (+ pos old-len)
                                        (string-length old-str))))
  (set-token-map-str! tm str)
  (define start (match (interval-map-ref im pos #f)
                  [(token beg _end backup) (- beg backup)]
                  [_                       pos]))
  (interval-map-remove! im start +inf.0)
  (tokenize-string im str start #f)
  start)

(define (classify tm pos)
  (interval-map-ref (token-map-im tm) pos))

(define (tokens tm pos [proc values])
  (match (interval-map-ref (token-map-im tm) pos #f)
    [(? token? t) (cons (proc t)
                        (tokens tm (token-end t) proc))]
    [#f '()]))

(define (tokenize-string im str offset [mode #f])
  (define in (open-input-string str))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (tokenize-port im in offset mode))

(define (tokenize-port im in offset mode)
  (define-values (lexeme kind delimit beg end backup new-mode)
    (module-lexer in offset mode))
  (cond [(eof-object? lexeme)
         im]
        [(eq? kind 'white-space)
         (handle-white-space-token im lexeme beg end backup)
         (tokenize-port im in end new-mode)]
        [(eq? kind 'parenthesis)
         (handle-parenthesis-token im lexeme mode delimit beg end backup)
         (tokenize-port im in end new-mode)]
        [else
         (interval-map-set! im beg end (token:misc beg end backup kind))
         (tokenize-port im in end new-mode)]))

;; It is useful for the token map to handle end-of-line white-space
;; distinctly. This helps for things like indenters.
(define (handle-white-space-token im lexeme beg end backup)
  (let loop ([lexeme lexeme]
             [beg beg]
             [end end]
             [backup backup])
    (match lexeme
      [(pregexp "^\r\n")
       (interval-map-set! im
                          beg (+ 2 beg)
                          (token:misc beg (+ 2 beg) backup 'end-of-line))
       (loop (substring lexeme 2)
             (+ 2 beg)
             end
             (+ backup 2))]
      [(pregexp "^[\r\n]")
       (interval-map-set! im beg (add1 beg)
                          (token:misc beg (add1 beg) backup 'end-of-line))
       (loop (substring lexeme 1)
             (add1 beg)
             end
             (add1 backup))]
      [(pregexp "^([^\r\n]+)" (list _ s))
       (define len (string-length s))
       (interval-map-set! im beg (+ beg len)
                          (token:misc beg (+ beg len) backup 'white-space))
       (loop (substring lexeme len)
             (+ beg len)
             end
             (+ backup len))]
      [_ (void)])))

;; Handling "parentheses" well helps in various ways:
;;
;; 1. Editor commands to navigate and modify sub-expressions or
;;    "blocks". Things like backward-up-list, forward-sexp, and things
;;    depending on those like paredit. This is obvious in a sexpr
;;    lang, but can also work in C-like langs that use {}, langs that
;;    use keywords like "beging" and "end", and even langs that use
;;    indent (either as a shorthand for {} e.g. Haskell, or not e.g.
;;    Python).
;;
;; 2. A lang-provided indenter can use this information.
;;
;; STATUS QUO
;;
;; I find the status quo lexer/c handling of this to be not quite
;; right: `delimit` is one of the chars "()[]{}", and, it simply
;; "recapitulates" information from the lexeme. I'm guessing the
;; justification for this was that e.g. racket-lexer handles "#(" as a
;; single 2-char token, parenthesis "(". [However it handles "#'(" as
;; 2 tokens -- a 2-char "#'" constant followed by a 1-char parenthesis
;; "(". So...???]
;;
;; color-text<%> start-colorer accepts a "pairs" list e.g. '((|(| |)|)
;; (|[| |]|) (begin end)). BUT. Where does this value come from?? It
;; does NOT come from the lexer which is supplied by the lang's
;; get-info. So, this seems like something "bolted on" at the DrRacket
;; level that the user must configure -- whereas it should be
;; specified by the lang. The only example I can find of something
;; calling start-colorer with pairs, among all Racket organization
;; repos on GitHub, is "ProfessorJ":
;; <https://github.com/racket/old-plt/blob/a580d75deae2a0d2f3d8a93bc3c4f8f1f619b5b7/collects/profj/tool.ss#L762>.
;; So the paradigm here seems to be "A 'language' is drscheme:tool
;; code loaded by DrRacket's Choose Language command" -- as opposed to
;; "a module language entirely determined by #lang and get-info", I
;; think? If so, this makes it unsuitable for use outside DrRacket.
;;
;; INSTEAD
;;
;; 1. I suggest instead of "parenthesis" tokens, a lexer should return
;;    "open" and "close" tokens. Instead of a 'delimit' field as in
;;    the status quo (which mostly just recapitulates information in
;;    the 'lexeme' field) there is a field stating the opposite
;;    matching token. An open token says what it's matching close
;;    token is, and vice versa. That way a lang could specify
;;    expression/block delimiters like "begin" and "end", its lexer
;;    could emit tokens like (open lexeme:"begin" opposite:"end") and
;;    (close lexeme:"end" opposite:"begin"), and a consuming indenter
;;    or editor can use these effectively. (I think this will work
;;    even if multiple open tokens share the same end token -- like
;;    when "then" and "else" both use "end", as opposed to "elif" and
;;    "end" -- right?) (For off-side rule lexers, presumably indent =
;;    open and dedent = close, and the "opposite" value is N/A?)
;;
;; 2. Also, some languages (at least lisps) might want to have a token
;;    type of "expression prefix" -- such as for reader shorthands
;;    like ' and #'. Currently racket-lexer classifies these as
;;    "symbol", which makes sense as they are shortand for symbols
;;    like "quote" and "syntax". At the same time, end user sexpr
;;    navigation wants to treat the first character of those prefixes
;;    as the start of the sexpr, not the open paren. Likewise, indent
;;    usually wants to align with that first character, not the open
;;    paren. [Alternatively, I suppose a lexer could return "'(" or
;;    "#hasheq(" as as single open token. That means e.g. Emacs could
;;    not use char-syntax, but, we could provide our own
;;    forward-sexp-function that understands these, I think.]
;;
;; MEANWHILE: The following function attempts to "normalize" the
;; status quo "legacy" lexer protocol.
(define (handle-parenthesis-token im lexeme mode delimit beg end backup)
  (define lexer-name (object-name (mode->lexer mode)))
  (define tok
    (match delimit
      ['|(| (token:expr:open beg end backup "(" ")")]
      ['|[| (token:expr:open beg end backup "[" "]")]
      ['|{| (token:expr:open beg end backup "{" "}")]
      ['|)| (token:expr:close beg end backup "(" ")")]
      ['|]| (token:expr:close beg end backup "[" "]")]
      ['|}| (token:expr:close beg end backup "{" "}")]
      [_
       (eprintf "unexpected 'parenthesis token with delimit = ~v and lexeme = ~v\n"
                delimit lexeme)
       (match lexeme
         ;; Defensive:
         ["(" (token:expr:open beg end backup "(" ")")]
         [")" (token:expr:close beg end backup "(" ")")]
         ;; I have seen this with at-exp and scribble/text. No
         ;; idea why. I guess treat it as 'symbol, like how "'"
         ;; is lexed??
         ["@"
          #:when (memq lexer-name '(scribble-lexer
                                    scribble-inside-lexer))
          (token:misc beg end backup 'symbol)]
         ;; Super-defensive WTF:
         [_  (token:misc beg end backup 'other)])]))
  ;; In Emacs, the standard char-syntax stuff is just that --
  ;; single chars. As a result it's helpful here to return
  ;; tokens for parens that are indeed single chars. Any
  ;; "excess" chars should be constant or perhaps Emacs 'prefix
  ;; syntax-class. The latter would be ideal for Racket Mode
  ;; and sexpr langs, but I'm not sure how clever to get here
  ;; b/c it could bake in some assumption that is /not/ good
  ;; for non-sepxr langs. So for now just accept "constant" for
  ;; "'", "#", or "#'". (In Emacs we also have the option of
  ;; not using char-syntax for these, at all, and instead
  ;; supplying forward-sexp-function that calls the back end.)
  ;;
  ;; Weirdly, racket-lexer treats "#(" as a 2-char open paren
  ;; token -- but "#'(" as a 2-char constant followed by a
  ;; 1-char open paren. Handle the former like the latter, here
  ;; (so that Emacs front end can rely on the
  ;; syntax-propertization).
  (cond [(and (memq lexer-name '(racket-lexer
                                 racket-lexer/status
                                 racket-nobar-lexer/status
                                 scribble-inside-lexer
                                 scribble-lexer))
              (token:expr:open? tok)
              (< 1 (- end beg)))
         (interval-map-set! im beg (sub1 end)
                            (token:misc beg (sub1 end) backup 'constant))
         (interval-map-set! im (sub1 end) end
                            (token:expr:open (sub1 end) end backup
                                             (token:expr-open tok)
                                             (token:expr-close tok)))]
        [else
         (interval-map-set! im beg end tok)])  )

(define (mode->lexer mode)
  (match mode
    [(? procedure? p)          p]
    [(cons (? procedure? p) _) p]
    [#f                       #f]))

;;; "Navigation": Useful for a lang indenter -- these roughly
;;; correspond to methods from text<%> that an indenter might need --
;;; as well as an end user text editor. Note that these work in terms
;;; of open and close tokens -- not necessarily traditional lisp
;;; s-expressions.

(define (beginning-of-line tm pos)
  (define im  (token-map-im tm))
  (let loop ([pos pos])
    (match (interval-map-ref im pos #f)
      [(? token:misc? t)
       #:when (eq? (token:misc-kind t) 'end-of-line)
       (token-end t)]
      [(? token? t)
       (loop (sub1 (token-beg t)))]
      [#f 1])))

(define (end-of-line tm pos)
  (define im (token-map-im tm))
  (let loop ([pos pos])
    (match (interval-map-ref im pos #f)
      [(? token:misc? t)
       #:when (eq? (token:misc-kind t) 'end-of-line)
       (token-end t)]
      [(? token? t)
       (loop (token-end t))]
      [#f (add1 (string-length (token-map-str tm)))])))

(define (backward-up tm pos)
  (define im (token-map-im tm))
  (let loop ([pos pos]
             [ht (hash)])
    (match (interval-map-ref im pos #f)
      [#f #f]
      [(? token:expr:close? t)
       (loop (sub1 (token-beg t))
             (hash-update ht (token:expr-open t) add1 0))]
      [(? token:expr:open? t)
       (if (zero? (hash-ref ht (token:expr-open t) 0))
           (token-beg t)
           (loop (sub1 (token-beg t))
                 (hash-update ht (token:expr-open t)
                              sub1
                              0)))]
      [(? token? t)
       (loop (sub1 (token-beg t))
             ht)])))

(define (forward-whitespace/comment tm pos)
  (define im (token-map-im tm))
  (match (interval-map-ref im pos #f)
    [#f #f]
    [(token:misc _beg end _backup (or 'end-of-line
                                      'white-space
                                      'comment
                                      'sexp-comment))
     (forward-whitespace/comment tm end)]
    [_ pos]))

(define (backward-whitespace/comment tm pos)
  (define im (token-map-im tm))
  (match (interval-map-ref im pos #f)
    [#f #f]
    [(token:misc beg _end _backup (or 'end-of-line
                                      'white-space
                                      'comment
                                      'sexp-comment))
     (forward-whitespace/comment tm (sub1 beg))]
    [_ pos]))

(define (forward-sexp tm pos)
  (match (forward-whitespace/comment tm pos)
    [#f #f]
    [pos
     (define im (token-map-im tm))
     (match (interval-map-ref im pos #f)
       [#f #f]
       ;; Open token: Scan for matching close token.
       [(? token:expr:open? open-t)
        (let loop ([pos pos]
                   [depth 0])
          ;;(println (list pos depth (interval-map-ref im pos)))
          (match (interval-map-ref im pos)
            [(? token:expr:open? t)
             #:when (equal? (token:expr-open open-t)
                            (token:expr-open t))
             (loop (token-end t)
                   (add1 depth))]
            [(? token:expr:close? t)
             #:when (equal? (token:expr-open open-t)
                            (token:expr-open t))
             (if (= depth 1)
                 (token-end t)
                 (loop (token-end t)
                       (sub1 depth)))]
            [(? token? t)
             (loop (token-end t)
                   depth)]))]
       ;; Some other non-white-space/comment token. Simply use last
       ;; char pos.
       [(? token? t)
        (token-end t)])]))

(define (backward-sexp tm pos)
  (match (backward-whitespace/comment tm pos)
    [#f #f]
    {pos
     (define im (token-map-im tm))
     (match (interval-map-ref im pos)
      ;; Close token: Scan for matching open token.
      [(? token:expr:close? close-t)
       (let loop ([pos pos]
                  [depth 0])
         ;;(println (list pos depth (interval-map-ref im pos)))
         (match (interval-map-ref im pos)
           [(? token:expr:open? t)
            #:when (equal? (token:expr-open close-t)
                           (token:expr-open t))
            (if (= depth 1)
                (token-beg t)
                (loop (sub1 (token-beg t))
                      (sub1 depth)))]
           [(? token:expr:close? t)
            #:when (equal? (token:expr-open close-t)
                           (token:expr-open t))
            (loop (sub1 (token-beg t))
                  (add1 depth))]
           [(? token? t)
            (loop (sub1 (token-beg t))
                  depth)]))]
      ;; Some other token. Simply use first char pos.
      [(? token? t)
       (token-beg t)])}))

(module+ test
  (require rackunit)
  (require racket/pretty)
  (define str "#lang racket\n(a (b (c  foo))) (bar ((x)) y)")
  ;;           1234567890123 456789012345678901234567890123
  ;;                    1          2         3         4
  (define tm (create str))
  (pretty-print tm)
  (check-equal? (beginning-of-line tm 1) 1)
  (check-equal? (beginning-of-line tm 2) 1)
  (check-equal? (beginning-of-line tm 3) 1)
  (check-equal? (beginning-of-line tm 14) 14)
  (check-equal? (beginning-of-line tm 15) 14)
  (check-equal? (backward-up tm 14) 14)
  (check-equal? (backward-up tm 16) 14)
  (check-equal? (backward-up tm 17) 17)
  (check-equal? (backward-up tm 18) 17)
  (check-equal? (backward-up tm 20) 20)
  (check-equal? (backward-up tm 22) 20)
  (check-equal? (backward-up tm 31) 31)
  (check-equal? (backward-up tm 34) 31)
  (check-equal? (backward-up tm 42) 31)
  (check-false  (backward-up tm  1))
  (check-false  (backward-up tm 12))
  (check-false  (backward-up tm 13))
  (check-false  (backward-up tm 30))
  (check-false  (backward-up tm 43))
  (check-equal? (forward-whitespace/comment tm 23) 24)
  (check-equal? (backward-whitespace/comment tm 22) 21)
  (check-equal? (forward-sexp tm 24) 27)
  (check-equal? (forward-sexp tm 14) 30)
  (check-equal? (backward-sexp tm 26) 24)
  (check-equal? (backward-sexp tm 29) 14))

(module+ example-0
  (require racket/pretty)
  (define str "#lang racket
               42
               (print \"hello\")
               @print{Hello}
               'foo #:bar")
  (define tm (create str))
  (pretty-print tm)
  (pretty-print (update tm 25 1 "J"))
  (pretty-print tm)
  (pretty-print (classify tm (sub1 (string-length str)))))

(module+ example-1
  (require racket/pretty)
  (define str "#lang at-exp racket
               42
               (print \"hello\")
               @print{Hello (there)}
               'foo #:bar")
  (define tm (create str))
  (pretty-print tm)
  (pretty-print (classify tm (sub1 (string-length str)))))

(module+ example-2
  (require racket/pretty)
  (define str "#lang scribble/text
               Hello
               @(print \"hello\")
               @print{Hello (there)}
               #:not-a-keyword")
  (define tm (create str))
  (pretty-print tm)
  (pretty-print (classify tm (sub1 (string-length str)))))

(module+ example-3
  (require racket/pretty)
  (define str "#lang racket\n(λ () #t)")
  (define tm (create str))
  (pretty-print tm)
  (pretty-print (classify tm 14))
  (pretty-print (classify tm (sub1 (string-length str)))))

(module+ example-4
  (require racket/pretty)
  (define str "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")
  (define tm (create str))
  (pretty-print tm))

(module+ example-heredoc
  (require racket/pretty)
  (define str "#lang racket\n#<<HERE\nblah blah\nblah blah\nHERE\n")
  (define tm (create str))
  (pretty-print tm))

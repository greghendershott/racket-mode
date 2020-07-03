#lang racket/base

(require (only-in data/interval-map
                  make-interval-map
                  interval-map-set!
                  interval-map-ref
                  interval-map-ref/bounds
                  interval-map-remove!
                  interval-map-expand!
                  interval-map-contract!)
         racket/contract
         racket/dict
         racket/match
         syntax-color/module-lexer
         "util.rkt")

(provide token-map?
         create
         update
         tokens
         classify
         token-text
         (struct-out bounds+token)
         (struct-out token)
         (struct-out token:expr)
         (struct-out token:expr:open)
         (struct-out token:expr:close)
         (struct-out token:misc)
         beg-of-line
         end-of-line
         backward-up
         forward-whitespace
         forward-whitespace/comment
         backward-whitespace/comment
         forward-sexp
         backward-sexp)

(struct token-map ([str #:mutable]
                   tokens ;interval-map
                   modes) ;interval-map
  #:transparent)

(struct bounds+token (beg end token) #:transparent)
(struct token (lexeme backup) #:transparent)
(struct token:expr token (open close) #:transparent)
(struct token:expr:open token:expr () #:transparent)
(struct token:expr:close token:expr () #:transparent)
(struct token:misc token (kind) #:transparent)

(define (token-map-ref tm pos)
  (define-values (beg end token)
    (interval-map-ref/bounds (token-map-tokens tm) pos #f))
  (and beg end token
       (bounds+token beg end token)))

(define current-set-interval (make-parameter interval-map-set!))
(define (set-interval tokens beg end token)
  ((current-set-interval) tokens beg end token))

(define (create s)
  (define tokens (make-interval-map))
  (define modes  (make-interval-map))
  (tokenize-string tokens modes s 1)
  (token-map s tokens modes))

(define/contract (update tm pos old-len str)
  (-> token-map? exact-positive-integer? exact-nonnegative-integer? string? any)
  (unless (<= (sub1 pos) (string-length (token-map-str tm)))
    (raise-argument-error 'update "valid position" 1 tm pos old-len str))
  (cond [(and (zero? old-len) (equal? str ""))
         (list)] ;no-op
        [else
         (set-token-map-str! tm
                             (string-append (substring (token-map-str tm) 0 (sub1 pos))
                                            str
                                            (substring (token-map-str tm)
                                                       (+ (sub1 pos) old-len))))
         ;;(log-racket-mode-debug "~v" (token-map-str tm))
         (define beg (match (token-map-ref tm pos)
                       [(bounds+token beg _end (token _ backup)) (- beg backup)]
                       [_                                        pos]))
         (define diff (- (string-length str) old-len))
         (define tokens (token-map-tokens tm))
         (define modes  (token-map-modes tm))
         (cond [(< 0 diff) (interval-map-expand!   tokens pos (+ pos diff))
                           (define orig-mode (interval-map-ref modes pos #f))
                           (interval-map-expand!   modes  beg (+ pos diff))
                           (interval-map-set!      modes  beg (+ pos diff) orig-mode)]
               [(< diff 0) (interval-map-contract! tokens pos (- pos diff))
                           (interval-map-contract! modes  beg (- pos diff))])
         (define old-tokens (make-interval-map
                             (for/list ([(k v) (in-dict tokens)])
                               (cons k v))))
         (define updated-intervals '())
         (define (set-interval/update tokens beg end token)
           (define-values (old-beg old-end old-token)
             (interval-map-ref/bounds old-tokens
                                      (- beg diff)
                                      #f))
           (cond [(and old-beg old-end old-token
                       (= old-beg (- beg diff))
                       (= old-end (- end diff))
                       (equal? old-token token))
                  (log-racket-mode-debug
                   "Stopping because no change except shift; diff=~v old=~v new=~v"
                   diff
                   (list old-beg old-end old-token)
                   (list beg end token))
                  #f] ;stop
                 [else
                  (set! updated-intervals (cons (bounds+token beg end token)
                                                updated-intervals))
                  (interval-map-set! tokens beg end token)
                  #t])) ;continue
         (parameterize ([current-set-interval set-interval/update])
           (tokenize-string tokens modes (token-map-str tm) beg))
         (reverse updated-intervals)]))

(define (classify tm pos)
  (token-map-ref tm pos))

(define (token-text tm pos)
  (match (token-map-ref tm pos)
    [(bounds+token _beg _end (token lexeme _)) lexeme]
    [#f #f]))

(define (tokens tm beg end [proc values])
  (match (token-map-ref tm beg)
    [(? bounds+token? b+t)
     #:when (< (bounds+token-beg b+t) end)
     (cons (proc b+t)
           (tokens tm (bounds+token-end b+t) end proc))]
    [_ '()]))

(define (tokenize-string tokens modes str beg)
  (define in (open-input-string str))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (let loop ()
    (define-values (_line _col offset) (port-next-location in))
    (unless (= offset beg)
      (read-byte-or-special in)
      (loop)))
  (tokenize-port tokens modes in beg (interval-map-ref modes beg #f)))

(define (tokenize-port tokens modes in offset mode)
  (define-values (lexeme kind delimit beg end backup new-mode)
    (module-lexer in offset mode))
  ;;(println (list offset mode lexeme kind delimit beg end backup new-mode))
  (unless (eof-object? lexeme)
    (interval-map-set! modes beg end mode)
    (when (case kind
            [(white-space) (handle-white-space-token tokens lexeme beg end backup)]
            [(parenthesis) (handle-parenthesis-token tokens lexeme mode delimit beg end backup)]
            [else          (set-interval tokens beg end (token:misc lexeme backup kind))])
      (tokenize-port tokens modes in end new-mode))))

;; It is useful for the token map to handle end-of-line white-space
;; distinctly. This helps for things like indenters.
(define (handle-white-space-token im lexeme beg end backup)
  (let loop ([lexeme lexeme]
             [beg beg]
             [end end]
             [backup backup])
    (match lexeme
      [(pregexp "^\r\n")
       (set-interval im
                     beg (+ 2 beg)
                     (token:misc lexeme backup 'end-of-line))
       (loop (substring lexeme 2)
             (+ 2 beg)
             end
             (+ backup 2))]
      [(pregexp "^[\r\n]")
       (set-interval im beg (add1 beg)
                     (token:misc lexeme backup 'end-of-line))
       (loop (substring lexeme 1)
             (add1 beg)
             end
             (add1 backup))]
      [(pregexp "^([^\r\n]+)" (list _ s))
       (define len (string-length s))
       (set-interval im beg (+ beg len)
                     (token:misc lexeme backup 'white-space))
       (loop (substring lexeme len)
             (+ beg len)
             end
             (+ backup len))]
      [_ #t])))

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
      ['|(| (token:expr:open lexeme backup "(" ")")]
      ['|[| (token:expr:open lexeme backup "[" "]")]
      ['|{| (token:expr:open lexeme backup "{" "}")]
      ['|)| (token:expr:close lexeme backup "(" ")")]
      ['|]| (token:expr:close lexeme backup "[" "]")]
      ['|}| (token:expr:close lexeme backup "{" "}")]
      [_
       (log-racket-mode-warning
        "unexpected 'parenthesis token with delimit = ~v and lexeme = ~v\n"
        delimit lexeme)
       (match lexeme
         ;; Defensive:
         ["(" (token:expr:open lexeme backup "(" ")")]
         [")" (token:expr:close lexeme backup "(" ")")]
         ;; I have seen this with at-exp and scribble/text. No
         ;; idea why. I guess treat it as 'symbol, like how "'"
         ;; is lexed??
         ["@"
          #:when (memq lexer-name '(scribble-lexer
                                    scribble-inside-lexer))
          (token:misc lexeme backup 'symbol)]
         ;; Super-defensive WTF:
         [_  (token:misc lexeme backup 'other)])]))
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
         (set-interval im beg (sub1 end)
                       (token:misc lexeme backup 'constant))
         (set-interval im (sub1 end) end
                       (token:expr:open lexeme backup
                                        (token:expr-open tok)
                                        (token:expr-close tok)))]
        [else
         (set-interval im beg end tok)])  )

(define (mode->lexer mode)
  (match mode
    [(? procedure? p)          p]
    [(cons (? procedure? p) _) p]
    [v                         v]))

;;; "Navigation": Useful for a lang indenter -- these roughly
;;; correspond to methods from text<%> that an indenter might need --
;;; as well as an end user text editor. Note that these work in terms
;;; of open and close tokens -- not necessarily traditional lisp
;;; s-expressions.

(define (beg-of-line tm pos)
  (let loop ([pos pos])
    (match (token-map-ref tm pos)
      [(bounds+token _beg end (? token:misc? t))
       #:when (eq? (token:misc-kind t) 'end-of-line)
       end]
      [(bounds+token beg _end (? token? t))
       (loop (sub1 beg))]
      [#f 1])))

(define (end-of-line tm pos)
  (let loop ([pos pos])
    (match (token-map-ref tm pos)
      [(bounds+token _beg end (? token:misc? t))
       #:when (eq? (token:misc-kind t) 'end-of-line)
       end]
      [(bounds+token _beg end (? token? t))
       (loop end)]
      [#f (add1 (string-length (token-map-str tm)))])))

(define (backward-up tm pos)
  (let loop ([pos pos]
             [ht (hash)])
    (match (token-map-ref tm pos)
      [#f #f]
      [(bounds+token beg end (? token:expr:close? t))
       (loop (sub1 beg)
             (hash-update ht (token:expr-open t) add1 0))]
      [(bounds+token beg end (? token:expr:open? t))
       (if (zero? (hash-ref ht (token:expr-open t) 0))
           beg
           (loop (sub1 beg)
                 (hash-update ht (token:expr-open t)
                              sub1
                              0)))]
      [(bounds+token beg end (? token? t))
       (loop (sub1 beg)
             ht)])))

(define (forward-whitespace tm pos)
  (match (token-map-ref tm pos)
    [#f #f]
    [(bounds+token beg end (token:misc _lexeme _backup 'white-space))
     (forward-whitespace tm end)]
    [_ pos]))

(define (forward-whitespace/comment tm pos)
  (match (token-map-ref tm pos)
    [#f #f]
    [(bounds+token beg end (token:misc _lexeme _backup (or 'end-of-line
                                                            'white-space
                                                            'comment
                                                            'sexp-comment)))
     (forward-whitespace/comment tm end)]
    [_ pos]))

(define (backward-whitespace/comment tm pos)
  (match (token-map-ref tm pos)
    [#f #f]
    [(bounds+token beg end (token:misc _lexme _backup (or 'end-of-line
                                                          'white-space
                                                          'comment
                                                          'sexp-comment)))
     (forward-whitespace/comment tm (sub1 beg))]
    [_ pos]))

(define (forward-sexp tm pos)
  (match (forward-whitespace/comment tm pos)
    [#f #f]
    [pos
     (match (token-map-ref tm pos)
       [#f #f]
       ;; Open token: Scan for matching close token.
       [(bounds+token beg end (? token:expr:open? open-t))
        (let loop ([pos pos]
                   [depth 0])
          ;;(println (list pos depth (interval-map-ref im pos)))
          (match (token-map-ref tm pos)
            [(bounds+token beg end (? token:expr:open? t))
             #:when (equal? (token:expr-open open-t)
                            (token:expr-open t))
             (loop end
                   (add1 depth))]
            [(bounds+token beg end (? token:expr:close? t))
             #:when (equal? (token:expr-open open-t)
                            (token:expr-open t))
             (if (= depth 1)
                 end
                 (loop end
                       (sub1 depth)))]
            [(bounds+token beg end (? token? t))
             (loop end
                   depth)]))]
       ;; Some other non-white-space/comment token. Simply use last
       ;; char pos.
       [(bounds+token beg end (? token? t))
        end])]))

(define (backward-sexp tm pos)
  (match (backward-whitespace/comment tm pos)
    [#f #f]
    {pos
     (match (token-map-ref tm pos)
      ;; Close token: Scan for matching open token.
      [(bounds+token beg end (? token:expr:close? close-t))
       (let loop ([pos pos]
                  [depth 0])
         ;;(println (list pos depth (interval-map-ref im pos)))
         (match (token-map-ref tm pos)
           [(bounds+token beg end (? token:expr:open? t))
            #:when (equal? (token:expr-open close-t)
                           (token:expr-open t))
            (if (= depth 1)
                beg
                (loop (sub1 beg)
                      (sub1 depth)))]
           [(bounds+token beg end (? token:expr:close? t))
            #:when (equal? (token:expr-open close-t)
                           (token:expr-open t))
            (loop (sub1 beg)
                  (add1 depth))]
           [(bounds+token beg end (? token? t))
            (loop (sub1 beg)
                  depth)]))]
      ;; Some other token. Simply use first char pos.
      [(bounds+token beg end (? token? t))
       beg])}))

(module+ test
  (require rackunit)
  (require racket/pretty)
  (define str "#lang racket\n(a (b (c  foo))) (bar ((x)) y)")
  ;;           1234567890123 456789012345678901234567890123
  ;;                    1          2         3         4
  (define tm (create str))
  (pretty-print tm)
  (check-equal? (classify tm 1)
                (bounds+token 1 13 (token:misc "#lang racket" 0 'other)))
  (check-equal? (classify tm 13)
                (bounds+token 13 14 (token:misc "\n" 0 'end-of-line)))
  (check-equal? (beg-of-line tm 1) 1)
  (check-equal? (beg-of-line tm 2) 1)
  (check-equal? (beg-of-line tm 3) 1)
  (check-equal? (beg-of-line tm 14) 14)
  (check-equal? (beg-of-line tm 15) 14)
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
  (define str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar")
  ;;           1234567890123 45678901234 567890 12345678901234567890123456
  ;;                    1          2          3          4         5
  (define tm (create str))
  tm
  (update tm 52 5 "'bar")
  (update tm 47 4 "'bar")
  (update tm 24 7 "'hell")
  (update tm 14 2 "99999")
  tm)

(module+ example-1
  (define str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar")
  (define tm (create str))
  tm
  (classify tm (sub1 (string-length str))))

(module+ example-2
  (define str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword")
  (define tm (create str))
  tm
  (classify tm (sub1 (string-length str))))

(module+ example-3
  (define str "#lang racket\n(λ () #t)")
  (define tm (create str))
  tm
  (classify tm 14)
  (classify tm (sub1 (string-length str))))

(module+ example-4
  (define str "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")
  (create str))

(module+ example-heredoc
  (define str "#lang racket\n#<<HERE\nblah blah\nblah blah\nHERE\n")
  (create str))

(module+ example-99
  (define str "#lang racket\n")
  ;;           1234567890123 45678901234 567890 12345678901234567890123456
  ;;                    1          2          3          4         5
  (define tm (create str))
  tm
  (update tm 14 0 "()")
  (update tm 15 0 "d")
  tm
  (update tm 16 0 "o")
  (update tm 15 0 "1")
  (update tm 16 0 "2")
  (update tm 17 0 " ")
  tm)

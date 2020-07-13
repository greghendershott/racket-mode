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

(module+ test (require rackunit))

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

;; Given an update to the string, re-tokenize and return a minimal
;; list of changes, doing mimimal work.
(define/contract (update tm pos old-len str)
  (-> token-map? exact-positive-integer? exact-nonnegative-integer? string? any)
  (unless (<= (sub1 pos) (string-length (token-map-str tm)))
    (raise-argument-error 'update "valid position" 1 tm pos old-len str))
  [if (and (zero? old-len) (equal? str ""))
      (list) ;no-op fast path
      (update* tm pos old-len str)])

(define (update* tm pos old-len str)
  (set-token-map-str! tm
                      (string-append (substring (token-map-str tm) 0 (sub1 pos))
                                     str
                                     (substring (token-map-str tm)
                                                (+ (sub1 pos) old-len))))
  (define diff (- (string-length str) old-len))
   ;; From where do we need to re-tokenize? This will be < the pos of
   ;; the change. Back up to the start of the previous token (plus any
   ;; `backup` amount the lexer may have supplied) to ensure re-lexing
   ;; enough that e.g. appending a character does not create a
   ;; separate token when instead it should be combined with an
   ;; existing token for preceding character(s).
  (define beg
    (match (token-map-ref tm (sub1 pos))
      [(bounds+token beg _end (token _ backup)) (- beg backup)]
      [#f pos]))
  (define tokens (token-map-tokens tm))
  ;; We want to detect tokens that did not change other than their
  ;; position being shifted by `diff`. To do so for the insert/expand
  ;; case, unfortunately we need a copy of the token interval-map:
  (define old-tokens (make-interval-map
                      (for/list ([(k v) (in-dict tokens)])
                        (cons k v))))
  (define modes  (token-map-modes tm))
  (cond [(< 0 diff) (interval-map-expand!   tokens pos (+ pos diff))
                    (define orig-mode (interval-map-ref modes beg #f))
                    (interval-map-expand!   modes  beg (+ beg diff))
                    (interval-map-set!      modes  beg (+ beg diff) orig-mode)]
        [(< diff 0) (interval-map-contract! tokens pos (- pos diff))
                    (interval-map-contract! modes  beg (- beg diff))])
  (define updated-intervals '())
  (define just-shifted-count 0)
  (define (set-interval/update tokens beg end token)
    (define-values (old-beg old-end old-token)
      (interval-map-ref/bounds old-tokens (- beg diff) #f))
    (cond [(and old-beg old-end old-token
                (= old-beg (- beg diff))
                (= old-end (- end diff))
                (equal? old-token token))
           (log-racket-mode-debug
            "no change except shift; diff=~v old=~v new=~v"
            diff
            (list old-beg old-end old-token)
            (list beg end token))
           ;; Because we back up one extra, to be safe, we should
           ;; accumulate multiple just-shifted tokens to be sure
           ;; before stopping.
           (set! just-shifted-count (add1 just-shifted-count))
           (define continue? (< just-shifted-count 2))
           continue?]
          [else
           (set! updated-intervals (cons (bounds+token beg end token)
                                         updated-intervals))
           (log-racket-mode-debug "Set [~v ~v) to ~v" beg end token)
           (interval-map-set! tokens beg end token)
           #t])) ;continue
  (parameterize ([current-set-interval set-interval/update])
    (tokenize-string tokens modes (token-map-str tm) beg))
  (reverse updated-intervals))

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
  (define in (open-input-string (substring str (sub1 beg))))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (set-port-next-location! in 1 0 beg) ;we don't use line/col, just pos
  (tokenize-port str tokens modes in beg (interval-map-ref modes beg #f)))

(define (tokenize-port str tokens modes in offset mode)
  (define-values (lexeme kind delimit beg end backup new-mode)
    (module-lexer in offset mode))
  ;;(println (list offset mode lexeme kind delimit beg end backup new-mode))
  (unless (eof-object? lexeme)
    (interval-map-set! modes beg end mode)
    ;; The scribble-inside-lexer sometimes returns a value for
    ;; `lexeme` that is not the original lexed text. One example is
    ;; returning " " instead of "\n" for whitespace -- but we need
    ;; that in handle-white-space-token to create end-of-line tokens.
    ;; Also it sometimes returns 'text instead of the string for 'text
    ;; tokens, which would be harmless, except we want our `validate`
    ;; to be able to say that all token lexemes equal the original
    ;; string.
    (let ([lexeme (substring str (sub1 beg) (sub1 end))])
     (when (case kind
             [(white-space) (handle-white-space-token tokens lexeme beg end backup)]
             [(parenthesis) (handle-parenthesis-token tokens lexeme mode delimit beg end backup)]
             [else          (set-interval tokens beg end (token:misc lexeme backup kind))])
       (tokenize-port str tokens modes in end new-mode)))))

;; It is convenient to some users of the token map (e.g. indenters)
;; for it to supply end-of-line tokens distinct from generic
;; white-space tokens.
(define (handle-white-space-token im lexeme beg end backup)
  (let loop ([lexeme lexeme]
             [beg beg]
             [end end]
             [backup backup])
    (match lexeme
      [(pregexp "^\r\n")
       (set-interval im
                     beg (+ 2 beg)
                     (token:misc (substring lexeme 0 2) backup 'end-of-line))
       (loop (substring lexeme 2)
             (+ 2 beg)
             end
             (+ backup 2))]
      [(pregexp "^[\r\n]")
       (set-interval im beg (add1 beg)
                     (token:misc (substring lexeme 0 1) backup 'end-of-line))
       (loop (substring lexeme 1)
             (add1 beg)
             end
             (add1 backup))]
      [(pregexp "^([^\r\n]+)" (list _ s))
       (define len (string-length s))
       (set-interval im beg (+ beg len)
                     (token:misc (substring lexeme 0 len) backup 'white-space))
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
;; single 2-char token, parenthesis "(". HOWEVER. It handles "#'(" as
;; 2 tokens -- a 2-char "#'" constant followed by a 1-char parenthesis
;; "(". So...???
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
;; "a module language entirely determined by #lang and get-info". I
;; think? If so, this makes it unsuitable for use outside DrRacket.
;;
;; INSTEAD
;;
;; 1. I suggest instead of "parenthesis" tokens, a lexer should return
;;    "open" and "close" tokens. Instead of a 'delimit' field as in
;;    the status quo (which mostly just recapitulates information in
;;    the 'lexeme' field) there is a field stating the opposite,
;;    matching token. An open token says what its matching close
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
;; 2. Also, some languages (at least sexp langs) might want to have a
;;    token type similar to what Emacs calls "expression prefix" --
;;    such as for reader shorthands like ' and #' and #hasheq.
;;    Currently racket-lexer classifies these as "symbol", which makes
;;    sense as they are shortand for symbols like "quote" and
;;    "syntax". However, end user sexp navigation wants to treat the
;;    first character of those prefixes as the start of the entire
;;    sexp -- not the open paren. Likewise, indent usually wants to
;;    align with that first character, not the open paren.
;;    [Alternatively, I suppose a lexer could return "'(" or
;;    "#hasheq(" as as single open token. That means e.g. Emacs could
;;    not use char-syntax, but, we could provide our own
;;    forward-sexp-function that understands these, I think.]
;;
;; MEANWHILE: The following function attempts to "normalize" the
;; status quo "legacy" lexer protocol along these lines. Maybe something
;; like this would be necessary for backward compatibilty, even if a new
;; 'color-lexer-2 were implemented.
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
        "unexpected 'parenthesis token with delimit = ~v and lexeme = ~v"
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

(define (beg-of-line tm start-pos)
  (let loop ([pos start-pos])
    (match (token-map-ref tm pos)
      [(bounds+token beg end (? token:misc? t))
       #:when (and (eq? (token:misc-kind t) 'end-of-line)
                   ( < beg start-pos))
       end]
      [(bounds+token beg _end (? token?))
       (loop (sub1 beg))]
      [#f #:when (< 1 pos) (loop (sub1 pos))]
      [#f 1])))

(define (end-of-line tm pos)
  (let loop ([pos pos])
    (match (token-map-ref tm pos)
      [(bounds+token _beg end (? token:misc? t))
       #:when (eq? (token:misc-kind t) 'end-of-line)
       end]
      [(bounds+token _beg end (? token?))
       (loop end)]
      [#f (add1 (string-length (token-map-str tm)))])))

(define (backward-up tm pos)
  (let loop ([pos (match (token-map-ref tm pos)
                    ;; When pos is already exactly the start of an
                    ;; open token, start one position earlier.
                    [(bounds+token beg _end (? token:expr:open?))
                     (sub1 beg)]
                    [_ pos])]
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
            [#f #f]
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
           [#f #f]
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
  (let* ([str "#lang racket\n(a (b (c  foo))) (bar ((x)) y)\n"]
         ;;    1234567890123 4567890123456789012345678901234
         ;;             1          2         3         4
         [tm (create str)])
    (check-equal? (classify tm 1)
                  (bounds+token 1 13 (token:misc "#lang racket" 0 'other)))
    (check-equal? (classify tm 13)
                  (bounds+token 13 14 (token:misc "\n" 0 'end-of-line)))
    (check-equal? (beg-of-line tm 1) 1)
    (check-equal? (beg-of-line tm 2) 1)
    (check-equal? (beg-of-line tm 3) 1)
    (check-equal? (beg-of-line tm 13) 1)
    (check-equal? (beg-of-line tm 14) 14)
    (check-equal? (beg-of-line tm 15) 14)
    (check-equal? (beg-of-line tm 44) 14)
    (check-equal? (backward-up tm 16) 14)
    (check-equal? (backward-up tm 17) 14)
    (check-equal? (backward-up tm 18) 17)
    (check-equal? (backward-up tm 20) 17)
    (check-equal? (backward-up tm 22) 20)
    (check-equal? (backward-up tm 34) 31)
    (check-equal? (backward-up tm 42) 31)
    (check-false  (backward-up tm  1))
    (check-false  (backward-up tm 12))
    (check-false  (backward-up tm 13))
    (check-false  (backward-up tm 14))
    (check-false  (backward-up tm 30))
    (check-false  (backward-up tm 31))
    (check-false  (backward-up tm 43))
    (check-equal? (forward-whitespace/comment tm 23) 24)
    (check-equal? (backward-whitespace/comment tm 22) 21)
    (check-equal? (forward-sexp tm 24) 27)
    (check-equal? (forward-sexp tm 14) 30)
    (check-equal? (backward-sexp tm 26) 24)
    (check-equal? (backward-sexp tm 29) 14)))

(module+ test
  (let* ([str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar"]
         ;;    1234567890123 45678901234 567890 12345678901234567890123456
         ;;             1          2          3          4         5
         [tm (create str)])
    (check-equal? (token-map-str tm)
                  "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 16) (token:misc "42" 0 'constant))
                   (cons '(16 . 17) (token:misc " " 0 'white-space))
                   (cons '(17 . 18) (token:expr:open "(" 0 "(" ")"))
                   (cons '(18 . 23) (token:misc "print" 0 'symbol))
                   (cons '(23 . 24) (token:misc " " 0 'white-space))
                   (cons '(24 . 31) (token:misc "\"hello\"" 0 'string))
                   (cons '(31 . 32) (token:expr:close ")" 0 "(" ")"))
                   (cons '(32 . 33) (token:misc " " 0 'white-space))
                   (cons '(33 . 39) (token:misc "@print" 0 'symbol))
                   (cons '(39 . 40) (token:expr:open "{" 0 "{" "}"))
                   (cons '(40 . 45) (token:misc "Hello" 0 'symbol))
                   (cons '(45 . 46) (token:expr:close "}" 0 "{" "}"))
                   (cons '(46 . 47) (token:misc " " 0 'white-space))
                   (cons '(47 . 48) (token:misc "'" 0 'constant))
                   (cons '(48 . 51) (token:misc "foo" 0 'symbol))
                   (cons '(51 . 52) (token:misc " " 0 'white-space))
                   (cons '(52 . 57) (token:misc "#:bar" 0 'hash-colon-keyword))))
    (check-equal? (dict->list (token-map-modes tm))
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
    (check-equal? (update tm 52 5 "'bar")
                  (list
                   (bounds+token 51 52 (token:misc " " 0 'white-space))
                   (bounds+token 52 53 (token:misc "'" 0 'constant))
                   (bounds+token 53 56 (token:misc "bar" 0 'symbol))))
    (check-equal? (update tm 47 4 "'bar")
                  '()
                  "although lexeme changes from \"'foo\" to \"'bar\" bounds and classification are the same, therefore no update")
    (check-equal? (update tm 24 7 "'hell")
                  (list
                   (bounds+token 23 24 (token:misc " " 0 'white-space))
                   (bounds+token 24 25 (token:misc "'" 0 'constant))
                   (bounds+token 25 29 (token:misc "hell" 0 'symbol))))
    (check-equal? (update tm 14 2 "99999")
                  (list
                   (bounds+token 13 14 (token:misc "\n" 0 'end-of-line))
                   (bounds+token 14 19 (token:misc "99999" 0 'constant))))
    (check-equal? (token-map-str tm)
                  "#lang racket\n99999 (print 'hell) @print{Hello} 'bar 'bar")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 19) (token:misc "99999" 0 'constant))
                   (cons '(19 . 20) (token:misc " " 0 'white-space))
                   (cons '(20 . 21) (token:expr:open "(" 0 "(" ")"))
                   (cons '(21 . 26) (token:misc "print" 0 'symbol))
                   (cons '(26 . 27) (token:misc " " 0 'white-space))
                   (cons '(27 . 28) (token:misc "'" 0 'constant))
                   (cons '(28 . 32) (token:misc "hell" 0 'symbol))
                   (cons '(32 . 33) (token:expr:close ")" 0 "(" ")"))
                   (cons '(33 . 34) (token:misc " " 0 'white-space))
                   (cons '(34 . 40) (token:misc "@print" 0 'symbol))
                   (cons '(40 . 41) (token:expr:open "{" 0 "{" "}"))
                   (cons '(41 . 46) (token:misc "Hello" 0 'symbol))
                   (cons '(46 . 47) (token:expr:close "}" 0 "{" "}"))
                   (cons '(47 . 48) (token:misc " " 0 'white-space))
                   (cons '(48 . 49) (token:misc "'" 0 'constant))
                   (cons '(49 . 52) (token:misc "foo" 0 'symbol))
                   (cons '(52 . 53) (token:misc " " 0 'white-space))
                   (cons '(53 . 54) (token:misc "'" 0 'constant))
                   (cons '(54 . 57) (token:misc "bar" 0 'symbol))))
    (check-equal? (dict->list (token-map-modes tm))
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
                    ((54 . 57) . ,racket-lexer)))))

(module+ test
  (let* ([str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar"]
         [tm (create str)])
    (check-equal? (token-map-str tm)
                  str)
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 20) (token:misc "#lang at-exp racket" 0 'other))
                   (cons '(20 . 21) (token:misc "\n" 0 'end-of-line))
                   (cons '(21 . 23) (token:misc "42" 0 'constant))
                   (cons '(23 . 24) (token:misc " " 0 'white-space))
                   (cons '(24 . 25) (token:expr:open "(" 0 "(" ")"))
                   (cons '(25 . 30) (token:misc "print" 0 'symbol))
                   (cons '(30 . 31) (token:misc " " 0 'white-space))
                   (cons '(31 . 38) (token:misc "\"hello\"" 0 'string))
                   (cons '(38 . 39) (token:expr:close ")" 0 "(" ")"))
                   (cons '(39 . 40) (token:misc " " 0 'white-space))
                   (cons '(40 . 41) (token:misc "@" 0 'symbol))
                   (cons '(41 . 46) (token:misc "print" 0 'symbol))
                   (cons '(46 . 47) (token:expr:open "{" 0 "{" "}"))
                   (cons '(47 . 60) (token:misc "Hello (there)" 0 'text))
                   (cons '(60 . 61) (token:expr:close "}" 0 "{" "}"))
                   (cons '(61 . 62) (token:misc " " 0 'white-space))
                   (cons '(62 . 63) (token:misc "'" 0 'constant))
                   (cons '(63 . 66) (token:misc "foo" 0 'symbol))
                   (cons '(66 . 67) (token:misc " " 0 'white-space))
                   (cons '(67 . 72) (token:misc "#:bar" 0 'hash-colon-keyword))))
    (check-equal? (classify tm (sub1 (string-length str)))
                  (bounds+token 67 72 (token:misc "#:bar" 0 'hash-colon-keyword)))))

(module+ test
  (let* ([str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword"]
         [tm (create str)])
    (check-equal? (token-map-str tm)
                  str)
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 20) (token:misc "#lang scribble/text" 0 'other))
                   (cons '(20 . 21) (token:misc "\n" 0 'end-of-line))
                   (cons '(21 . 27) (token:misc "Hello" 0 'text))
                   (cons '(27 . 28) (token:misc "@" 0 'symbol))
                   (cons '(28 . 29) (token:expr:open "(" 0 "(" ")"))
                   (cons '(29 . 34) (token:misc "print" 0 'symbol))
                   (cons '(34 . 35) (token:misc " " 0 'white-space))
                   (cons '(35 . 42) (token:misc "\"hello\"" 0 'string))
                   (cons '(42 . 43) (token:expr:close ")" 0 "(" ")"))
                   (cons '(43 . 44) (token:misc " " 0 'text))
                   (cons '(44 . 45) (token:misc "@" 0 'symbol))
                   (cons '(45 . 50) (token:misc "print" 0 'symbol))
                   (cons '(50 . 51) (token:expr:open "{" 0 "{" "}"))
                   (cons '(51 . 64) (token:misc "Hello (there)" 0 'text))
                   (cons '(64 . 65) (token:expr:close "}" 0 "{" "}"))
                   (cons '(65 . 81) (token:misc " #:not-a-keyword" 0 'text))))))

(module+ test
  (let ([tm  (create "#lang racket\n(λ () #t)")])
    (check-equal? (classify tm 15)
                  (bounds+token 15 16 (token:misc "λ" 0 'symbol)))
    (check-equal? (update tm 18 0 "a")
                  (list
                   (bounds+token 17 18 (token:expr:open "(" 0 "(" ")"))
                   (bounds+token 18 19 (token:misc "a" 0 'symbol))))
    (check-equal? (classify tm 18)
                  (bounds+token 18 19 (token:misc "a" 0 'symbol)))))

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

(module+ test
  (require syntax-color/racket-lexer)
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (create str)])
    (update tm 14 0 "d")
    (update tm 15 0 "o")
    (check-equal? (token-map-str tm) "#lang racket\ndo")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 16) (token:misc "do" 0 'symbol))))
    (check-equal? (dict->list (token-map-modes tm))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 16) racket-lexer))))
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (create str)])
    (update tm 14 0 "1") ;initially lexed as 'constant
    (update tm 15 0 "x") ;should re-lex "1x" as 'symbol
    (check-equal? (token-map-str tm) "#lang racket\n1x")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 16) (token:misc "1x" 0 'symbol))))
    (check-equal? (dict->list (token-map-modes tm))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 16) racket-lexer))))
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (create str)])
    (update tm 14 0 "1") ;initially lexed as 'constant
    (update tm 15 0 "x") ;should re-lex "1x" as 'symbol
    (update tm 16 0 "1") ;still symbol
    (update tm 15 1 "")  ;deleting the "x" should re-lex the "11" as constant
    (check-equal? (token-map-str tm) "#lang racket\n11")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 16) (token:misc "11" 0 'constant))))
    (check-equal? (dict->list (token-map-modes tm))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 16) racket-lexer))))
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (create str)])
    ;; as if paredit etc. were enabled
    (update tm 14 0 "(")
    (update tm 15 0 ")")
    (update tm 15 0 "h")
    (update tm 16 0 "i")
    (check-equal? (token-map-str tm) "#lang racket\n(hi)")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 15) (token:expr:open "(" 0 "(" ")"))
                   (cons '(15 . 17) (token:misc "hi" 0 'symbol))
                   (cons '(17 . 18) (token:expr:close ")" 0 "(" ")"))))
    (check-equal? (dict->list (token-map-modes tm))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 15) racket-lexer)
                   (cons '(15 . 17) racket-lexer)
                   (cons '(17 . 18) racket-lexer)))))

(module+ test
  (require racket/format)
  (provide check-valid?)
  (define (check-valid? tm)
    (define-values (str min-token-beg max-token-end)
      (for/fold ([str ""]
                 [min-token-beg 99999]
                 [max-token-end 0])
                ([(k v) (in-dict (token-map-tokens tm))])
        (values (string-append str (token-lexeme v))
                (min (car k) min-token-beg)
                (max (cdr k) max-token-end))))
    (check-equal? str (token-map-str tm)
                  (format "append of lexemes equals string, in: ~v" tm))
    (check-equal? min-token-beg
                  1)
    (check-equal? max-token-end
                  (add1 (string-length (token-map-str tm))))
    (define-values (min-modes-beg max-modes-end)
      (for/fold ([min-modes-beg 99999]
                 [max-modes-end 0])
                ([k (in-dict-keys (token-map-modes tm))])
        (values (min (car k) min-modes-beg)
                (max (cdr k) max-modes-end))))
    (check-equal? min-token-beg min-modes-beg)
    (check-equal? max-token-end max-modes-end)))

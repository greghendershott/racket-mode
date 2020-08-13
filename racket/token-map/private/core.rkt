#lang racket/base

;; Note: The idea is that this would be, not a part of Racket Mode,
;; but instead a package used by both tools and hash-lang indenters.
;; It is similar in spirit to a subset of racket:text<%> but without
;; requiring racket/gui.

(require (only-in data/interval-map
                  make-interval-map
                  interval-map-set!
                  interval-map-ref
                  interval-map-ref/bounds ;added data-lib 1.1 / Racket 6.12
                  interval-map-remove!
                  interval-map-expand!
                  interval-map-contract!)
         racket/contract
         racket/contract/option
         racket/dict
         racket/match
         syntax-color/module-lexer)

(provide token-map?
         create
         update!
         tokens
         classify
         token-text
         lexer-names
         (struct-out bounds+token)
         (struct-out token)
         (struct-out token:open)
         (struct-out token:close)
         (struct-out token:misc)
         position/c)

;; Provided only for use by nav.rkt
(module+ private
  (provide token-map-ref
           token-map-str))

(module+ test (require rackunit))

;; Keep in mind that the token map interface uses 1-based positions --
;; because lexers do so. (This library is supposed to be agnostic wrt
;; clients, so although it is convenient when using this from Emacs
;; that it also uses 1-based positions, that is not the motivation.)
(define position/c exact-positive-integer?)

;; A token-map has a copy of the entire original source string, an
;; interval-map of tokens, and an interval-map of lexer modes (the
;; last to support updating and re-lexing).
(struct token-map ([str #:mutable]
                   tokens ;interval-map: position/c -> token?
                   modes) ;interval-map: position/c -> lexer mode
  ;; Printing the string and modes is overhwelming; just print tokens.
  #:methods gen:custom-write
  [(define (write-proc tm port mode)
     (parameterize ([current-output-port port])
       (display "#<token-map")
       (for/list ([(k v) (in-dict (token-map-tokens tm))])
         (display "\n  ")
         (print (bounds+token (car k) (cdr k) v)))
       (display ">")))]
  #:transparent)

;; Mainly we follow the lexer protocol where various token kinds are
;; simply encoded with a symbol like 'white-space or 'constant.
;; However it is helpful to use distinct struct types for open and
;; close tokens: They store the matching, opposite lexee.
(struct token (lexeme backup) #:transparent)
(struct token:open  token (close) #:transparent)
(struct token:close token (open) #:transparent)
(struct token:misc  token (kind) #:transparent)

;; A bounds+token represents a token in an interval-map -- i.e. it is
;; interval-map-ref/bounds represented as one value not three.
(struct bounds+token (beg end token) #:transparent)

;; (-> token-map? position/c (or/c #f bounds+token?))
(define (token-map-ref tm pos)
  (and pos
       (let-values ([(beg end token)
                     (interval-map-ref/bounds (token-map-tokens tm) pos #f)])
         (and beg end token
              (bounds+token beg end token)))))

(define/contract (create str)
  (-> string? token-map?)
  (define tm (token-map str (make-interval-map) (make-interval-map)))
  (define (set-interval beg end token)
    (interval-map-set! (token-map-tokens tm) beg end token))
  (tokenize-string! tm 1 set-interval)
  tm)

;; Given an update to the string, re-tokenize -- doing minimal work
;; and returning a minimal list of changes. The emphasis on "minimal"
;; is because potentially this could be called for every single
;; character change that a user makes while typing in their editor.
;; (Although an editor might accumulate contiguous changes to give us
;; at once, any such lazy tactic is tricky to get right, and we don't
;; expect it. Instead we try to be as fast as possible.) Also, this
;; might be called for quite large changes such as a cut or paste.
;;
;; The function signature here is similar to that of Emacs'
;; after-change functions: Something changed starting at POS. The text
;; there used to be OLD-LEN chars long. It is now STR.
(define/contract (update! tm pos old-len str)
  (-> token-map? position/c exact-nonnegative-integer? string? any)
  (unless (<= (sub1 pos) (string-length (token-map-str tm)))
    (raise-argument-error 'update "valid position" 1 tm pos old-len str))
  (if (and (zero? old-len) (equal? str ""))
      (list) ;no-op fast path
      (do-update! tm pos old-len str)))

(define (do-update! tm pos old-len str)
  (set-token-map-str! tm
                      (string-append (substring (token-map-str tm)
                                                0
                                                (sub1 pos))
                                     str
                                     (substring (token-map-str tm)
                                                (+ (sub1 pos) old-len))))
  (define diff (- (string-length str) old-len))
  ;; From where do we need to re-tokenize? This will be < the pos of
  ;; the change. Back up to the start of the previous token (plus any
  ;; `backup` amount the lexer may have supplied for that token) to
  ;; ensure re-lexing enough such that e.g. appending a character does
  ;; not create a separate token when instead it should be combined
  ;; with an existing token for preceding character(s).
  (define beg
    (match (token-map-ref tm (sub1 pos))
      [(bounds+token beg _end (token _ backup)) (- beg backup)]
      [#f pos]))
  ;; Expand/contract the tokens and modes interval-maps.
  (match-define (token-map _str tokens modes) tm)
  (cond [(< 0 diff) (interval-map-expand!   tokens pos (+ pos diff))
                    (define orig-mode (interval-map-ref modes beg #f))
                    (interval-map-expand!   modes  beg (+ beg diff))
                    (interval-map-set!      modes  beg (+ beg diff) orig-mode)]
        [(< diff 0) (interval-map-contract! tokens pos (- pos diff))
                    (interval-map-contract! modes  beg (- beg diff))])
  ;; We want to detect tokens that did not change other than their
  ;; position being shifted by `diff`. When we've encountered enough
  ;; of those, we can conclude the re-lex has converged back to the
  ;; original lex and we need not continue further -- saving
  ;; potentially a huge amount of time/work.
  ;;
  ;; Because we are modifying the tokens interval-map, we can't rely
  ;; on it to obtain the "old" picture -- maybe we overwrote some of
  ;; the old. A naive idea is to copy the entire original interval-map
  ;; to an `old-tokens` map, but this can be extremely slow for large
  ;; maps.
  ;;
  ;; Instead use a "backup on write" approach. Populate `old-tokens`
  ;; as/when we modify the main map. Then to do `diff`-shift-only
  ;; compares, consult `old-tokens` first, else use the main map.
  (define old-tokens (make-interval-map))
  (define just-shifted-count 0)
  (define just-shifted-goal 3) ;2 b/c beg from prev token; +1 to be safe
  (define actual-changes '())
  (define (set-interval beg end token)
    ;; If the "shadow" old-tokens interval-map has values, use those,
    ;; else use the main interval-map.
    (define-values (old-beg old-end old-token)
      (let-values ([(old-beg old-end old-tok) (interval-map-ref/bounds old-tokens beg #f)])
        (if (and old-beg old-end old-tok)
            (values old-beg old-end old-tok)
            (interval-map-ref/bounds tokens beg #f))))
    (cond [;; If there's no difference, possibly stop re-lexing.
           (and old-beg old-end old-token
                (= old-beg beg)
                (= old-end end)
                (equal? old-token token))
           (set! just-shifted-count (add1 just-shifted-count))
           (define continue? (< just-shifted-count just-shifted-goal))
           continue?]
          [else
           ;; "Backup" the original interval(s) in the "shadow"
           ;; `old-tokens` map. Do so with the positions shifted by
           ;; `diff`, to simplify lookup and compares, above.
           (let loop ([n beg])
             (define-values (old-beg old-end old-token)
               (interval-map-ref/bounds tokens n #f))
             (when (and old-beg old-end old-token)
               (interval-map-set! old-tokens (+ old-beg diff) (+ old-end diff) old-token)
               (when (< old-end end)
                 (loop old-end))))
           ;; Accumulate this actual change, update the main map, and
           ;; definitely continue re-lexing.
           (set! actual-changes (cons (bounds+token beg end token)
                                      actual-changes))
           (interval-map-set! tokens beg end token)
           #t])) ;continue
  (tokenize-string! tm beg set-interval)
  (reverse actual-changes))

(define/contract (classify tm pos)
  (-> token-map? (or/c #f position/c) (or/c #f bounds+token?))
  (token-map-ref tm pos))

(define/contract (token-text tm pos)
  (-> token-map? position/c (or/c #f string?))
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

(define module-lexer/waived (waive-option module-lexer))

(define/contract (tokenize-string! tm from set-interval)
  (-> token-map? position/c (-> position/c position/c token? any) any)
  (match-define (token-map str _tokens modes) tm)
  (define in (open-input-string (substring str (sub1 from))))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (set-port-next-location! in 1 0 from) ;we don't use line/col, just pos
  (let tokenize-port! ([offset from]
                       [mode   (interval-map-ref modes from #f)])
    (define-values (lexeme kind delimit beg end backup new-mode)
      (module-lexer/waived in offset mode))
    (unless (eof-object? lexeme)
      (interval-map-set! modes beg end mode)
      ;; Don't trust `lexeme`; instead get from the input string.^1
      (let ([lexeme (substring str (sub1 beg) (sub1 end))])
        (when (handle-token set-interval mode lexeme kind delimit beg end backup)
          (tokenize-port! end new-mode))))))
;; ^1: The scribble-inside-lexer sometimes returns a value for
;; `lexeme` that is not the original lexed text. One example is
;; returning " " instead of "\n" for whitespace -- but we need that in
;; handle-white-space-token to create end-of-line tokens. Also it
;; sometimes returns 'text instead of the string for 'text tokens,
;; which would be harmless, except we want `check-valid?` to be able
;; to say that appending all token lexemes equals the input string.

;; Returns boolean? meaning, "keep going?".
(define (handle-token set-interval mode lexeme kind delimit beg end backup)
  (case kind
    [(white-space) (handle-white-space set-interval lexeme beg end backup)]
    [(parenthesis) (handle-parenthesis set-interval lexeme mode delimit beg end backup)]
    [else          (set-interval beg end (token:misc lexeme backup kind))]))

;; It is convenient to some users of the token map (e.g. indenters)
;; for it to supply end-of-line tokens distinct from generic
;; white-space tokens.
(define (handle-white-space set-interval lexeme beg end backup)
  (let loop ([lexeme lexeme]
             [beg    beg]
             [end    end]
             [backup backup])
    (match lexeme
      [(pregexp "^\r\n")
       (set-interval beg (+ 2 beg)
                     (token:misc (substring lexeme 0 2) backup 'end-of-line))
       (loop (substring lexeme 2)
             (+ 2 beg)
             end
             (+ backup 2))]
      [(pregexp "^[\r\n]")
       (set-interval beg (add1 beg)
                     (token:misc (substring lexeme 0 1) backup 'end-of-line))
       (loop (substring lexeme 1)
             (add1 beg)
             end
             (add1 backup))]
      [(pregexp "^([^\r\n]+)" (list _ s))
       (define len (string-length s))
       (set-interval beg (+ beg len)
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
;; right: `delimit` is one of the chars "()[]{}", and, often it just
;; "recapitulates" information from the lexeme. I'm guessing the
;; justification for this was that e.g. racket-lexer handles "#hasheq"
;; as a single multi-char paren token with delimit="(", and similarly
;; #(" as a single 2-char paren token with delimit="(". That seems
;; reasonable. HOWEVER, racket-lexer handles "#'(" as 2 tokens -- a
;; 2-char "#'" constant followed by a 1-char paren token "(". I don't
;; understand that.
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
;; I suggest instead of "parenthesis" tokens, a lexer should return
;; "open" and "close" tokens. Instead of a 'delimit' field as in the
;; status quo (which mostly just recapitulates information in the
;; 'lexeme' field) there is a field stating the /opposite/, matching
;; token. An open token says what its matching close token is, and
;; vice versa. That way a lang could specify expression/block
;; delimiters like "begin" and "end", its lexer could emit tokens like
;; (open lexeme:"begin" opposite:"end") and (close lexeme:"end"
;; opposite:"begin"), and a consuming indenter or editor can use these
;; effectively. (I think this will work even if multiple open tokens
;; share the same end token -- like when "then" and "else" both use
;; "end", as opposed to "elif" and "fi" -- right?) (For off-side rule
;; lexers, presumably indent = open and dedent = close, and the
;; "opposite" value is N/A? TBD!)
;;
;;   Question: Should such a new protocol be something like (get-info
;;   'color-lexer-2)? Or, possibly should this be handled more like a
;;   lang-provided read and read-syntax? After all, what we do here is
;;   "midway" between a lex and a Lisp read: Like read, we interpret
;;   open/close tokens enough to discover and represent nested
;;   expressions/blocks -- but we do not go further to discover
;;   literal values like numbers or symbols, instead the tokens remain
;;   "raw strings".
;;
;; MEANWHILE: The following function attempts to "normalize" the
;; status quo "legacy" lexer protocol along these lines. Maybe
;; something like this would be good for backward compatibilty, even
;; if a new protocol were adopted.
(define (handle-parenthesis set-interval lexeme mode delimit beg end backup)
  (define (open close) (token:open lexeme backup close))
  (define (close open) (token:close lexeme backup open))
  (define tok
    (match delimit
      ['|(| (open ")")]
      ['|[| (open "]")]
      ['|{| (open "}")]
      ['|)| (close "(")]
      ['|]| (close "[")]
      ['|}| (close "{")]
      [_
       #;
       (log--warning
        "unexpected 'parenthesis token with delimit = ~v and lexeme = ~v"
        delimit lexeme)
       (match lexeme
         ;; Defensive:
         ["(" (open ")")]
         [")" (close "(")]
         ;; I have seen this with at-exp and scribble/text. No idea
         ;; why. Definitely don't want to treat it as an open -- there
         ;; will be no matching close, later. Instead I guess treat it
         ;; as 'symbol, like how "'" is lexed??
         ["@"
          #:when (memq (mode->lexer-name mode)
                       '(scribble-lexer
                         scribble-inside-lexer))
          (token:misc lexeme backup 'symbol)]
         ;; Super-defensive WTF:
         [_  (token:misc lexeme backup 'other)])]))
  (set-interval beg end tok))

(define (mode->lexer-name mode)
  (object-name (match mode
                 [(? procedure? p)          p]
                 [(cons (? procedure? p) _) p]
                 [v                         v])))

(define (lexer-names tm)
  (for*/fold ([names '()])
             ([mode (in-dict-values (token-map-modes tm))]
              #:when mode
              [name (in-value (mode->lexer-name mode))]
              #:when (not (member name names)))
    (cons name names)))

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
                   (cons '(17 . 18) (token:open "(" 0 ")"))
                   (cons '(18 . 23) (token:misc "print" 0 'symbol))
                   (cons '(23 . 24) (token:misc " " 0 'white-space))
                   (cons '(24 . 31) (token:misc "\"hello\"" 0 'string))
                   (cons '(31 . 32) (token:close ")" 0 "("))
                   (cons '(32 . 33) (token:misc " " 0 'white-space))
                   (cons '(33 . 39) (token:misc "@print" 0 'symbol))
                   (cons '(39 . 40) (token:open "{" 0 "}"))
                   (cons '(40 . 45) (token:misc "Hello" 0 'symbol))
                   (cons '(45 . 46) (token:close "}" 0 "{"))
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
    (check-equal? (update! tm 52 5 "'bar")
                  (list
                   (bounds+token 52 53 (token:misc "'" 0 'constant))
                   (bounds+token 53 56 (token:misc "bar" 0 'symbol))))
    (check-equal? (update! tm 47 4 "'bar")
                  (list (bounds+token 48 51 (token:misc "bar" 0 'symbol))))
    (check-equal? (update! tm 24 7 "'hell")
                  (list
                   (bounds+token 24 25 (token:misc "'" 0 'constant))
                   (bounds+token 25 29 (token:misc "hell" 0 'symbol))))
    (check-equal? (update! tm 14 2 "99999")
                  (list
                   (bounds+token 14 19 (token:misc "99999" 0 'constant))))
    (check-equal? (token-map-str tm)
                  "#lang racket\n99999 (print 'hell) @print{Hello} 'bar 'bar")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 19) (token:misc "99999" 0 'constant))
                   (cons '(19 . 20) (token:misc " " 0 'white-space))
                   (cons '(20 . 21) (token:open "(" 0  ")"))
                   (cons '(21 . 26) (token:misc "print" 0 'symbol))
                   (cons '(26 . 27) (token:misc " " 0 'white-space))
                   (cons '(27 . 28) (token:misc "'" 0 'constant))
                   (cons '(28 . 32) (token:misc "hell" 0 'symbol))
                   (cons '(32 . 33) (token:close ")" 0 "("))
                   (cons '(33 . 34) (token:misc " " 0 'white-space))
                   (cons '(34 . 40) (token:misc "@print" 0 'symbol))
                   (cons '(40 . 41) (token:open "{" 0 "}"))
                   (cons '(41 . 46) (token:misc "Hello" 0 'symbol))
                   (cons '(46 . 47) (token:close "}" 0 "{"))
                   (cons '(47 . 48) (token:misc " " 0 'white-space))
                   (cons '(48 . 49) (token:misc "'" 0 'constant))
                   (cons '(49 . 52) (token:misc "bar" 0 'symbol))
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
                   (cons '(24 . 25) (token:open "(" 0 ")"))
                   (cons '(25 . 30) (token:misc "print" 0 'symbol))
                   (cons '(30 . 31) (token:misc " " 0 'white-space))
                   (cons '(31 . 38) (token:misc "\"hello\"" 0 'string))
                   (cons '(38 . 39) (token:close ")" 0 "("))
                   (cons '(39 . 40) (token:misc " " 0 'white-space))
                   (cons '(40 . 41) (token:misc "@" 0 'symbol))
                   (cons '(41 . 46) (token:misc "print" 0 'symbol))
                   (cons '(46 . 47) (token:open "{" 0 "}"))
                   (cons '(47 . 60) (token:misc "Hello (there)" 0 'text))
                   (cons '(60 . 61) (token:close "}" 0 "{"))
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
                   (cons '(21 . 27) (token:misc "Hello " 0 'text))
                   (cons '(27 . 28) (token:misc "@" 0 'symbol))
                   (cons '(28 . 29) (token:open "(" 0 ")"))
                   (cons '(29 . 34) (token:misc "print" 0 'symbol))
                   (cons '(34 . 35) (token:misc " " 0 'white-space))
                   (cons '(35 . 42) (token:misc "\"hello\"" 0 'string))
                   (cons '(42 . 43) (token:close ")" 0 "("))
                   (cons '(43 . 44) (token:misc " " 0 'text))
                   (cons '(44 . 45) (token:misc "@" 0 'symbol))
                   (cons '(45 . 50) (token:misc "print" 0 'symbol))
                   (cons '(50 . 51) (token:open "{" 0 "}"))
                   (cons '(51 . 64) (token:misc "Hello (there)" 0 'text))
                   (cons '(64 . 65) (token:close "}" 0 "{"))
                   (cons '(65 . 81) (token:misc " #:not-a-keyword" 0 'text))))))

(module+ test
  (let ([tm  (create "#lang racket\n(λ () #t)")])
    (check-equal? (classify tm 15)
                  (bounds+token 15 16 (token:misc "λ" 0 'symbol)))
    (check-equal? (update! tm 18 0 "a")
                  (list
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
  (update! tm 14 0 "()")
  (update! tm 15 0 "d")
  tm
  (update! tm 16 0 "o")
  (update! tm 15 0 "1")
  (update! tm 16 0 "2")
  (update! tm 17 0 " ")
  tm)

(module+ test
  (require syntax-color/racket-lexer)
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (create str)])
    (update! tm 14 0 "d")
    (update! tm 15 0 "o")
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
    (update! tm 14 0 "1") ;initially lexed as 'constant
    (update! tm 15 0 "x") ;should re-lex "1x" as 'symbol
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
    (update! tm 14 0 "1") ;initially lexed as 'constant
    (update! tm 15 0 "x") ;should re-lex "1x" as 'symbol
    (update! tm 16 0 "1") ;still symbol
    (update! tm 15 1 "")  ;deleting the "x" should re-lex the "11" as constant
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
    (update! tm 14 0 "(")
    (update! tm 15 0 ")")
    (update! tm 15 0 "h")
    (update! tm 16 0 "i")
    (check-equal? (token-map-str tm) "#lang racket\n(hi)")
    (check-equal? (dict->list (token-map-tokens tm))
                  (list
                   (cons '(1 . 13) (token:misc "#lang racket" 0 'other))
                   (cons '(13 . 14) (token:misc "\n" 0 'end-of-line))
                   (cons '(14 . 15) (token:open "(" 0 ")"))
                   (cons '(15 . 17) (token:misc "hi" 0 'symbol))
                   (cons '(17 . 18) (token:close ")" 0 "("))))
    (check-equal? (dict->list (token-map-modes tm))
                  (list
                   (cons '(1 . 13) #f)
                   (cons '(13 . 14) racket-lexer)
                   (cons '(14 . 15) racket-lexer)
                   (cons '(15 . 17) racket-lexer)
                   (cons '(17 . 18) racket-lexer))))
  (let* ([str "#lang racket\n#hash\n"]
         ;;    1234567890123 456789
         ;;             1
         [tm (create str)])
    (check-equal? (token-map-ref tm 14)
                  (bounds+token 14 19 (token:misc "#hash" 0 'error)))
    (check-equal? (update! tm 19 0 "(")
                  (list (bounds+token 14 20 (token:open "#hash(" 0 ")")))
                  "Adding parens after #hash re-lexes from an error to an open")
    (check-equal? (token-map-ref tm 14)
                  (bounds+token 14 20 (token:open "#hash(" 0 ")")))))

(module+ test
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

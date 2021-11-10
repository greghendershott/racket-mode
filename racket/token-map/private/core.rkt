#lang racket/base

(require (only-in data/interval-map
                  make-interval-map
                  interval-map-set!
                  interval-map-ref
                  interval-map-ref/bounds ;added data-lib 1.1 / Racket 6.12
                  interval-map-remove!
                  interval-map-expand!
                  interval-map-contract!)
         racket/async-channel
         racket/class
         racket/contract
         racket/contract/option
         racket/dict
         racket/match
         syntax-color/module-lexer)

(provide token-map?
         create
         delete
         update!
         tokens
         classify
         token-text
         (struct-out bounds+token)
         (struct-out token)
         (struct-out token:open)
         (struct-out token:close)
         (struct-out token:misc)
         generation/c
         position/c
         max-position)

;; Provided only for use by nav.rkt and indent.rkt
(module+ private
  (provide token-map-ref
           token-map-str
           block-until-updated-thru
           token-map-like-text%))

(module+ test (require rackunit))

;; To coordinate we use a monotonic "generation". `create` sets the
;; generation to 1. Thereafter the client should increment the
;; generation for every call to update!. Then, when it needs to use
;; some query operation such as classify, it supplies both its latest
;; generation and the position. The query op will block until/unless
;; we have finished updating that generation through that position.
(define generation/c exact-nonnegative-integer?)

;; Keep in mind that the token map interface uses 1-based positions --
;; because lexers do so. (This library is supposed to be agnostic wrt
;; clients, so although it is convenient when using this from Emacs
;; that it also uses 1-based positions, that is not the motivation.)
(define position/c exact-positive-integer?)
(define max-position (sub1 (expt 2 63)))

;; A token-map has a copy of the entire original source string, an
;; interval-map of tokens, and an interval-map of lexer modes (the
;; last to support updating and re-lexing).
(struct token-map
  ([generation #:mutable]      ;generation/c
   [str #:mutable]             ;string?
   tokens                      ;interval-map: position/c -> token?
   modes                       ;interval-map: position/c -> lexer mode
   update-chan                 ;async-channel? msgs -> updater thread
   notify-chan                 ;async-channel? msgs <- updater thread
   [updated-thru #:mutable]    ;position/c
   lexer                       ;lexer
   open/close                  ;(listof (list/c symbol? symbol?))
   grouper                     ;procedure?
   [like-text% #:mutable]      ;object?
   )
  #:methods gen:custom-write
  [(define (write-proc tm port mode)
     (parameterize ([current-output-port port])
       (printf "#<token-map generation ~v updated-thru ~v>"
               (token-map-generation tm)
               (token-map-updated-thru tm))))]
  #:transparent)

(define default-open/close `((,(string->symbol "(") ,(string->symbol ")"))
                             (,(string->symbol "[") ,(string->symbol "]"))
                             (,(string->symbol "{") ,(string->symbol "}"))))

(define (default-grouping-position _obj _start _limit _direction) #t)

(define/contract (create str notify-chan)
  (-> string? async-channel? token-map?)
  (define get-info (or (with-handlers ([values (λ _ #f)])
                         (read-language (open-input-string str)
                                        (λ _ #f)))
                       (λ (_key default) default)))
  ;; Create initially empty and call update! to do that work on the
  ;; updater thread. That way we can return immediately and we are
  ;; coordinated with any additional updates.
  (define tm (token-map 0
                        ""
                        (make-interval-map)
                        (make-interval-map)
                        (make-async-channel)
                        notify-chan
                        0
                        (or (get-info 'color-lexer #f)
                            (waive-option module-lexer))
                        (or (get-info 'drracket:paren-matches #f)
                            default-open/close)
                        (or (get-info 'drracket:grouping-position #f)
                            default-grouping-position)
                        #f))
  (set-token-map-like-text%! tm (new-like-text% tm))
  (thread (updater tm))
  (update! tm 1 1 0 str)
  tm)

(define/contract (delete tm)
  (-> token-map? any)
  (async-channel-put (token-map-update-chan tm) 'quit))

;; Mainly we follow the lexer protocol where various token kinds are
;; simply encoded with a symbol like 'white-space or 'constant.
;; However it is helpful to use distinct struct types for open and
;; close tokens: They store the matching, opposite lexeme.
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

;; The function signature here is similar to that of Emacs'
;; after-change functions: Something changed starting at POS. The text
;; there used to be OLD-LEN chars long, but is now STR.
(define/contract (update! tm gen pos old-len str)
  (-> token-map? generation/c position/c exact-nonnegative-integer? string? any)
  (unless (< (token-map-generation tm) gen)
    (raise-argument-error 'update! "valid generation" 1 tm gen pos old-len str))
  (unless (and (<= 1 pos)
               #;
               (<= (sub1 pos) (string-length (token-map-str tm))))
    (raise-argument-error 'update! "valid position" 2 tm gen pos old-len str))
  (unless (and (zero? old-len) (equal? str ""))
    (async-channel-put (token-map-update-chan tm)
                       (list 'update tm gen pos old-len str))))

(define ((updater tm))
  (let loop ()
    (match (async-channel-get (token-map-update-chan tm))
      ['quit null]
      [(cons 'update args) (apply do-update! args) (loop)])))

(define (do-update! tm gen pos old-len str)
  (set-token-map-generation! tm gen)
  (set-token-map-updated-thru! tm pos)
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
  (define modes (token-map-modes tm))
  (define tokens (token-map-tokens tm))
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
  (define merely-shifted-count 0)
  (define merely-shifted-goal 3) ;2 b/c beg from prev token; +1 to be safe
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
           (set! merely-shifted-count (add1 merely-shifted-count))
           (define continue? (< merely-shifted-count merely-shifted-goal))
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
           ;; Update the main map, notify this actual change, and
           ;; definitely continue re-lexing.
           (interval-map-set! tokens beg end token)
           (set-token-map-updated-thru! tm end)
           (async-channel-put (token-map-notify-chan tm)
                              (bounds+token beg end token))
           #t])) ;continue
  (async-channel-put (token-map-notify-chan tm) 'begin)
  (tokenize-string! tm beg set-interval)
  (set-token-map-updated-thru! tm max-position)
  (async-channel-put (token-map-notify-chan tm) 'end))

;; Block until the updater thread has progressed through at least a
;; given generation and also through a given position (although the
;; latter defaults to max-position meaning the entire string has been
;; re-tokenized).
(define (block-until-updated-thru tm gen [pos max-position])
  (unless (and (>= (token-map-generation tm) gen)
               (>= (token-map-updated-thru tm) pos))
    (sleep 0.1) ;meh!
    (block-until-updated-thru tm gen pos)))

(define/contract (classify tm gen pos)
  (-> token-map? generation/c position/c (or/c #f bounds+token?))
  (block-until-updated-thru tm gen pos)
  (token-map-ref tm pos))

(define/contract (token-text tm gen pos)
  (-> token-map? generation/c position/c (or/c #f string?))
  (block-until-updated-thru tm gen pos)
  (match (token-map-ref tm pos)
    [(bounds+token _beg _end (token lexeme _)) lexeme]
    [#f #f]))

(define/contract (tokens tm gen beg end [proc values])
  (->* (token-map? generation/c position/c position/c) (procedure?) any)
  (block-until-updated-thru tm gen end)
  (match (token-map-ref tm beg)
    [(? bounds+token? b+t)
     #:when (< (bounds+token-beg b+t) end)
     (cons (proc b+t)
           (tokens tm gen (bounds+token-end b+t) end proc))]
    [_ '()]))

(define/contract (tokenize-string! tm from set-interval)
  (-> token-map? position/c (-> position/c position/c token? any) any)
  (define str (token-map-str tm))
  (define lexer (token-map-lexer tm))
  (define modes (token-map-modes tm))
  (define in (open-input-string (substring str (sub1 from))))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (set-port-next-location! in 1 0 from) ;we don't use line/col, just pos
  (let tokenize-port! ([offset from]
                       [mode   (interval-map-ref modes from #f)])
    (define-values (lexeme kind delimit beg end backup new-mode)
      (lexer in offset mode))
    (unless (eof-object? lexeme)
      (interval-map-set! modes beg end mode)
      ;; Don't trust `lexeme`; instead get from the input string.^1
      (let ([lexeme (substring str (sub1 beg) (sub1 end))])
        (when (handle-token tm set-interval mode lexeme kind delimit beg end backup)
          (tokenize-port! end new-mode))))))
;; ^1: The scribble-inside-lexer sometimes returns a value for
;; `lexeme` that is not the original lexed text. One example is
;; returning " " instead of "\n" for whitespace -- but we need that in
;; handle-white-space-token to create end-of-line tokens. Also it
;; sometimes returns 'text instead of the string for 'text tokens,
;; which would be harmless, except we want `check-valid?` to be able
;; to say that appending all token lexemes equals the input string.

;; Returns boolean? meaning, "keep going?".
(define (handle-token tm set-interval mode lexeme kind delimit beg end backup)
  (case kind
    [(white-space) (handle-white-space set-interval lexeme beg end backup)]
    [(parenthesis) (handle-parenthesis tm set-interval lexeme mode delimit beg end backup)]
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

(define (handle-parenthesis tm set-interval lexeme mode delimit beg end backup)
  (define (open close) (token:open lexeme backup close))
  (define (close open) (token:close lexeme backup open))
  (define tok
    (or (for/or ([o/c (in-list (token-map-open/close tm))])
          (match-define (list o c) o/c)
          (or (and (eq? o delimit) (open (symbol->string c)))
              (and (eq? c delimit) (close (symbol->string o)))))
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
          [_  (token:misc lexeme backup 'other)])))
  (set-interval beg end tok))

(define (mode->lexer-name mode)
  (object-name (match mode
                 [(? procedure? p)          p]
                 [(cons (? procedure? p) _) p]
                 [v                         v])))

(define (new-like-text% tm)
  (new like-text% [tm tm]))

(define like-text%
  (class object%
    (init-field tm)

    (super-new)

    ;; FIXME: Move to token-map update
    (define (temporary-slow-hack paragraph-starts)
      (define content (token-map-str tm))
      (let loop ([pos 0] [para 0] [pos-para #hasheqv()] [para-pos #hasheqv((0 . 0))])
        (cond
          [(= pos (string-length content))
           (values (hash-set pos-para pos para) para-pos)]
          [(char=? #\newline (string-ref content pos))
           (loop (add1 pos) (add1 para)
                 (hash-set pos-para pos para)
                 (hash-set para-pos (add1 para) (add1 pos)))]
          [else
           (loop (add1 pos) para (hash-set pos-para pos para) para-pos)])))

    (define/public (get-text from upto)
      (substring (token-map-str tm) from upto))

    (define/public (classify-position* pos)
      (match (or (classify tm pos)
                 (classify tm (sub1 pos))) ; make end position work
        [(bounds+token _ _ (or (? token:open?)
                               (? token:close?))) 'parenthesis]
        [(bounds+token _ _ (? token:misc? t))     (token:misc-kind t)]
        [_ (error 'classify-position "lookup failed: ~e" pos)]))

    (define/public (classify-position pos)
      (define attribs (classify-position* pos))
      (if (symbol? attribs)
          attribs
          (hash-ref attribs 'type 'unknown)))

    (define/public (get-token-range pos)
      (match (classify tm pos)
        [(bounds+token from upto _) (values from upto)]
        [_ (values #f #f)]))

    (define/public (last-position)
      (string-length (token-map-str tm)))

    (define/public (position-paragraph pos [eol? #f])
      (define-values (position-paragraphs paragraph-starts) (temporary-slow-hack))
      (or (hash-ref position-paragraphs pos #f)
          (error 'position-paragraph "lookup failed: ~e" pos)))

    (define/public (paragraph-start-position para)
      (define-values (position-paragraphs paragraph-starts) (temporary-slow-hack))
      (or (hash-ref paragraph-starts para #f)
          (error 'paragraph-start-position "lookup failed: ~e" para)))

    (define/public (paragraph-end-position para)
      (define-values (position-paragraphs paragraph-starts) (temporary-slow-hack))
      (define n (hash-ref paragraph-starts (add1 para) #f))
      (if n
          (sub1 n)
          (last-position)))

    (define/public (backward-match pos cutoff)
      (let loop ([pos (sub1 pos)] [depth -1] [need-close? #t])
        (cond
          [(pos . < . 0) #f]
          [else
           (define-values (s e) (get-token-range pos))
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (define sym (string->symbol (get-text s e)))
              (let paren-loop ([parens (token-map-open/close tm)])
                (cond
                  [(null? parens) #f]
                  [(eq? sym (caar parens))
                   (and (not need-close?)
                        (if (= depth 0)
                            s
                            (loop (sub1 s) (sub1 depth) #f)))]
                  [(eq? sym (cadar parens))
                   (loop (sub1 s) (add1 depth) #f)]
                  [else
                   (paren-loop (cdr parens))]))]
             [(whitespace comment)
              (loop (sub1 s) depth need-close?)]
             [else (if need-close?
                       s
                       (loop (sub1 s) depth #f))])])))

    (define/public (forward-match pos cutoff)
      (let loop ([pos pos] [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (define sym (string->symbol (get-text s e)))
              (let paren-loop ([parens (token-map-open/close tm)])
                (cond
                  [(null? parens) #f]
                  [(eq? sym (caar parens))
                   (loop e (add1 depth))]
                  [(eq? sym (cadar parens))
                   (if (depth . <= . 1)
                       e
                       (loop e (sub1 depth)))]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (loop e depth)])])))))

;; Having changed update! not to return updated bounds+tokens but
;; instead put to an async channel --- as well as changing create to
;; start with an empty string followed by an update! --- it's now
;; somewhat awkward to write tests. To do so, we have the
;; notify-channel that we give to create gather sequences of ('begin
;; bounds+tokens ... 'end) and post each such list to a result channel
;; for check-equal? to use.
(module+ test
  (define gathering-channel (make-async-channel))
  (define result-channel (make-async-channel))
  (void
   (thread
    (λ () (let loop ([xs null])
            (match (async-channel-get gathering-channel)
              ['begin (loop null)]
              [(? bounds+token? x) (loop (cons x xs))]
              ['end
               (async-channel-put result-channel (reverse xs))
               (loop null)])))))
  (define (test-create str)
    (begin0 (create str gathering-channel)
      (async-channel-get result-channel)))
  (define (test-update! tm gen pos old-len str)
    (update! tm gen pos old-len str)
    (async-channel-get result-channel)))

(module+ test
  (let* ([str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar"]
         ;;    1234567890123 45678901234 567890 12345678901234567890123456
         ;;             1          2          3          4         5
         [tm (test-create str)])
    (check-equal? (tokens tm 1 1 max-position)
                  (list
                   (bounds+token  1 13 (token:misc "#lang racket" 0 'other))
                   (bounds+token 13 14 (token:misc "\n" 0 'end-of-line))
                   (bounds+token 14 16 (token:misc "42" 0 'constant))
                   (bounds+token 16 17 (token:misc " " 0 'white-space))
                   (bounds+token 17 18 (token:open "(" 0 ")"))
                   (bounds+token 18 23 (token:misc "print" 0 'symbol))
                   (bounds+token 23 24 (token:misc " " 0 'white-space))
                   (bounds+token 24 31 (token:misc "\"hello\"" 0 'string))
                   (bounds+token 31 32 (token:close ")" 0 "("))
                   (bounds+token 32 33 (token:misc " " 0 'white-space))
                   (bounds+token 33 39 (token:misc "@print" 0 'symbol))
                   (bounds+token 39 40 (token:open "{" 0 "}"))
                   (bounds+token 40 45 (token:misc "Hello" 0 'symbol))
                   (bounds+token 45 46 (token:close "}" 0 "{"))
                   (bounds+token 46 47 (token:misc " " 0 'white-space))
                   (bounds+token 47 48 (token:misc "'" 0 'constant))
                   (bounds+token 48 51 (token:misc "foo" 0 'symbol))
                   (bounds+token 51 52 (token:misc " " 0 'white-space))
                   (bounds+token 52 57 (token:misc "#:bar" 0 'hash-colon-keyword))))
    (check-equal? (token-map-str tm) str)
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
    (check-equal? (test-update! tm 2 52 5 "'bar")
                  (list
                   (bounds+token 52 53 (token:misc "'" 0 'constant))
                   (bounds+token 53 56 (token:misc "bar" 0 'symbol))))
    (check-equal? (test-update! tm 3 47 4 "'bar")
                  (list (bounds+token 48 51 (token:misc "bar" 0 'symbol))))
    (check-equal? (test-update! tm 4 24 7 "'hell")
                  (list
                   (bounds+token 24 25 (token:misc "'" 0 'constant))
                   (bounds+token 25 29 (token:misc "hell" 0 'symbol))))
    (check-equal? (test-update! tm 5 14 2 "99999")
                  (list
                   (bounds+token 14 19 (token:misc "99999" 0 'constant))))
    ;; Double check final result of the edits
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
         [tm (test-create str)])
    (check-equal? (tokens tm 1 1 max-position)
                  (list
                   (bounds+token 1 13 (token:misc "#lang at-exp" 0 'other))
                   (bounds+token 13 14 (token:misc " " 0 'white-space))
                   (bounds+token 14 20 (token:misc "racket" 0 'symbol))
                   (bounds+token 20 21 (token:misc "\n" 0 'end-of-line))
                   (bounds+token 21 23 (token:misc "42" 0 'constant))
                   (bounds+token 23 24 (token:misc " " 0 'white-space))
                   (bounds+token 24 25 (token:open "(" 0 ")"))
                   (bounds+token 25 30 (token:misc "print" 0 'symbol))
                   (bounds+token 30 31 (token:misc " " 0 'white-space))
                   (bounds+token 31 38 (token:misc "\"hello\"" 0 'string))
                   (bounds+token 38 39 (token:close ")" 0 "("))
                   (bounds+token 39 40 (token:misc " " 0 'white-space))
                   (bounds+token 40 41 (token:misc "@" 0 'other))
                   (bounds+token 41 46 (token:misc "print" 0 'symbol))
                   (bounds+token 46 47 (token:open "{" 0 "}"))
                   (bounds+token 47 60 (token:misc "Hello (there)" 0 'text))
                   (bounds+token 60 61 (token:close "}" 0 "{"))
                   (bounds+token 61 62 (token:misc " " 0 'white-space))
                   (bounds+token 62 63 (token:misc "'" 0 'constant))
                   (bounds+token 63 66 (token:misc "foo" 0 'symbol))
                   (bounds+token 66 67 (token:misc " " 0 'white-space))
                   (bounds+token 67 72 (token:misc "#:bar" 0 'hash-colon-keyword))))
    (check-equal? (token-map-str tm) str)
    (check-equal? (classify tm 1 (sub1 (string-length str)))
                  (bounds+token 67 72 (token:misc "#:bar" 0 'hash-colon-keyword)))))

(module+ test
  (let* ([str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword"]
         [tm (test-create str)])
    (check-equal? (tokens tm 1 1 max-position)
                  (list
                   (bounds+token 1  20 (token:misc "#lang scribble/text" 0 'text))
                   (bounds+token 20 21 (token:misc "\n" 0 'end-of-line))
                   (bounds+token 21 27 (token:misc "Hello " 0 'text))
                   (bounds+token 27 28 (token:misc "@" 0 'other))
                   (bounds+token 28 29 (token:open "(" 0 ")"))
                   (bounds+token 29 34 (token:misc "print" 0 'symbol))
                   (bounds+token 34 35 (token:misc " " 0 'white-space))
                   (bounds+token 35 42 (token:misc "\"hello\"" 0 'string))
                   (bounds+token 42 43 (token:close ")" 0 "("))
                   (bounds+token 43 44 (token:misc " " 0 'text))
                   (bounds+token 44 45 (token:misc "@" 0 'other))
                   (bounds+token 45 50 (token:misc "print" 0 'symbol))
                   (bounds+token 50 51 (token:open "{" 0 "}"))
                   (bounds+token 51 64 (token:misc "Hello (there)" 0 'text))
                   (bounds+token 64 65 (token:close "}" 0 "{"))
                   (bounds+token 65 81 (token:misc " #:not-a-keyword" 0 'text))))
    (check-equal? (token-map-str tm) str)))

(module+ test
  (let* ([str "#lang racket\n(λ () #t)"]
         [tm  (test-create str)])
    (check-equal? (classify tm 1 15)
                  (bounds+token 15 16 (token:misc "λ" 0 'symbol)))
    (check-equal? (test-update! tm 2 18 0 "a")
                  (list
                   (bounds+token 18 19 (token:misc "a" 0 'symbol))))
    (check-equal? (classify tm 2 18)
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
         [tm (test-create str)])
    (test-update! tm 2 14 0 "d")
    (test-update! tm 3 15 0 "o")
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
         [tm (test-create str)])
    (test-update! tm 2 14 0 "1") ;initially lexed as 'constant
    (test-update! tm 3 15 0 "x") ;should re-lex "1x" as 'symbol
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
         [tm (test-create str)])
    (test-update! tm 2 14 0 "1") ;initially lexed as 'constant
    (test-update! tm 3 15 0 "x") ;should re-lex "1x" as 'symbol
    (test-update! tm 4 16 0 "1") ;still symbol
    (test-update! tm 5 15 1 "")  ;deleting the "x" should re-lex the "11" as constant
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
         [tm (test-create str)])
    ;; as if paredit etc. were enabled
    (test-update! tm 2 14 0 "(")
    (test-update! tm 3 15 0 ")")
    (test-update! tm 4 15 0 "h")
    (test-update! tm 5 16 0 "i")
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
         [tm (test-create str)])
    (check-equal? (token-map-ref tm 14)
                  (bounds+token 14 19 (token:misc "#hash" 0 'error)))
    (check-equal? (test-update! tm 2 19 0 "(")
                  (list (bounds+token 14 20 (token:open "#hash(" 0 ")")))
                  "Adding parens after #hash re-lexes from an error to an open")
    (check-equal? (token-map-ref tm 14)
                  (bounds+token 14 20 (token:open "#hash(" 0 ")")))))

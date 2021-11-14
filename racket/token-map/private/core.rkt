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
         racket/contract/option
         racket/dict
         racket/match
         syntax-color/module-lexer)

(provide hash-lang%
         (struct-out bounds+token)
         (struct-out token)
         generation/c
         position/c
         max-position)

(module+ test (require rackunit))

;; To coordinate inter-process updates and queries we use a monotonic
;; "generation". A new object is generation 0. Thereafter the client
;; should increment the generation for every call to update!. Then,
;; when it needs to use some query operation such as classify, it
;; supplies both its latest generation and the position. The query op
;; will block until/unless we have finished updating (a) that
;; generation (b) through that position.
(define generation/c exact-nonnegative-integer?)

;; Our interface uses 1-based positions -- as do lexers and Emacs.
(define position/c exact-positive-integer?)
(define max-position (sub1 (expt 2 63)))

(define hash-lang%
  (class object%
    (super-new)
    (init-field notify-chan)
    ;; We create with `str` empty. Caller should use update! to set
    ;; the initial string value. That way we tokenize on the updater
    ;; thread. That way both `new` and `update!` return immediately
    ;; and we are coordinated with subsequent `update!'s.
    (define str               "")
    (define generation        0)
    (define tokens            (make-interval-map))
    (define modes             (make-interval-map))
    (define update-chan       (make-async-channel))
    (define updated-thru      0)
    (define lexer             default-lexer)
    (define paren-matches     default-paren-matches)
    (define quote-matches     default-quote-matches)
    (define grouping-position default-grouping-position)
    (define line-indenter     default-line-indenter)
    (define range-indenter    default-range-indenter)

    ;; This must called from update! because #lang could have changed
    ;; and we may might have new values for all of these. Although it
    ;; might be unnecessary, and slow, to call this on every single
    ;; update!, that is the safest thing to do for now.
    (define/private (refresh-lang-info!)
      (define info
        (or (with-handlers ([values (λ _ #f)])
              (read-language (open-input-string str)
                             (λ _ #f)))
            (λ (_key default) default)))
      (set! lexer (info 'color-lexer (waive-option module-lexer)))
      (set! paren-matches (info 'drracket:paren-matches default-paren-matches))
      (set! quote-matches (info 'drracket:quote-matches default-quote-matches))
      (set! grouping-position (info 'drracket:grouping-position default-grouping-position))
      (set! line-indenter (info 'drracket:indentation default-line-indenter))
      (set! range-indenter (info 'drracket:range-indentation default-range-indenter)))

    ;; These accessor methods really intended just for tests
    (define/public (-get-string) str)
    (define/public (-get-modes) modes)
    (define/public (-get-tokens) tokens)
    (define/public (-get-line-indenter) line-indenter)

    (define/public (delete)
      (async-channel-put update-chan 'quit))

    (define/public (token-ref pos) ;; temporarily public for debugging
      (and pos
           (let-values ([(beg end token)
                         (interval-map-ref/bounds tokens pos #f)])
             (and beg end token
                  (bounds+token beg end token)))))

    ;; The method signature here is similar to that of Emacs'
    ;; after-change functions: Something changed starting at POS. The text
    ;; there used to be OLD-LEN chars long, but is now STR.
    (define/public (update! gen pos old-len new-str)
      ;;(-> generation/c position/c exact-nonnegative-integer? string? any)
      (unless (< generation gen)
        (raise-argument-error 'update! "valid generation" 0 gen pos old-len new-str))
      (unless (and (<= 1 pos)
                   #;
                   (<= (sub1 pos) (string-length (token-map-str tm))))
        (raise-argument-error 'update! "valid position" 1 gen pos old-len new-str))
      (unless (and (zero? old-len) (equal? new-str ""))
        (async-channel-put update-chan
                           (list 'update gen pos old-len new-str))))

    (define (consume-update-chan)
      (match (async-channel-get update-chan)
        ['quit null] ;let thread exit/die
        [(list 'update gen pos old-len new-str)
         (do-update! gen pos old-len new-str)
         (consume-update-chan)]))
    (thread consume-update-chan)

    (define/public (do-update! gen pos old-len new-str)
      (set! generation gen)
      (set! updated-thru pos)
      (set! str
            (string-append (substring str 0 (sub1 pos))
                           new-str
                           (substring str (+ (sub1 pos) old-len))))
      (refresh-lang-info!) ;from new value of `str`
      (define diff (- (string-length new-str) old-len))
      ;; From where do we need to re-tokenize? This will be < the pos of
      ;; the change. Back up to the start of the previous token (plus any
      ;; `backup` amount the lexer may have supplied for that token) to
      ;; ensure re-lexing enough such that e.g. appending a character does
      ;; not create a separate token when instead it should be combined
      ;; with an existing token for preceding character(s).
      (define beg
        (match (token-ref (sub1 pos))
          [(bounds+token beg _end (token _ _ _ backup)) (- beg backup)]
          [#f pos]))
      ;; Expand/contract the tokens and modes interval-maps.
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
               (set! updated-thru end)
               (async-channel-put notify-chan
                                  (bounds+token beg end token))
               #t])) ;continue
      (async-channel-put notify-chan 'begin)
      (tokenize-string! beg set-interval)
      (set! updated-thru max-position)
      (async-channel-put notify-chan 'end))

    (define/private (tokenize-string! from set-interval)
      (define in (open-input-string (substring str (sub1 from))))
      (port-count-lines! in) ;important for Unicode e.g. λ
      (set-port-next-location! in 1 0 from) ;we don't use line/col, just pos
      (let tokenize-port! ([offset from]
                           [mode   (interval-map-ref modes from #f)])
        (define-values (lexeme type paren beg end backup new-mode)
          (lexer in offset mode))
        (unless (eof-object? lexeme)
          (interval-map-set! modes beg end mode)
          ;; Don't trust `lexeme`; instead get from the input string.
          (let ([lexeme (substring str (sub1 beg) (sub1 end))])
            (when (set-interval beg end (token lexeme type paren backup))
              (tokenize-port! end new-mode))))))

    ;; Block until the updater thread has progressed through at least a
    ;; given generation and also through a given position (although the
    ;; latter defaults to max-position meaning the entire string has been
    ;; re-tokenized).
    (define/public (block-until-updated-thru gen [pos max-position])
      (unless (and (>= generation gen)
                   (>= updated-thru pos))
        (sleep 0.1) ;meh!
        (block-until-updated-thru gen pos)))

    (define/public (classify gen pos)
      ;; (-> generation/c position/c (or/c #f bounds+token?))
      (block-until-updated-thru gen pos)
      (token-ref pos))

    (define/public (token-text gen pos)
      ;; (-> generation/c position/c (or/c #f string?))
      (block-until-updated-thru gen pos)
      (match (token-ref pos)
        [(bounds+token _beg _end (token lexeme _ _ _)) lexeme]
        [#f #f]))

    (define/public (get-tokens [gen generation] [beg 1] [end max-position] [proc values])
      ;; (->* (generation/c position/c position/c) (procedure?) any)
      (block-until-updated-thru gen end)
      (match (token-ref beg)
        [(? bounds+token? b+t)
         #:when (< (bounds+token-beg b+t) end)
         (cons (proc b+t)
               (get-tokens gen (bounds+token-end b+t) end proc))]
        [_ '()]))

    ;;; Something for Emacs forward-sexp-function etc.

    (define/public (grouping gen pos dir limit count)
      (cond
        [(zero? count) pos]
        [else
         (block-until-updated-thru gen
                                   (case dir
                                     [(up backward) 1]
                                     [(down forward) max-position]))
         (let loop ([pos pos]
                    [count count])
           (match (grouping-position this pos limit dir)
             [#f #f]
             [#t 'use-default-s-expression]
             [(? number? new-pos)
              (if (= count 1)
                  new-pos
                  (loop new-pos (sub1 count)))]))]))

    ;;; Indent

    (define/public (indent-line-amount gen pos)
      (block-until-updated-thru gen pos)
      (line-indenter this pos))

    (define/public (indent-region-amounts gen from upto)
      (block-until-updated-thru gen upto)
      (range-indenter this from upto))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Subset of text% methods needed by drracket:indentation and
    ;;; drracket:range-indentation.
    ;;;
    ;;; 1. These use 0-based positions, not 1-based like the rest of
    ;;; our code.
    ;;;
    ;;; 2. Obviously these method signatures do not include any
    ;;; generation argument. However that should be OK because they're
    ;;; called only indirectly, via indent-line-amount or
    ;;; indent-region-amounts (which do take an expected generation
    ;;; and block if necessary), which call drracket:indentation or
    ;;; drracket:range-indentation supplying this object for these
    ;;; methods.

    ;; NEW: This was not in expeditor like-text% but I found that
    ;; scribble's determine-spaces calls it.
    (define/public (get-character pos)
      (if (< pos (string-length str))
          (string-ref str pos)
          #\nul))

    ;; NEW: This was not in expeditor like-text% but I found that
    ;; scribble's determine-spaces calls it.
    (define/public (find-up-sexp pos)
      (if (< pos (string-length str))
          (add1 pos) ;; FIXME
          #f))

    (define/public (get-text from upto)
      (substring str from upto))

    (define/private (get-token who pos)
      (let ([pos (add1 pos)])
        (match (or (token-ref pos)
                   #;(token-ref (sub1 pos))
                   ) ;make end position work
          [(bounds+token _ _ (? token? token)) token]
          [_ (error who "lookup failed: ~e" (sub1 pos))])))

    (define/public (classify-position* pos)
      (token-type (get-token 'classify-position* pos)))

    (define/public (classify-position pos)
      (define attribs (classify-position* pos))
      (if (symbol? attribs)
          attribs
          (hash-ref attribs 'type 'unknown)))

    (define/private (get-paren pos)
      (token-paren (get-token 'get-paren pos)))

    (define/public (get-token-range pos)
      (match (token-ref (add1 pos))
        [(bounds+token from upto _) (values (sub1 from) (sub1 upto))]
        [_ (values #f #f)]))

    (define/public (last-position)
      (string-length str))

    ;; FIXME: Move equivalent to `update!`
    (define/private (temporary-slow-hack)
      (let loop ([pos 0] [para 0] [pos-para #hasheqv()] [para-pos #hasheqv((0 . 0))])
        (cond
          [(= pos (string-length str))
           (values (hash-set pos-para pos para) para-pos)]
          [(char=? #\newline (string-ref str pos))
           (loop (add1 pos) (add1 para)
                 (hash-set pos-para pos para)
                 (hash-set para-pos (add1 para) (add1 pos)))]
          [else
           (loop (add1 pos) para (hash-set pos-para pos para) para-pos)])))

    (define/public (position-paragraph pos [eol? #f])
      (define-values (position-paragraphs _paragraph-starts) (temporary-slow-hack))
      (or (hash-ref position-paragraphs pos #f)
          (error 'position-paragraph "lookup failed: ~e" pos)))

    (define/public (paragraph-start-position para)
      (define-values (_position-paragraphs paragraph-starts) (temporary-slow-hack))
      (or (hash-ref paragraph-starts para #f)
          (error 'paragraph-start-position "lookup failed: ~e" para)))

    (define/public (paragraph-end-position para)
      (define-values (_position-paragraphs paragraph-starts) (temporary-slow-hack))
      (define n (hash-ref paragraph-starts (add1 para) #f))
      (if n
          (sub1 n)
          (last-position)))

    (define/public (skip-whitespace pos direction comments?)
      (let loop ([pos pos])
        (cond [(and (eq? direction 'forward)
                    (>= pos (last-position)))
               pos]
              [(and (eq? direction 'backward)
                    (<= pos 0))
               pos]
              [else
               (define token-pos (if (eq? direction 'forward) pos (sub1 pos)))
               (define-values (s e) (get-token-range token-pos))
               (define type (classify-position token-pos))
               (cond [(or (eq? type 'white-space)
                          (and comments? (eq? type 'comment)))
                      (loop (if (eq? direction 'forward)
                                e
                                s))]
                     [else pos])])))

    (define/public (backward-match pos _cutoff)
      (let loop ([pos (skip-whitespace (sub1 pos) 'backward #t)]
                 [depth -1]
                 [need-close? #t])
        (cond
          [(pos . < . 0) #f]
          [else
           (define-values (s e) (get-token-range pos))
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (define sym (get-paren pos))
              (let paren-loop ([parens paren-matches])
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

    (define/public (forward-match pos _cutoff)
      (let loop ([pos (skip-whitespace pos 'forward #t)]
                 [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define category (classify-position pos))
           (case category
             [(parenthesis)
              (define sym (get-paren s))
              (let paren-loop ([parens paren-matches])
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

(define default-lexer (waive-option module-lexer))
(define default-paren-matches `((,(string->symbol "(") ,(string->symbol ")"))
                                (,(string->symbol "[") ,(string->symbol "]"))
                                (,(string->symbol "{") ,(string->symbol "}"))))
(define default-quote-matches '(#\" #\|))
(define (default-grouping-position _obj _start _limit _direction) #t)
(define (default-line-indenter _text-like% _pos) #f)
(define (default-range-indenter _text-like% _from _upto) #f)

(struct token (lexeme type paren backup) #:transparent)

;; A bounds+token represents a token in an interval-map -- i.e. it is
;; interval-map-ref/bounds represented as one value not three.
(struct bounds+token (beg end token) #:transparent)

(define (mode->lexer-name mode)
  (object-name (match mode
                 [(? procedure? p)          p]
                 [(cons (? procedure? p) _) p]
                 [v                         v])))

;; Having changed update! not to return updated bounds+tokens but
;; instead put to an async channel --- as well as changing create to
;; start with an empty string followed by an update! --- it's now
;; somewhat awkward to write tests. To do so, we have our notify
;; channel, that we give to the object, gather sequences of ('begin
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
    (define o (new hash-lang% [notify-chan gathering-channel]))
    (test-update! o 1 1 0 str)
    o)
  (define (test-update! o gen pos old-len str)
    (send o update! gen pos old-len str)
    (async-channel-get result-channel)))

(module+ test
  (define (sym ch)
    (string->symbol (string ch))))

(module+ test
  (let* ([str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar"]
         ;;    1234567890123 45678901234 567890 12345678901234567890123456
         ;;             1          2          3          4         5
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (bounds+token  1 13 (token "#lang racket" 'other #f 0))
                   (bounds+token 13 14 (token "\n" 'white-space #f 0))
                   (bounds+token 14 16 (token "42" 'constant #f 0))
                   (bounds+token 16 17 (token " " 'white-space #f 0))
                   (bounds+token 17 18 (token "(" 'parenthesis (sym #\() 0))
                   (bounds+token 18 23 (token "print" 'symbol #f 0))
                   (bounds+token 23 24 (token " " 'white-space #f 0))
                   (bounds+token 24 31 (token "\"hello\"" 'string #f 0))
                   (bounds+token 31 32 (token ")" 'parenthesis (sym #\)) 0))
                   (bounds+token 32 33 (token " " 'white-space #f 0))
                   (bounds+token 33 39 (token "@print" 'symbol #f 0))
                   (bounds+token 39 40 (token "{" 'parenthesis (sym #\{) 0))
                   (bounds+token 40 45 (token "Hello" 'symbol #f 0))
                   (bounds+token 45 46 (token "}" 'parenthesis (sym #\}) 0))
                   (bounds+token 46 47 (token " " 'white-space #f 0))
                   (bounds+token 47 48 (token "'" 'constant #f 0))
                   (bounds+token 48 51 (token "foo" 'symbol #f 0))
                   (bounds+token 51 52 (token " " 'white-space #f 0))
                   (bounds+token 52 57 (token "#:bar" 'hash-colon-keyword #f 0))))
    (check-equal? (send tm -get-string) str)
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
                  (list
                   (bounds+token 52 53 (token "'" 'constant #f 0))
                   (bounds+token 53 56 (token "bar" 'symbol #f 0))))
    (check-equal? (test-update! tm 3 47 4 "'bar")
                  (list (bounds+token 48 51 (token "bar" 'symbol #f 0))))
    (check-equal? (test-update! tm 4 24 7 "'hell")
                  (list
                   (bounds+token 24 25 (token "'" 'constant #f 0))
                   (bounds+token 25 29 (token "hell" 'symbol #f 0))))
    (check-equal? (test-update! tm 5 14 2 "99999")
                  (list
                   (bounds+token 14 19 (token "99999" 'constant #f 0))))
    ;; Double check final result of the edits
    (check-equal? (send tm -get-string)
                  "#lang racket\n99999 (print 'hell) @print{Hello} 'bar 'bar")
    (check-equal? (dict->list (send tm -get-tokens))
                  (list
                   (cons '(1 . 13)  (token "#lang racket" 'other #f 0))
                   (cons '(13 . 14) (token "\n" 'white-space #f 0))
                   (cons '(14 . 19) (token "99999" 'constant #f 0))
                   (cons '(19 . 20) (token " " 'white-space #f 0))
                   (cons '(20 . 21) (token "(" 'parenthesis (sym #\() 0))
                   (cons '(21 . 26) (token "print" 'symbol #f 0))
                   (cons '(26 . 27) (token " " 'white-space #f 0))
                   (cons '(27 . 28) (token "'" 'constant #f 0))
                   (cons '(28 . 32) (token "hell" 'symbol #f 0))
                   (cons '(32 . 33) (token ")" 'parenthesis (sym #\)) 0))
                   (cons '(33 . 34) (token " " 'white-space #f 0))
                   (cons '(34 . 40) (token "@print" 'symbol #f 0))
                   (cons '(40 . 41) (token "{" 'parenthesis (sym #\{) 0))
                   (cons '(41 . 46) (token "Hello" 'symbol #f 0))
                   (cons '(46 . 47) (token "}" 'parenthesis (sym #\}) 0))
                   (cons '(47 . 48) (token " " 'white-space #f 0))
                   (cons '(48 . 49) (token "'" 'constant #f 0))
                   (cons '(49 . 52) (token "bar" 'symbol #f 0))
                   (cons '(52 . 53) (token " " 'white-space #f 0))
                   (cons '(53 . 54) (token "'" 'constant #f 0))
                   (cons '(54 . 57) (token "bar" 'symbol #f 0))))
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
                    ((54 . 57) . ,racket-lexer)))))

(module+ test
  (let* ([str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar"]
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (bounds+token  1 13 (token "#lang at-exp" 'other #f 0))
                   (bounds+token 13 14 (token " " 'white-space #f 0))
                   (bounds+token 14 20 (token "racket" 'symbol #f 0))
                   (bounds+token 20 21 (token "\n" 'white-space #f 0))
                   (bounds+token 21 23 (token "42" 'constant #f 0))
                   (bounds+token 23 24 (token " " 'white-space #f 0))
                   (bounds+token 24 25 (token "(" 'parenthesis (sym #\() 0))
                   (bounds+token 25 30 (token "print" 'symbol #f 0))
                   (bounds+token 30 31 (token " " 'white-space #f 0))
                   (bounds+token 31 38 (token "\"hello\"" 'string #f 0))
                   (bounds+token 38 39 (token ")" 'parenthesis (sym #\)) 0))
                   (bounds+token 39 40 (token " " 'white-space #f 0))
                   (bounds+token 40 41 (token "@" 'parenthesis #f 0)) ;;??
                   (bounds+token 41 46 (token "print" 'symbol #f 0))
                   (bounds+token 46 47 (token "{" 'parenthesis (sym #\{) 0))
                   (bounds+token 47 60 (token "Hello (there)" 'text #f 0))
                   (bounds+token 60 61 (token "}" 'parenthesis (sym #\}) 0))
                   (bounds+token 61 62 (token " " 'white-space #f 0))
                   (bounds+token 62 63 (token "'" 'constant #f 0))
                   (bounds+token 63 66 (token "foo" 'symbol #f 0))
                   (bounds+token 66 67 (token " " 'white-space #f 0))
                   (bounds+token 67 72 (token "#:bar" 'hash-colon-keyword #f 0))))
    (check-equal? (send tm -get-string) str)
    (check-equal? (send tm classify 1 (sub1 (string-length str)))
                  (bounds+token 67 72 (token "#:bar" 'hash-colon-keyword #f 0)))))

(module+ test
  (let* ([str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword"]
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (bounds+token 1  20 (token "#lang scribble/text" 'text #f 0))
                   (bounds+token 20 21 (token "\n" 'white-space #f 0))
                   (bounds+token 21 27 (token "Hello " 'text #f 0))
                   (bounds+token 27 28 (token "@" 'parenthesis #f 0)) ;;??
                   (bounds+token 28 29 (token "(" 'parenthesis (sym #\() 0))
                   (bounds+token 29 34 (token "print" 'symbol #f 0))
                   (bounds+token 34 35 (token " " 'white-space #f 0))
                   (bounds+token 35 42 (token "\"hello\"" 'string #f 0))
                   (bounds+token 42 43 (token ")" 'parenthesis (sym #\)) 0))
                   (bounds+token 43 44 (token " " 'text #f 0))
                   (bounds+token 44 45 (token "@" 'parenthesis #f 0))
                   (bounds+token 45 50 (token "print" 'symbol #f 0))
                   (bounds+token 50 51 (token "{" 'parenthesis (sym #\{) 0))
                   (bounds+token 51 64 (token "Hello (there)" 'text #f 0))
                   (bounds+token 64 65 (token "}" 'parenthesis (sym #\}) 0))
                   (bounds+token 65 81 (token " #:not-a-keyword" 'text #f 0))))
    (check-equal? (send tm -get-string) str)))

(module+ test
  (let* ([str "#lang racket\n(λ () #t)"]
         [tm  (test-create str)])
    (check-equal? (send tm classify 1 15)
                  (bounds+token 15 16 (token "λ" 'symbol #f 0)))
    (check-equal? (test-update! tm 2 18 0 "a")
                  (list
                   (bounds+token 18 19 (token "a" 'symbol #f 0))))
    (check-equal? (send tm classify 2 18)
                  (bounds+token 18 19 (token "a" 'symbol #f 0)))))

(module+ test
  (let ([o (test-create "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")])
    (check-equal? (send o get-tokens)
                  (list
                   (bounds+token  1 13 (token "#lang racket" 'other #f 0))
                   (bounds+token 13 14 (token "\n" 'white-space #f 0))
                   (bounds+token 14 23 (token "#rx\"1234\"" 'string #f 0))
                   (bounds+token 23 24 (token "\n" 'white-space #f 0))
                   (bounds+token 24 26 (token "#(" 'parenthesis (sym #\() 0))
                   (bounds+token 26 27 (token "1" 'constant #f 0))
                   (bounds+token 27 28 (token " " 'white-space #f 0))
                   (bounds+token 28 29 (token "2" 'constant #f 0))
                   (bounds+token 29 30 (token " " 'white-space #f 0))
                   (bounds+token 30 31 (token "3" 'constant #f 0))
                   (bounds+token 31 32 (token ")" 'parenthesis (sym #\)) 0))
                   (bounds+token 32 33 (token "\n" 'white-space #f 0))
                   (bounds+token 33 35 (token "#'" 'constant #f 0))
                   (bounds+token 35 36 (token "(" 'parenthesis (sym #\() 0))
                   (bounds+token 36 37 (token "1" 'constant #f 0))
                   (bounds+token 37 38 (token " " 'white-space #f 0))
                   (bounds+token 38 39 (token "2" 'constant #f 0))
                   (bounds+token 39 40 (token " " 'white-space #f 0))
                   (bounds+token 40 41 (token "3" 'constant #f 0))
                   (bounds+token 41 42 (token ")" 'parenthesis (sym #\)) 0))))))

(module+ test
  (let ([o (test-create "#lang racket\n#<<HERE\nblah blah\nblah blah\nHERE\n")])
    (check-equal? (send o get-tokens)
                  (list
                   (bounds+token  1 13 (token "#lang racket" 'other #f 0))
                   (bounds+token 13 14 (token "\n" 'white-space #f 0))
                   (bounds+token 14 46 (token "#<<HERE\nblah blah\nblah blah\nHERE" 'string #f 0))
                   (bounds+token 46 47 (token "\n" 'white-space #f 0))))))

(module+ test
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
    (void)))

(module+ test
  (require syntax-color/racket-lexer)
  (let* ([str "#lang racket\n"]
         ;;    1234567890123 4
         ;;             1
         [tm (test-create str)])
    (test-update! tm 2 14 0 "d")
    (test-update! tm 3 15 0 "o")
    (check-equal? (send tm -get-string) "#lang racket\ndo")
    (check-equal? (dict->list (send tm -get-tokens))
                  (list
                   (cons '(1 . 13) (token "#lang racket" 'other #f 0))
                   (cons '(13 . 14) (token "\n" 'white-space #f 0))
                   (cons '(14 . 16) (token "do" 'symbol #f 0))))
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
    (check-equal? (send tm -get-string) "#lang racket\n1x")
    (check-equal? (dict->list (send tm -get-tokens))
                  (list
                   (cons '(1 . 13) (token "#lang racket" 'other #f 0))
                   (cons '(13 . 14) (token "\n" 'white-space #f 0))
                   (cons '(14 . 16) (token "1x" 'symbol #f 0))))
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
    (check-equal? (send tm -get-string) "#lang racket\n11")
    (check-equal? (dict->list (send tm -get-tokens))
                  (list
                   (cons '(1 . 13) (token "#lang racket" 'other #f 0))
                   (cons '(13 . 14) (token "\n" 'white-space #f 0))
                   (cons '(14 . 16) (token "11" 'constant #f 0))))
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
    (check-equal? (send tm -get-string) "#lang racket\n(hi)")
    (check-equal? (dict->list (send tm -get-tokens))
                  (list
                   (cons '(1 . 13) (token "#lang racket" 'other #f 0))
                   (cons '(13 . 14) (token "\n" 'white-space #f 0))
                   (cons '(14 . 15) (token "(" 'parenthesis (sym #\() 0))
                   (cons '(15 . 17) (token "hi" 'symbol #f 0))
                   (cons '(17 . 18) (token ")" 'parenthesis (sym #\)) 0))))
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
                  (bounds+token 14 19 (token "#hash" 'error #f 0)))
    (check-equal? (test-update! tm 2 19 0 "(")
                  (list (bounds+token 14 20 (token "#hash(" 'parenthesis (sym #\() 0)))
                  "Adding parens after #hash re-lexes from an error to an open")
    (check-equal? (send tm classify 2 14)
                  (bounds+token 14 20 (token "#hash(" 'parenthesis (sym #\() 0)))))

;; Test equivalance of our text%-like methods
(module+ test
  (require racket/gui/base
           framework)
  (define (insert t str)
    (define lp (send t last-position))
    (send t insert str lp lp)
    (send t freeze-colorer)
    (send t thaw-colorer))

  ;; text% interface; note uses 0-based positions
  (let ()
    (define str "#lang racket\n(1) #(2) #hash((1 . 2))\n@racket[]{\n#(2)\n}\n")
    ;;           01234567890123456789 012345678901234567890123 4567890123 456789 01 23456
    ;;                     1          2         3         4          5           6
    (define o (test-create str))
    (define t (new racket:text%))
    (send t start-colorer symbol->string default-lexer default-paren-matches)
    (insert t str)
    (check-equal? (send o last-position)
                  (send t last-position))

    ;; Test that our implementation of skip-whitespace is equivalent
    ;; to that of racket:text%.
    (for ([pos (in-range 0 (string-length str))])
      (check-equal? (send o skip-whitespace pos 'forward #t)
                    (send t skip-whitespace pos 'forward #t)
                    (format "skip-whitespace ~v 'forward" pos))
      (check-equal? (send o skip-whitespace pos 'backward #t)
                    (send t skip-whitespace pos 'backward #t)
                    (format "skip-whitespace ~v 'backward" pos)))

    ;; Test that our implementations of {forward backward}-match are
    ;; equivalent to those of racket:text%.
    (define lp (string-length str))
    ;; FIXME: These tests currently mostly fail. I'm not yet sure if
    ;; that's because mflatt did a subset of behavior needed by
    ;; indenters, or due to some other problem.
    #;
    (for ([pos (in-range 0 (string-length str))])
      (check-equal? (send o forward-match pos lp)
                    (send t forward-match pos lp)
                    (format "forward-match ~v" pos))
      (check-equal? (send o backward-match pos lp)
                    (send t backward-match pos lp)
                    (format "backward-match ~v" pos)))

    ;; Test that we supply enough entire color-text% methods, and that
    ;; they behave equivalently to from racket-text%, as needed by a
    ;; lang-supplied drracket:indentation a.k.a. determine-spaces
    ;; function. (After all, this is the motivation to provide
    ;; text%-like methods; otherwise we wouldn't bother.)
    (define determine-spaces (send o -get-line-indenter))
    #;
    (for ([pos (in-range 0 (string-length str))])
      (check-equal? (determine-spaces o pos)
                    (determine-spaces t pos)
                    (format "~v ~v" determine-spaces pos)))
    (void)))

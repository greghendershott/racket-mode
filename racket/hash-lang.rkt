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
(define min-position 1)
(define max-position (sub1 (expt 2 63)))

(define hash-lang%
  (class object%
    (super-new)
    (init-field notify-chan)
    ;; A new object has an empty string and is at generation 0. The
    ;; creator should then use update! to set a string value. That way
    ;; both `new` and `update!` return immediately; all
    ;; (re)tokenization is handled uniformly on the dedicated updater
    ;; thread.
    (define str               "")
    (define generation        0)
    (define tokens            (make-interval-map))
    (define modes             (make-interval-map))
    (define update-chan       (make-async-channel))
    (define updated-thru      0)
    ;; These members correspond to various lang-info items.
    (define lexer             default-lexer)
    (define paren-matches     default-paren-matches)
    (define quote-matches     default-quote-matches)
    (define grouping-position default-grouping-position)
    (define line-indenter     default-line-indenter)
    (define range-indenter    default-range-indenter)

    ;; This must called from update! because #lang could have changed
    ;; and we may might have new values for all of these. Although it
    ;; might be unnecessary, and slow, to call this on every single
    ;; update! -- even beyond the #lang near the very start -- that is
    ;; the safest thing to do for now.
    (define/private (refresh-lang-info!)
      (define info
        (or (with-handlers ([values (λ _ #f)])
              (read-language (open-input-string str) (λ _ #f)))
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
    (define/public (-get-lexer) lexer)
    (define/public (-get-paren-matches) paren-matches)
    (define/public (-get-line-indenter) line-indenter)

    (define/public (delete)
      (async-channel-put update-chan 'quit))

    ;; (or/c #f position/c) -> (or/c #f (list/c position/c position/c token?))
    (define/private (token-ref pos)
      (and pos
           (let/ec k
             (call-with-values
              (λ () (interval-map-ref/bounds tokens pos (λ () (k #f))))
              list))))

    ;; The method signature here is similar to that of Emacs'
    ;; after-change functions: Something changed starting at POS. The text
    ;; there used to be OLD-LEN chars long, but is now STR.
    (define/public (update! gen pos old-len new-str)
      ;;(-> generation/c position/c exact-nonnegative-integer? string? any)
      (unless (< generation gen)
        (raise-argument-error 'update! "valid generation" 0 gen pos old-len new-str))
      (unless (<= min-position pos)
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
          [(list beg _end (token _ _ _ backup)) (- beg backup)]
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
                                  (make-notify-channel-value beg end token))
               #t])) ;continue
      (async-channel-put notify-chan 'begin)
      (tokenize-string! beg set-interval)
      (set! updated-thru max-position)
      (async-channel-put notify-chan 'end))

    ;; Produce a value convenient for Emacs to use as a notification.
    ;; Specifically parenthesis tokens get extra data: An open? flag
    ;; and the symbol for the matching open or close.
    (define/private (make-notify-channel-value beg end token)
      (define ht-or-type (token-type token))
      (define type (if (symbol? ht-or-type)
                       ht-or-type
                       (hash-ref ht-or-type 'type 'unknown)))
      (define paren (token-paren token))
      (list* beg
             end
             type
             (if paren
                 (or (for/or ([pm (in-list paren-matches)])
                       (match-define (list open close) pm)
                       (cond [(eq? paren open)
                              (list #t (symbol->string close))]
                             [(eq? paren close)
                              (list #f (symbol->string open))]
                             [else #f]))
                     null)
                 null)))

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
      ;; (-> generation/c position/c (or/c #f (list/c position/c position/c))
      (block-until-updated-thru gen pos)
      (token-ref pos))

    (define/public (get-tokens [gen generation]
                               [from min-position]
                               [upto max-position]
                               [proc values])
      (block-until-updated-thru gen upto)
      (match (token-ref from)
        [(and v (list beg end _token))
         #:when (< beg upto)
         (cons (proc v)
               (get-tokens gen end upto proc))]
        [_ '()]))

    ;;; Something for Emacs forward-sexp-function etc.

    (define/public (grouping gen pos dir limit count)
      (cond
        [(<= count 0) pos]
        [else
         (block-until-updated-thru gen
                                   (case dir
                                     [(up backward) min-position]
                                     [(down forward) max-position]))
         (let loop ([pos (sub1 pos)] ;1.. -> 0..
                    [count count])
           (define (failure-result) ;can't/didn't move
             (call-with-values (λ () (get-token-range pos)) list))
           (match (grouping-position this pos limit dir)
             [#f (failure-result)]
             [#t 'use-default-s-expression]
             [(? number? new-pos)
              (cond [(< 1 count) (loop new-pos (sub1 count))]
                    [(= new-pos pos) (failure-result)]
                    [else (add1 new-pos)])]))])) ;0.. -> 1..

    ;;; Indent

    (define/public (indent-line-amount gen pos)
      (block-until-updated-thru gen pos)
      (line-indenter this (sub1 pos))) ;1.. -> 0..

    (define/public (indent-region-amounts gen from upto)
      (block-until-updated-thru gen upto)
      (range-indenter this (sub1 from) (sub1 upto))) ;1.. -> 0..

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

    (define/public (get-character pos)
      (if (< pos (string-length str))
          (string-ref str pos)
          #\nul))

    ;; ;; I think this is needed by at-exp determine-spaces.
    ;; (define/public (find-up-sexp pos)
    ;;   (if (< pos (string-length str))
    ;;       (add1 pos) ;; FIXME
    ;;       #f))

    (define/public (get-text from upto)
      (substring str from upto))

    (define/private (get-token who pos)
      (let ([pos (add1 pos)])
        (match (or (token-ref pos)
                   (token-ref (sub1 pos))) ;make end position work
          [(list _ _ (? token? token)) token]
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
        [(list from upto _token) (values (sub1 from) (sub1 upto))]
        [_ (values #f #f)]))

    (define/public (last-position)
      (string-length str))

    ;; Note: Not attempting to maintain a data structure in do-update!
    ;; for paragraphs; just calculating on-demand.

    (define/public (position-paragraph desired-pos [eol? #f])
      (let loop ([pos 0] [para 0])
        (cond [(= pos desired-pos) para]
              [(= pos (string-length str)) para]
              [(char=? #\newline (string-ref str pos))
               (loop (add1 pos) (add1 para))]
              [else
               (loop (add1 pos) para)])))

    (define/public (paragraph-start-position desired-para)
      (let loop ([pos 0] [para 0])
        (cond [(= para desired-para) pos]
              [(= pos (string-length str))
               (error 'paragraph-start-position "lookup failed: ~e" desired-para)]
              [(char=? #\newline (string-ref str pos))
               (loop (add1 pos) (add1 para))]
              [else
               (loop (add1 pos) para)])))

    (define/public (paragraph-end-position desired-para)
      (let loop ([pos 0] [para -1])
        (cond [(= para desired-para) (sub1 pos)]
              [(= pos (string-length str)) pos]
              [(char=? #\newline (string-ref str pos))
               (loop (add1 pos) (add1 para))]
              [else
               (loop (add1 pos) para)])))

    ;; Faster alternative to the "paragraphs" methods, for use by indenters.

    (define/public (beginning-of-line pos)
      (define len (string-length str))
      (let loop ([pos pos])
        (cond [(<= pos 0) 0]
              [(<= len pos) (loop (sub1 len))]
              [(char=? #\newline (string-ref str (sub1 pos))) pos]
              [else (loop (sub1 pos))])))

    (define/public (end-of-line pos)
      (define len (string-length str))
      (let loop ([pos pos])
        (cond [(< pos 0) (loop 0)]
              [(<= len pos) (sub1 len)] ;implicit at end
              [(char=? #\newline (string-ref str pos)) pos]
              [else (loop (add1 pos))])))

    (define/public (backward-match pos cutoff)
      (backward-matching-search pos cutoff 'one))

    (define/public (backward-containing-sexp pos cutoff)
      (backward-matching-search pos cutoff 'all))

    (define/private (backward-matching-search init-pos cutoff mode)
      (define start-pos (if (and (eq? mode 'all)
                                 (init-pos . <= . cutoff))
                            cutoff
                            (sub1 init-pos)))
      (let loop ([pos start-pos]
                 [depth (if (eq? mode 'one) -1 0)]
                 [need-close? (eq? mode 'one)])
        (cond
          [(pos . < . cutoff) #f]
          [else
           (define-values (s e) (get-token-range pos))
           (define sym (get-paren s))
           (cond
             [sym
              (let paren-loop ([parens paren-matches])
                (cond
                  [(null? parens) #f]
                  [(eq? sym (caar parens))
                   (and (not need-close?)
                        (if (= depth 0)
                            (cond
                              [(eq? mode 'all)
                               ;; color:text% method skips back over whitespace, but
                               ;; doesn't go beyond the starting position
                               (min (skip-whitespace e 'forward #f)
                                    init-pos)]
                              [else s])
                            (loop (sub1 s) (sub1 depth) #f)))]
                  [(eq? sym (cadar parens))
                   (cond
                     [(e . > . init-pos)
                      ;; started in middle of closer
                      (if (eq? mode 'one)
                          s
                          (loop (sub1 s) depth #f))]
                     [else (loop (sub1 s) (add1 depth) #f)])]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (define category (classify-position pos))
              (case category
                [(white-space comment)
                 (loop (sub1 s) depth need-close?)]
                [else (if need-close?
                          s
                          (loop (sub1 s) depth #f))])])])))

    (define/public (forward-match pos cutoff)
      (let loop ([pos pos] [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define sym (get-paren s))
           (cond
             [sym
              (let paren-loop ([parens paren-matches])
                (cond
                  [(null? parens) #f]
                  [(eq? sym (caar parens))
                   (if (eqv? pos s) ; don't count the middle of a parenthesis token
                       (loop e (add1 depth))
                       e)]
                  [(eq? sym (cadar parens))
                   (cond
                     [(depth . <= . 0) #f]
                     [(depth . = . 1) e]
                     [else (loop e (sub1 depth))])]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (define category (classify-position pos))
              (case category
                [(white-space comment) (loop e depth)]
                [else
                 (if (zero? depth)
                     e ;; didn't find paren to match, so finding token end
                     (loop e depth))])])])))

    (define/public (skip-whitespace pos dir comments?)
      (define (skip? category)
        (or (eq? category 'white-space)
            (and comments? (eq? category 'comment))))
      (case dir
        [(forward)
         (let loop ([pos pos])
           (define category (classify-position pos))
           (cond
             [(skip? category)
              (define-values (s e) (get-token-range pos))
              (if e
                  (loop e)
                  pos)]
             [else pos]))]
        [(backward)
         (cond
           [(zero? pos) 0]
           [else
            (let loop ([pos (sub1 pos)] [end-pos pos])
              (define category (classify-position pos))
              (cond
                [(skip? category)
                 (define-values (s e) (get-token-range pos))
                 (loop (sub1 s) s)]
                [else end-pos]))])]
        [else
         (error 'skip-whitespace "bad direction: ~e" dir)]))))

(define default-lexer (waive-option module-lexer))
(define default-paren-matches '((\( \)) (\[ \]) (\{ \})))
(define default-quote-matches '(#\" #\|))
(define (default-grouping-position _obj _start _limit _direction) #t)
(define (default-line-indenter _text-like% _pos) #f)
(define (default-range-indenter _text-like% _from _upto) #f)

(struct token (lexeme type paren backup) #:transparent)

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
              [(? list? x) (loop (cons x xs))]
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
  (let* ([str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar"]
         ;;    1234567890123 45678901234 567890 12345678901234567890123456
         ;;             1          2          3          4         5
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "42" 'constant #f 0))
                   (list 16 17 (token " " 'white-space #f 0))
                   (list 17 18 (token "(" 'parenthesis '\( 0))
                   (list 18 23 (token "print" 'symbol #f 0))
                   (list 23 24 (token " " 'white-space #f 0))
                   (list 24 31 (token "\"hello\"" 'string #f 0))
                   (list 31 32 (token ")" 'parenthesis '\) 0))
                   (list 32 33 (token " " 'white-space #f 0))
                   (list 33 39 (token "@print" 'symbol #f 0))
                   (list 39 40 (token "{" 'parenthesis '\{ 0))
                   (list 40 45 (token "Hello" 'symbol #f 0))
                   (list 45 46 (token "}" 'parenthesis '\} 0))
                   (list 46 47 (token " " 'white-space #f 0))
                   (list 47 48 (token "'" 'constant #f 0))
                   (list 48 51 (token "foo" 'symbol #f 0))
                   (list 51 52 (token " " 'white-space #f 0))
                   (list 52 57 (token "#:bar" 'hash-colon-keyword #f 0))))
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
                  '((52 53 constant)
                    (53 56 symbol)))
    (check-equal? (test-update! tm 3 47 4 "'bar")
                  '((48 51 symbol)))
    (check-equal? (test-update! tm 4 24 7 "'hell")
                  '((24 25 constant)
                    (25 29 symbol)))
    (check-equal? (test-update! tm 5 14 2 "99999")
                  '((14 19 constant)))
    ;; Double check final result of the edits
    (check-equal? (send tm -get-string)
                  "#lang racket\n99999 (print 'hell) @print{Hello} 'bar 'bar")
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 19 (token "99999" 'constant #f 0))
                   (list 19 20 (token " " 'white-space #f 0))
                   (list 20 21 (token "(" 'parenthesis '\( 0))
                   (list 21 26 (token "print" 'symbol #f 0))
                   (list 26 27 (token " " 'white-space #f 0))
                   (list 27 28 (token "'" 'constant #f 0))
                   (list 28 32 (token "hell" 'symbol #f 0))
                   (list 32 33 (token ")" 'parenthesis '\) 0))
                   (list 33 34 (token " " 'white-space #f 0))
                   (list 34 40 (token "@print" 'symbol #f 0))
                   (list 40 41 (token "{" 'parenthesis '\{ 0))
                   (list 41 46 (token "Hello" 'symbol #f 0))
                   (list 46 47 (token "}" 'parenthesis '\} 0))
                   (list 47 48 (token " " 'white-space #f 0))
                   (list 48 49 (token "'" 'constant #f 0))
                   (list 49 52 (token "bar" 'symbol #f 0))
                   (list 52 53 (token " " 'white-space #f 0))
                   (list 53 54 (token "'" 'constant #f 0))
                   (list 54 57 (token "bar" 'symbol #f 0))))
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
                   (list  1 13 (token "#lang at-exp" 'other #f 0))
                   (list 13 14 (token " " 'white-space #f 0))
                   (list 14 20 (token "racket" 'symbol #f 0))
                   (list 20 21 (token "\n" 'white-space #f 0))
                   (list 21 23 (token "42" 'constant #f 0))
                   (list 23 24 (token " " 'white-space #f 0))
                   (list 24 25 (token "(" 'parenthesis '\( 0))
                   (list 25 30 (token "print" 'symbol #f 0))
                   (list 30 31 (token " " 'white-space #f 0))
                   (list 31 38 (token "\"hello\"" 'string #f 0))
                   (list 38 39 (token ")" 'parenthesis '\) 0))
                   (list 39 40 (token " " 'white-space #f 0))
                   (list 40 41 (token "@" 'parenthesis #f 0)) ;;??
                   (list 41 46 (token "print" 'symbol #f 0))
                   (list 46 47 (token "{" 'parenthesis '\{ 0))
                   (list 47 60 (token "Hello (there)" 'text #f 0))
                   (list 60 61 (token "}" 'parenthesis '\} 0))
                   (list 61 62 (token " " 'white-space #f 0))
                   (list 62 63 (token "'" 'constant #f 0))
                   (list 63 66 (token "foo" 'symbol #f 0))
                   (list 66 67 (token " " 'white-space #f 0))
                   (list 67 72 (token "#:bar" 'hash-colon-keyword #f 0))))
    (check-equal? (send tm -get-string) str)
    (check-equal? (send tm classify 1 (sub1 (string-length str)))
                  (list 67 72 (token "#:bar" 'hash-colon-keyword #f 0)))))

(module+ test
  (let* ([str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword"]
         [tm (test-create str)])
    (check-equal? (send tm get-tokens)
                  (list
                   (list 1  20 (token "#lang scribble/text" 'text #f 0))
                   (list 20 21 (token "\n" 'white-space #f 0))
                   (list 21 27 (token "Hello " 'text #f 0))
                   (list 27 28 (token "@" 'parenthesis #f 0)) ;;??
                   (list 28 29 (token "(" 'parenthesis '\( 0))
                   (list 29 34 (token "print" 'symbol #f 0))
                   (list 34 35 (token " " 'white-space #f 0))
                   (list 35 42 (token "\"hello\"" 'string #f 0))
                   (list 42 43 (token ")" 'parenthesis '\) 0))
                   (list 43 44 (token " " 'text #f 0))
                   (list 44 45 (token "@" 'parenthesis #f 0))
                   (list 45 50 (token "print" 'symbol #f 0))
                   (list 50 51 (token "{" 'parenthesis '\{ 0))
                   (list 51 64 (token "Hello (there)" 'text #f 0))
                   (list 64 65 (token "}" 'parenthesis '\} 0))
                   (list 65 81 (token " #:not-a-keyword" 'text #f 0))))
    (check-equal? (send tm -get-string) str)))

(module+ test
  (let* ([str "#lang racket\n(λ () #t)"]
         [tm  (test-create str)])
    (check-equal? (send tm classify 1 15)
                  (list 15 16 (token "λ" 'symbol #f 0)))
    (check-equal? (test-update! tm 2 18 0 "a")
                  '((18 19 symbol)))
    (check-equal? (send tm classify 2 18)
                  (list 18 19 (token "a" 'symbol #f 0)))))

(module+ test
  (let ([o (test-create "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")])
    (check-equal? (send o get-tokens)
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 23 (token "#rx\"1234\"" 'string #f 0))
                   (list 23 24 (token "\n" 'white-space #f 0))
                   (list 24 26 (token "#(" 'parenthesis '\( 0))
                   (list 26 27 (token "1" 'constant #f 0))
                   (list 27 28 (token " " 'white-space #f 0))
                   (list 28 29 (token "2" 'constant #f 0))
                   (list 29 30 (token " " 'white-space #f 0))
                   (list 30 31 (token "3" 'constant #f 0))
                   (list 31 32 (token ")" 'parenthesis '\) 0))
                   (list 32 33 (token "\n" 'white-space #f 0))
                   (list 33 35 (token "#'" 'constant #f 0))
                   (list 35 36 (token "(" 'parenthesis '\( 0))
                   (list 36 37 (token "1" 'constant #f 0))
                   (list 37 38 (token " " 'white-space #f 0))
                   (list 38 39 (token "2" 'constant #f 0))
                   (list 39 40 (token " " 'white-space #f 0))
                   (list 40 41 (token "3" 'constant #f 0))
                   (list 41 42 (token ")" 'parenthesis '\) 0))))))

(module+ test
  (let ([o (test-create "#lang racket\n#<<HERE\nblah blah\nblah blah\nHERE\n")])
    (check-equal? (send o get-tokens)
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 46 (token "#<<HERE\nblah blah\nblah blah\nHERE" 'string #f 0))
                   (list 46 47 (token "\n" 'white-space #f 0))))))

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
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "do" 'symbol #f 0))))
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
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "1x" 'symbol #f 0))))
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
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 16 (token "11" 'constant #f 0))))
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
    (check-equal? (dict->list (send tm get-tokens))
                  (list
                   (list  1 13 (token "#lang racket" 'other #f 0))
                   (list 13 14 (token "\n" 'white-space #f 0))
                   (list 14 15 (token "(" 'parenthesis '\( 0))
                   (list 15 17 (token "hi" 'symbol #f 0))
                   (list 17 18 (token ")" 'parenthesis '\) 0))))
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
                  (list 14 19 (token "#hash" 'error #f 0)))
    (check-equal? (test-update! tm 2 19 0 "(")
                  '((14 20 parenthesis #t ")"))
                  "Adding parens after #hash re-lexes from an error to an open")
    (check-equal? (send tm classify 2 14)
                  (list 14 20 (token "#hash(" 'parenthesis '\( 0)))))

;; Test equivalance of our text%-like methods to those of racket:text%
(module+ test
  (require framework
           racket/port
           net/url)
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
      (define determine-spaces (send o -get-line-indenter))
      (for ([pos (in-range 0 (string-length str))])
        (when (or (= pos 0)
                  (char=? (string-ref str (sub1 pos)) #\newline))
          (check-equal? (determine-spaces o pos)
                        (determine-spaces t pos)
                        (format "~v ~v in ~a" determine-spaces pos what))))))

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
                #:check-motion? #f ;huge file & we already test motion above
                #:check-indent? #t))

(module+ test
  (let ()
    (define o (test-create "0\n234\n6\n8\n\n"))
    (check-equal? (send o beginning-of-line 0) 0)
    (check-equal? (send o beginning-of-line 1) 0)
    (check-equal? (send o beginning-of-line 2) 2)
    (check-equal? (send o beginning-of-line 3) 2)
    (check-equal? (send o beginning-of-line 4) 2)
    (check-equal? (send o beginning-of-line 5) 2)
    (check-equal? (send o beginning-of-line 6) 6)
    (check-equal? (send o beginning-of-line 7) 6)
    (check-equal? (send o beginning-of-line 8) 8)
    (check-equal? (send o beginning-of-line 9) 8)
    (check-equal? (send o beginning-of-line 10) 10)
    (check-equal? (send o beginning-of-line 11) 10)
    (check-equal? (send o beginning-of-line 1000) 10)
    (check-equal? (send o end-of-line -1) 1)
    (check-equal? (send o end-of-line 0) 1)
    (check-equal? (send o end-of-line 1) 1)
    (check-equal? (send o end-of-line 2) 5)
    (check-equal? (send o end-of-line 3) 5)
    (check-equal? (send o end-of-line 4) 5)
    (check-equal? (send o end-of-line 5) 5)
    (check-equal? (send o end-of-line 6) 7)
    (check-equal? (send o end-of-line 7) 7)
    (check-equal? (send o end-of-line 8) 9)
    (check-equal? (send o end-of-line 9) 9)
    (check-equal? (send o end-of-line 10) 10)
    (check-equal? (send o end-of-line 11) 10)
    (check-equal? (send o end-of-line 1000) 10)))

#lang racket/base

(require racket/async-channel
         racket/class
         racket/contract/option
         racket/match
         racket/set
         (only-in syntax-color/module-lexer module-lexer*)
         syntax-color/token-tree
         (prefix-in lines: "text-lines.rkt"))

(provide hash-lang%
         (struct-out token)
         generation/c
         position/c
         max-position)

;; To coordinate inter-process updates and queries we use a monotonic
;; "generation". A new object is generation 0. Thereafter the client
;; should increment the generation for every call to update!. Then,
;; when it needs to use some query operation such as classify, it
;; supplies both its latest generation and the position. The query op
;; will block until/unless we have finished updating (a) that
;; generation (b) through that position.
(define generation/c exact-nonnegative-integer?)

;; We use 0-based positions
(define position/c exact-nonnegative-integer?)
(define min-position 0)
(define max-position (sub1 (expt 2 63)))

;; color-textoid<%> added around Racket 8.3.0.8 / syntax-color-lib 1.3.
;; When running on older versions, this will fail, and we'll also set
;; hash-lang% to #f for hash-lang-bridge.rkt to notice and give the user
;; an error message.
(define color-textoid<%>
  (with-handlers ([exn:fail? (λ _ #f)])
    (dynamic-require 'syntax-color/color-textoid 'color-textoid<%>)))

;; Some of this inherited from /src/racket-lang/racket/share/pkgs/gui-lib/framework/private

(struct token (attribs paren) #:transparent) ;public
(struct token:private token (backup mode) #:transparent) ;private

(define (make-hash-lang%-class)
  (class* object% (color-textoid<%>)
    (super-new)
    (init-field [on-notify #f]) ;(or/c #f procedure?)
    ;; A new object has an empty string and is at generation 0. The
    ;; creator should then use update! to set a string value. That way
    ;; both `new` and `update!` return immediately; all
    ;; (re)tokenization is handled uniformly on the dedicated updater
    ;; thread.
    (define generation  0)
    (define content     lines:empty-text-lines)
    (define tokens      (new token-tree%))
    (define tokens-sema (make-semaphore 1))

    (define update-chan  (make-async-channel))
    (define updated-thru (sub1 min-position))

    ;; These members correspond to various lang-info items.
    (define lexer             default-lexer)
    (define paren-matches     default-paren-matches)
    (define quote-matches     default-quote-matches)
    (define grouping-position #f)
    (define line-indenter     #f)
    (define range-indenter    #f)

    ;; These accessor methods really intended just for tests
    (define/public (-get-content) (lines:get-text content 0))
    (define/public (-get-modes)
      (define modes null)
      (send tokens search-min!)
      (send tokens
            for-each
            (λ (beg end data)
              (set! modes (cons (list beg end (token:private-mode data))
                                modes))))
      (reverse modes))
    (define/public (-get-lexer) lexer)
    (define/public (-get-paren-matches) paren-matches)
    (define/public (-get-quote-matches) quote-matches)
    (define/public (-get-grouping-position) grouping-position)
    (define/public (-get-line-indenter) line-indenter)
    (define/public (-get-range-indenter) range-indenter)

    ;; just for debugging
    (define/public (-show-tree msg t [offset 0])
      (displayln msg)
      (send t for-each
            (λ (-beg len dat)
              (define beg (+ -beg offset))
              (define end (+ beg len))
              (println (vector beg end (lines:get-text content beg end) dat)))))

    (define/public (delete)
      (async-channel-put update-chan 'quit))

    ;; position/c -> (or/c #f (list/c position/c position/c token?))
    (define/private (token-ref pos)
      (call-with-semaphore
       tokens-sema
       (λ ()
         (send tokens search! pos)
         (define beg (send tokens get-root-start-position))
         (define end (send tokens get-root-end-position))
         (and (<= beg pos) (< pos end)
              (list beg end (send tokens get-root-data))))))

    ;; This method is safe to call from various threads.
    ;;
    ;; The method signature here is similar to that of Emacs'
    ;; after-change functions: Something changed starting at POS. The text
    ;; there used to be OLD-LEN chars long, but is now NEW-STR.
    (define/public (update! gen pos old-len new-str)
      ;;(-> generation/c position/c exact-nonnegative-integer? string? any)
      (unless (<= generation gen)
        (raise-argument-error 'update! "valid generation" 0 gen pos old-len new-str))
      (unless (<= min-position pos)
        (raise-argument-error 'update! "valid position" 1 gen pos old-len new-str))
      (async-channel-put update-chan
                         (list 'update gen pos old-len new-str)))

    ;; This is the entry thunk of our updater thread.
    ;;
    ;; Tolerate update requests arriving with out-of-order generation
    ;; numbers. This could result from update! being called from
    ;; various threads. For example Racket Mode commands are each
    ;; handled on their own thread, much like a web server. As a rough
    ;; analogy, this is like handling TCP packets arriving possibly
    ;; out of order.
    (define consume-update-chan
      (let ([next-update-gen 1]
            [pending-updates (make-hash)])
        (λ ()
          (match (async-channel-get update-chan)
            ['quit null] ;exit thread
            [(list 'update gen pos old-len new-str)
             (hash-set! pending-updates gen (list pos old-len new-str))
             (let loop ()
               (match (hash-ref pending-updates next-update-gen #f)
                 [(list pos old-len new-str)
                  (hash-remove! pending-updates next-update-gen)
                  (do-update! next-update-gen pos old-len new-str)
                  (set! next-update-gen (add1 next-update-gen))
                  (loop)]
                 [#f #f]))
             (consume-update-chan)]))))
    (thread consume-update-chan)

    ;;;; NOTE: Probably the semaphore needs to guard ALL reads/writes
    ;;;; from the token-tree, due to it being a splay tree (any search
    ;;;; mutates the tree to move an element to the top). So even if
    ;;;; it was fine (?) to do interval-map-ref for some pos < the
    ;;;; point where it was being updated, that certainly won't be
    ;;;; fine for a splay tree.

    ;; Allow threads to wait -- safely and without `sleep`ing -- for
    ;; the updater thread to progress to a certain gen/pos. [Although
    ;; I've never worked with "condition variables" that seems ~= the
    ;; synchronization pattern here?]
    (struct waiter (sema pred) #:transparent)
    (define waiters-set (mutable-set)) ;(set/c waiter?)
    (define waiters-sema (make-semaphore 1)) ;for modifying waiters-set
    ;; Set the generation and updated-thru fields, and un-block
    ;; threads waiting for them to reach certain values.
    (define/private (set/signal-update-progress g p)
      (call-with-semaphore waiters-sema
                           (λ ()
                             (set! generation g)
                             (set! updated-thru p)
                             (for ([w (in-set waiters-set)])
                               (when ((waiter-pred w))
                                 (semaphore-post (waiter-sema w)))))))
    ;; Block until the updater thread has progressed through at least
    ;; a desired generation and also through a desired position.
    (define/public (block-until-updated-thru gen [pos max-position])
      (define (pred) (and (<= gen generation) (<= pos updated-thru)))
      (unless (call-with-semaphore waiters-sema pred)  ;fast path
        (define pred-sema (make-semaphore 0))
        (define w (waiter pred-sema pred))
        (call-with-semaphore waiters-sema set-add! #f waiters-set w)
        (semaphore-wait pred-sema) ;block
        (call-with-semaphore waiters-sema set-remove! #f waiters-set w)))

    ;; Runs on updater thread.
    (define/private (do-update! gen pos old-len new-str)
      (set/signal-update-progress gen 0)
      (when (< 0 old-len)
        (set! content (lines:delete content pos (+ pos old-len))))
      (define new-len (string-length new-str))
      (when (< 0 new-len)
        (set! content (lines:insert content pos new-str)))
      (define diff (- new-len old-len))

      ;; FIXME: If new lang here, we need to restart from the
      ;; beginning. ~= reset content to "" and (do-update! gen
      ;; min-position 0 new-str). Because a new lang means a new
      ;; lexer that could potentially tokenize anything and everything
      ;; differently.
      (maybe-refresh-lang-info pos)  ;from new value of `content`

      ;; From where do we need to re-tokenize? This will be < the pos of
      ;; the change. Back up to the start of the previous token (plus any
      ;; `backup` amount the lexer may have supplied for that token) to
      ;; ensure re-lexing enough such that e.g. appending a character does
      ;; not create a separate token when instead it should be combined
      ;; with an existing token for preceding character(s).
      (define-values (tokenize-from mode)
        (match (token-ref (sub1 pos))
          [(list beg _end (struct* token:private ([backup backup] [mode mode])))
           (values (- beg backup) mode)]
          [#f (values pos #f)]))
      (set! updated-thru (sub1 tokenize-from))
      (define-values (t1 t2)
        (call-with-semaphore tokens-sema
                             (λ ()
                               (send tokens search! tokenize-from)
                               (send tokens split-before))))

      ;; (printf "tokenize-from ~v\n" tokenize-from)
      ;; (printf "diff ~v old-len ~v\n" diff old-len)
      ;; (-show-tree "t1" t1)
      ;; (-show-tree "t2" t2)

      (set! tokens t1)
      (define in (lines:open-input-text content tokenize-from))
      (when on-notify (on-notify 'begin-update))
      (let tokenize ([pos tokenize-from] [mode mode] [contig-same-count 0])
        (define pos/port (add1 pos))
        (define-values (lexeme attribs paren beg/port end/port backup new-mode)
          (lexer in pos/port mode))
        (unless (eof-object? lexeme)
          (define new-beg (sub1 beg/port))
          (define new-end (sub1 end/port))
          (define new-span (- new-end new-beg))
          (define new-tok (token:private attribs paren backup new-mode))
          (call-with-semaphore tokens-sema insert-last-spec! #f
                               tokens new-span new-tok)
          (set/signal-update-progress gen (sub1 new-end))

          ;; Detect whether same as before
          (send t2 search! (- new-beg tokenize-from diff))
          (define orig-beg (send t2 get-root-start-position))
          (define orig-end (send t2 get-root-end-position))
          (define orig-span (- orig-end orig-beg))
          (define orig-tok (send t2 get-root-data))
          (define same? (and (equal? new-span orig-span)
                             (equal? new-tok orig-tok)))
          (unless same?
            (when on-notify
              (on-notify 'token new-beg new-end (token attribs paren))))
          (cond
            [(= contig-same-count 3) ;; stop
             (send t2 search! orig-beg)
             (define-values (_ tail) (send t2 split-after))
             ;; (-show-tree "tokens prior to append" tokens)
             ;; (-show-tree "tail append" tail new-end)
             (call-with-semaphore tokens-sema insert-last! #f
                                  tokens tail)]
            [else ;; continue
             (tokenize new-end
                       new-mode
                       (if same? (add1 contig-same-count) 0))])))
      (when on-notify (on-notify 'end-update))
      (set/signal-update-progress gen max-position))

    ;; This must be called from do-update! because the #lang in the
    ;; source could have changed and we might need new values for all
    ;; of these.
    (define last-lang-end-pos 1)
    (define/private (maybe-refresh-lang-info pos)
      (cond [(<= pos last-lang-end-pos)
             (define in (lines:open-input-text content 0))
             (define info (or (with-handlers ([values (λ _ #f)])
                                (read-language in (λ _ #f)))
                              (λ (_key default) default)))
             (define-values (_line _col end-pos) (port-next-location in))
             (set! last-lang-end-pos end-pos)
             (set! lexer (info 'color-lexer default-lexer))
             (set! paren-matches (info 'drracket:paren-matches default-paren-matches))
             (set! quote-matches (info 'drracket:quote-matches default-quote-matches))
             (set! grouping-position (info 'drracket:grouping-position #f))
             (set! line-indenter (info 'drracket:indentation #f))
             (set! range-indenter (info 'drracket:range-indentation #f))
             (when on-notify
               (on-notify
                'lang
                'paren-matches     (for/list ([v (in-list paren-matches)])
                                     (cons (symbol->string (car v))
                                           (symbol->string (cadr v))))
                'quote-matches     (for/list ([c (in-list quote-matches)])
                                     (string c))
                'grouping-position (and grouping-position #t)
                'line-indenter     (and line-indenter #t)
                'range-indenter    (and range-indenter #t)))
             #t]
            [else #f]))

    (define/public (classify gen pos)
      ;; (-> generation/c position/c (or/c #f (list/c position/c position/c token?))
      (block-until-updated-thru gen pos)
      (match (token-ref pos)
        [(list beg end (token attribs paren)) ;slice off token:private
         (list beg end (token attribs paren))]
        [#f #f]))

    (define/public (get-tokens [gen generation]
                               [from min-position]
                               [upto max-position])
      (block-until-updated-thru gen upto)
      (let loop ([pos from])
        (match (token-ref pos)
          [(list beg end (token attribs paren))
           (cons (list beg end (token attribs paren))
                 (loop end))]
          [#f null])))

    ;;; Something for Emacs forward-sexp-function etc.

    (define/public (grouping gen pos dir limit count)
      (cond
        [(not grouping-position) #f]
        [(<= count 0) pos]
        [else
         (block-until-updated-thru gen
                                   (case dir
                                     [(up backward) min-position]
                                     [(down forward) max-position]))
         (let loop ([pos pos]
                    [count count])
           (define (failure-result) #f) ;can't/didn't move
           (match (grouping-position this pos limit dir)
             [#f (failure-result)]
             [#t 'use-default-s-expression]
             [(? number? new-pos)
              (cond [(< 1 count) (loop new-pos (sub1 count))]
                    [(= new-pos pos) (failure-result)]
                    [else new-pos])]))]))

    ;;; Indent

    (define/public (indent-line-amount gen pos)
      (cond [(not line-indenter) #f]
            [else
             (block-until-updated-thru gen pos)
             (line-indenter this pos)]))

    (define/public (indent-region-amounts gen from upto)
      (cond [(not range-indenter) #f]
            [else
             (block-until-updated-thru gen upto)
             (range-indenter this from upto)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; color-textoid<%> methods.
    ;;;
    ;;; Needed by drracket:indentation, drracket:range-indentation,
    ;;; drracket:grouping-positon.
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
      (if (< pos (lines:text-length content))
          (string-ref (lines:get-text content pos (add1 pos)) 0)
          #\nul))

    (define/public (get-text from upto)
      (lines:get-text content from upto))

    (define/private (get-token who pos)
      (match (or (token-ref pos)
                 (token-ref (sub1 pos))) ;make end position work
        [(list _ _ (? token? token)) token]
        [_ (error who "lookup failed: ~e" pos)]))

    (define/public (classify-position* pos)
      (token-attribs (get-token 'classify-position* pos)))

    (define/public (classify-position pos)
      (define attribs (classify-position* pos))
      (if (symbol? attribs)
          attribs
          (hash-ref attribs 'type 'unknown)))

    (define/private (get-paren pos)
      (token-paren (get-token 'get-paren pos)))

    (define/public (get-token-range pos)
      (match (token-ref pos)
        [(list from upto _token) (values from upto)]
        [_ (values #f #f)]))

    (define/public (last-position)
      (lines:text-length content))

    (define/public (get-backward-navigation-limit pos)
      0)

    (define/public (position-paragraph pos [eol? #f])
      (lines:position->line content pos))

    (define/public (paragraph-start-position para)
      (lines:line->start content (max 0 (min para (lines:text-line-count content)))))

    (define/public (paragraph-end-position para)
      (cond [(<= (lines:text-line-count content) (add1 para))
             (lines:text-length content)]
            [else
             (sub1 (lines:line->start content (add1 para)))]))

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
           (define (atom)
             (if need-close?
                 s
                 (loop (sub1 s) depth #f)))
           (define sym (get-paren s))
           (cond
             [sym
              (let paren-loop ([parens paren-matches])
                (cond
                  [(null? parens)
                   ;; treat an unrecognized parenthesis like an atom
                   (atom)]
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
                [else (atom)])])])))

    (define/public (forward-match pos cutoff)
      (let loop ([pos pos] [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define sym (get-paren s))
           (define (atom)
             (if (zero? depth)
                 e ;; didn't find paren to match, so finding token end
                 (loop e depth)))
           (cond
             [sym
              (let paren-loop ([parens paren-matches])
                (cond
                  [(null? parens)
                   ;; treat an unrecognized parenthesis like an atom
                   (atom)]
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
                [else (atom)])])])))

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

(define hash-lang%
  (and color-textoid<%>
       (make-hash-lang%-class)))

(define default-lexer (waive-option module-lexer*))
(define default-paren-matches '((\( \)) (\[ \]) (\{ \})))
(define default-quote-matches '(#\" #\|))

(module+ ex
  (define t (new hash-lang% [on-notify (λ as (println (cons 'NOTIFY as)))]))
  (send t update! 1 0 0 "#lang racket\n\n(1 2)")
  (send t get-tokens)
  (send t update! 2 13 0 "(")
  (send t get-tokens))


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

;; Most of our interface and implementation uses 1-based positions --
;; as do lexers and Emacs. However the color-textoid<%> methods use
;; 0-based positions, and adjust.
(define position/c exact-positive-integer?)
(define min-position 1)
(define max-position (sub1 (expt 2 63)))

;; color-textoid<%> added around Racket 8.3.0.8 / syntax-color-lib 1.3.
;; When running on older versions, this will fail, and we'll also set
;; hash-lang% to #f for hash-lang-bridge.rkt to notice and give the user
;; an error message.
(define color-textoid<%>
  (with-handlers ([exn:fail? (λ _ #f)])
    (dynamic-require 'syntax-color/color-textoid 'color-textoid<%>)))

;; Some of this inherited from /src/racket-lang/racket/share/pkgs/gui-lib/framework/private

(struct token (attribs paren) #:transparent)
(struct data token (backup mode) #:transparent)

(define (make-hash-lang%-class)
  (class* object% (color-textoid<%>)
    (super-new)
    (init-field [on-notify #f]) ;(or/c #f procedure?)
    ;; A new object has an empty string and is at generation 0. The
    ;; creator should then use update! to set a string value. That way
    ;; both `new` and `update!` return immediately; all
    ;; (re)tokenization is handled uniformly on the dedicated updater
    ;; thread.
    (define content           lines:empty-text-lines)
    (define generation        0)

    ;; Instead of multiple lexer-state structs, do one, directly, for now.
    (define up-to-date? #t)
    (define tokens (new token-tree%)) ;valid tokens
    (define invalid-tokens (new token-tree%))
    (define invalid-tokens-beg max-position)
    (define invalid-tokens-mode #f)
    (define current-pos min-position) ;pos of next token to be read
    (define current-mode #f)

    (define update-chan       (make-async-channel))
    ;; (define updated-thru      0) instead see invalid-tokens-beg

    ;; These members correspond to various lang-info items.
    (define lexer             default-lexer)
    (define paren-matches     default-paren-matches)
    (define quote-matches     default-quote-matches)
    (define grouping-position #f)
    (define line-indenter     #f)
    (define range-indenter    #f)

    ;; These accessor methods really intended just for tests
    (define/public (-get-content) (lines:get-text content 0))
    (define/public (-get-modes) null)
    (define/public (-get-lexer) lexer)
    (define/public (-get-paren-matches) paren-matches)
    (define/public (-get-quote-matches) quote-matches)
    (define/public (-get-grouping-position) grouping-position)
    (define/public (-get-line-indenter) line-indenter)
    (define/public (-get-range-indenter) range-indenter)

    ;; just for debugging
    (define/public (-show-tree)
      (send tokens for-each
            (λ (beg len dat) (println (vector beg (+ beg len) dat)))))

    (define/public (delete)
      (async-channel-put update-chan 'quit))

    (define/private (get-tokens-at-position pos)
      (send tokens search! pos))

    ;; (or/c #f position/c) -> (or/c #f (list/c position/c position/c token?))
    (define/private (token-ref pos)
      (and pos
           (begin
             (send tokens search! pos)
             (list (send tokens get-root-start-position)
                         (send tokens get-root-end-position)
                         (send tokens get-root-data)))))

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
                             (set! invalid-tokens-beg p)
                             (for ([w (in-set waiters-set)])
                               (when ((waiter-pred w))
                                 (semaphore-post (waiter-sema w)))))))
    ;; Block until the updater thread has progressed through at least
    ;; a desired generation and also through a desired position.
    (define/public (block-until-updated-thru gen [pos max-position])
      (define (pred) (and (<= gen generation) (< pos invalid-tokens-beg)))
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
        (set! content (lines:delete content (sub1 pos) (+ (sub1 pos) old-len))))
      (define new-len (string-length new-str))
      (when (< 0 new-len)
        (set! content (lines:insert content (sub1 pos) new-str)))
      (define diff (- new-len old-len))

      ;; FIXME: If new lang here, we need to restart from the
      ;; beginning. ~= reset content to "" and (do-update! gen
      ;; min-position 0 new-str). Because a new lang means a new
      ;; lexer that could potentially tokenize anything and everything
      ;; differently.
      (maybe-refresh-lang-info pos)  ;from new value of `content`

      (when on-notify (on-notify 'begin-update))
      (insert/delete pos (+ pos (max old-len new-len)) diff)
      (when on-notify (on-notify 'end-update))
      (set/signal-update-progress gen max-position))

    ;; Runs on updater thread.
    (define/private (insert/delete edit-start-pos edit-end-pos change-length)
      (unless up-to-date?
        (sync-invalid))
      (cond
        [up-to-date?
         (let-values ([(orig-token-start orig-token-end valid-tree invalid-tree orig-data)
                       (split-backward edit-start-pos tokens)])
           (set! invalid-tokens invalid-tree)
           (set! tokens valid-tree)
           (set! invalid-tokens-beg
            (if (send invalid-tokens is-empty?)
                max-position
                (+ orig-token-end change-length)))
           (set! invalid-tokens-mode (and orig-data (data-mode orig-data)))
           (let ([start orig-token-start])
             (set! current-pos start)
             (set! current-mode
                   (if (or (not orig-data) (= start min-position))
                       #f
                       (begin
                         (send valid-tree search-max!)
                         (data-mode (send valid-tree get-root-data))))))
           (set! up-to-date? #f)
           ;;(update-lexer-state-observers)
           )]
        [(and (>= edit-end-pos min-position)
              (> edit-end-pos current-pos))
         (let-values (((tok-start tok-end valid-tree invalid-tree orig-data)
                       (split-backward edit-end-pos invalid-tokens)))
           (set! invalid-tokens invalid-tree)
           (set! invalid-tokens-beg (+ tok-end change-length))
           (set! invalid-tokens-mode (and orig-data (data-mode orig-data))))]
        [(> edit-start-pos min-position)
         (set! invalid-tokens-beg (+ invalid-tokens-beg change-length))]
        [else
         (let-values (((tok-start tok-end valid-tree invalid-tree data)
                       (split-backward edit-start-pos tokens)))
           (set! tokens valid-tree)
           (set! invalid-tokens-beg (+ invalid-tokens-beg change-length))
           (let ([start tok-start])
             (set! current-pos start)
             (set! current-mode
                   (if (or (not data) (= start min-position))
                       #f
                       (begin
                         (send valid-tree search-max!)
                         (data-mode (send valid-tree get-root-data)))))))])
      (re-tokenize))

    ;; Discard extra tokens at the first of invalid-tokens
    (define/private (sync-invalid)
      (when (and (not (send invalid-tokens is-empty?))
                 (< invalid-tokens-beg current-pos))
        (send invalid-tokens search-min!)
        (let* ([length (send invalid-tokens get-root-length)]
               [data (send invalid-tokens get-root-data)]
               [_ (println data)]
               [mode (and data (data-mode data))])
          (send invalid-tokens remove-root!)
          (set! invalid-tokens-beg (+ invalid-tokens-beg length))
          (set! invalid-tokens-mode mode))
        (sync-invalid)))

    (define/private (split-backward pos valid-tokens)
      (let loop ([pos pos][valid-tree valid-tokens][old-invalid-tree #f])
        (let-values (((orig-token-start orig-token-end valid-tree invalid-tree orig-data)
                      (send valid-tree split/data pos)))
          (println (list "split-backward loop" orig-token-start orig-token-end valid-tree invalid-tree orig-data))
          (let ([backup-pos (- pos (data-backup orig-data))]
                [invalid-tree (or old-invalid-tree invalid-tree)])
            (if (< backup-pos pos)
                ;; back up more:
                (loop pos valid-tree invalid-tree)
                ;; that was far enough:
                (values orig-token-start orig-token-end valid-tree invalid-tree orig-data))))))

    (define/private (re-tokenize)
      ;; FIXME: Same as maybe-refresh-lang-info
      (define in (open-input-string (lines:get-text content 0)))
      (port-count-lines! in)
      (continue-re-tokenize in current-pos current-mode))

    (define/private (continue-re-tokenize in in-start-pos lexer-mode)
      (define-values (_line1 _col1 pos-before) (port-next-location in))
      (define-values (lexeme attribs paren new-token-start new-token-end
                             backup-delta new-lexer-mode)
        (lexer in in-start-pos lexer-mode))
      (define-values (_line2 _col2 pos-after) (port-next-location in))
      (cond
        [(eq? 'eof attribs)
         (set! up-to-date? #t)
         ;;(re-tokenize)
         ]
        [else
         (let ([len (- new-token-end new-token-start)])
           (set! current-pos (+ current-pos len))
           (set! current-mode new-lexer-mode)
           (sync-invalid)
           ;; insert-last! is 3X slower than insert-last-spec!
           (insert-last-spec! tokens
                              len
                              (data attribs paren backup-delta new-lexer-mode))
           (when on-notify
             (on-notify 'token
                        new-token-start
                        new-token-end
                        (token attribs paren backup-delta)))
           (cond
             [(and (not (send tokens is-empty?))
                   (= invalid-tokens-beg current-pos)
                   (equal? new-lexer-mode invalid-tokens-mode))
              (send invalid-tokens search-max!)
              (insert-last! tokens invalid-tokens)
              (set! invalid-tokens-beg max-position)
              (set! up-to-date? #t)
              (re-tokenize)]
             [else
              (continue-re-tokenize in in-start-pos new-lexer-mode)]))]))

    ;; This must be called from do-update! because the #lang in the
    ;; source could have changed and we might need new values for all
    ;; of these.
    (define last-lang-end-pos 1)
    (define/private (maybe-refresh-lang-info pos)
      (cond [(<= pos last-lang-end-pos)
             ;; FIXME: Don't do lines:get-text into one huge string.
             ;; Instead, do something like `open-input-text-editor` at
             ;; line 23 of ~/src/racket-lang/racket/share/pkgs/gui-lib/mred/private/snipfile.rkt.
             (define in (open-input-string (lines:get-text content 0)))
             (port-count-lines! in)
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
        [(not grouping-position) #f]
        [(<= count 0) pos]
        [else
         (block-until-updated-thru gen
                                   (case dir
                                     [(up backward) min-position]
                                     [(down forward) max-position]))
         (let loop ([pos (sub1 pos)] ;1.. -> 0..
                    [count count])
           (define (failure-result) #f) ;can't/didn't move
           (match (grouping-position this pos limit dir)
             [#f (failure-result)]
             [#t 'use-default-s-expression]
             [(? number? new-pos)
              (cond [(< 1 count) (loop new-pos (sub1 count))]
                    [(= new-pos pos) (failure-result)]
                    [else (add1 new-pos)])]))])) ;0.. -> 1..

    ;;; Indent

    (define/public (indent-line-amount gen pos)
      (cond [(not line-indenter) #f]
            [else
             (block-until-updated-thru gen pos)
             (line-indenter this (sub1 pos))])) ;1.. -> 0..

    (define/public (indent-region-amounts gen from upto)
      (cond [(not range-indenter) #f]
            [else
             (block-until-updated-thru gen upto)
             (range-indenter this (sub1 from) (sub1 upto))])) ;1.. -> 0..

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
      (let ([pos (add1 pos)])
        (match (or (token-ref pos)
                   (token-ref (sub1 pos))) ;make end position work
          [(list _ _ (? token? token)) token]
          [_ (error who "lookup failed: ~e" (sub1 pos))])))

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
      (match (token-ref (add1 pos))
        [(list from upto _token) (values (sub1 from) (sub1 upto))]
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
  (define t (new hash-lang% [on-notify (λ as (println as))]))
  (send t update! 1 1 0 "hi")
  (send t -show-tree)
  ;(send t update! 2 3 0 "de")
  )

(module+ m
  (require syntax-color/token-tree)
  (define (get-token t pos)
    (send t search! pos)
    (values (send t get-root-start-position)
            (send t get-root-end-position)
            (send t get-root-data)))
  (define (show-tree t)
    (send t for-each (λ (beg len dat) (println (vector beg (+ beg len) dat)))))
  (define t (new token-tree%))
  (insert-last-spec! t 10 "foo")
  (insert-last-spec! t 20 "bar")
  (insert-last-spec! t 10 "baz")
  (displayln "=== t ===")
  (show-tree t)
  ;;(println (call-with-values (λ () (get-token t 0)) list))
  (define-values (beg end t1 t2) (send t split 15))
  (displayln "=== t1 ===")
  (println beg)
  (show-tree t1)
  (displayln "=== t2 ===")
  (println end)
  (show-tree t2)
  )

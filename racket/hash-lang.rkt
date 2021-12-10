#lang racket/base

(require racket/async-channel
         racket/class
         racket/contract/option
         racket/match
         racket/set
         syntax-color/token-tree
         syntax-color/paren-tree
         syntax/parse/define
         (prefix-in lines: "text-lines.rkt"))

(provide hash-lang%
         generation/c
         position/c
         max-position)

;; To coordinate inter-process updates and queries we use successive
;; "generation" numbers. A new object is generation 0. Thereafter the
;; client should increment the generation for each call to `update!`.
;; (Clients should not skip generation numbers; strict succession is
;; used to detect out-of-order update requests.) Then, when it needs
;; to use some query operation such as classify, it supplies both its
;; latest generation and the position. The query op will block
;; until/unless we have finished updating (a) that generation (b)
;; through at least that position (or possibly a later position,
;; depending on the op).
(define generation/c exact-nonnegative-integer?)

;; We use 0-based positions
(define position/c exact-nonnegative-integer?)
(define min-position 0)
(define max-position (sub1 (expt 2 63)))

;; color-textoid<%> and module-lexer* were added around Racket 8.3.0.8
;; / syntax-color-lib 1.3. When running on older versions, these will
;; be #f, and we'll also set hash-lang% to #f for hash-lang-bridge.rkt
;; to notice and give the user an error message.
(define color-textoid<%>
  (with-handlers ([exn:fail? (λ _ #f)])
    (dynamic-require 'syntax-color/color-textoid 'color-textoid<%>)))
(define module-lexer*
  (with-handlers ([exn:fail? (λ _ #f)])
    (dynamic-require 'syntax-color/module-lexer 'module-lexer*)))
(define racket-amount-to-indent
  (with-handlers ([exn:fail? (λ _ #f)])
    (dynamic-require 'syntax-color/racket-indentation 'racket-amount-to-indent)))
(define racket-grouping-position
  (with-handlers ([exn:fail? (λ _ #f)])
    (dynamic-require 'syntax-color/racket-navigation 'racket-grouping-position)))

;; Some of this inherited from /src/racket-lang/racket/share/pkgs/gui-lib/framework/private

;; Our data for token-tree%
(struct data (attribs backup mode) #:transparent)

(define (attribs->type attribs)
  (if (symbol? attribs)
      attribs
      (hash-ref attribs 'type 'unknown)))

(define (attribs->table attribs)
  (if (symbol? attribs)
      (hasheq 'type attribs)
      attribs))

(define-simple-macro (with-semaphore sema e:expr ...+)
  (call-with-semaphore sema (λ () e ...)))

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
    (define parens      (new paren-tree% [matches default-paren-matches]))
    (define parens-sema (make-semaphore 1))

    (define update-chan  (make-async-channel))
    (define updated-thru (sub1 min-position))

    ;; These members correspond to various lang-info items.
    (define lexer             default-lexer)
    (define paren-matches     default-paren-matches)
    (define quote-matches     default-quote-matches)
    (define grouping-position racket-grouping-position)
    (define line-indenter     racket-amount-to-indent)
    (define range-indenter    #f)

    ;; These accessor methods really intended just for tests
    (define/public (-get-content) (lines:get-text content 0))
    (define/public (-get-modes)
      (define modes null)
      (send tokens search-min!)
      (send tokens
            for-each
            (λ (beg end data)
              (set! modes (cons (list beg end (data-mode data))
                                modes))))
      (reverse modes))
    (define/public (-get-parens) parens)
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
    ;;
    ;; Note: This is not thread safe without taking tokens-sema.
    (define/private (token-ref pos)
      (send tokens search! pos)
      (define beg (send tokens get-root-start-position))
      (define end (send tokens get-root-end-position))
      (and (<= beg pos) (< pos end)
           (list beg end (send tokens get-root-data))))

    ;; This method is safe to call from various threads.
    ;;
    ;; The method signature here is similar to that of Emacs'
    ;; after-change functions: Something changed starting at POS. The text
    ;; there used to be OLD-LEN chars long, but is now NEW-STR.
    (define/public (update! gen pos old-len new-str)
      ;;(-> generation/c position/c exact-nonnegative-integer? string? any)
      (unless (< generation gen)
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

    ;; Allow threads to wait -- safely and without `sleep`ing -- for
    ;; the updater thread to progress to a certain gen/pos. [Although
    ;; I've never worked with "condition variables" that seems ~= the
    ;; synchronization pattern here?]
    (struct waiter (sema pred) #:transparent)
    (define waiters-set (mutable-set)) ;(set/c waiter?)
    (define waiters-sema (make-semaphore 1)) ;for modifying waiters-set

    ;; Set the generation and updated-thru fields, and un-block
    ;; threads waiting for them to reach certain values.
    (define/private (set-update-progress #:generation [g generation]
                                         #:position   p)
      (with-semaphore waiters-sema
        (set! generation g)
        (set! updated-thru p)
        (for ([w (in-set waiters-set)])
          (when ((waiter-pred w))
            (semaphore-post (waiter-sema w))))))

    ;; Block until the updater thread has progressed through at least
    ;; a desired generation and also through a desired position.
    (define/public (block-until-updated-thru gen [pos max-position])
      (define (pred) (and (<= gen generation) (<= pos updated-thru)))
      (unless (call-with-semaphore waiters-sema pred)  ;fast path
        (define pred-sema (make-semaphore 0))
        (define w (waiter pred-sema pred))
        (with-semaphore waiters-sema (set-add! waiters-set w))
        (semaphore-wait pred-sema) ;block
        (with-semaphore waiters-sema (set-remove! waiters-set w))))

    ;; Runs on updater thread.
    (define last-lang-end-pos 1)
    (define/private (do-update! gen pos old-len new-str)
      (set-update-progress #:generation gen
                           #:position (sub1 min-position))

      (when (< 0 old-len)
        (set! content (lines:delete content pos (+ pos old-len))))
      (define new-len (string-length new-str))
      (when (< 0 new-len)
        (set! content (lines:insert content pos new-str)))

      ;; A change before the end of the text for the old #lang might
      ;; result in new lang info values. If any change, it could
      ;; result in entirely different tokens and parens, so in that
      ;; case restart from scratch.
      (cond
        [(< pos last-lang-end-pos)
         (define in (lines:open-input-text content 0))
         (define info (or (with-handlers ([values (λ _ #f)])
                            (read-language in (λ _ #f)))
                          (λ (_key default) default)))
         (define-values (_line _col end-pos) (port-next-location in))
         (set! last-lang-end-pos end-pos) ;to check next time

         (define any-changed? #f)
         (define-syntax-rule (set!? var expr)
           (let ([val expr])
             (unless (equal? var val)
               (set! var val)
               (set! any-changed? #t))))
         (set!? lexer             (info 'color-lexer default-lexer))
         (set!? paren-matches     (info 'drracket:paren-matches default-paren-matches))
         (set!? quote-matches     (info 'drracket:quote-matches default-quote-matches))
         (set!? grouping-position (info 'drracket:grouping-position racket-grouping-position))
         (set!? line-indenter     (info 'drracket:indentation racket-amount-to-indent))
         (set!? range-indenter    (info 'drracket:range-indentation #f))
         (cond
           [(or any-changed? (= gen 1))
            (when on-notify
              (on-notify
               'lang
               'racket-grouping   (equal? grouping-position racket-grouping-position)
               'line-indenter     (and line-indenter #t)
               'range-indenter    (and range-indenter #t)))
            (set! tokens (new token-tree%))
            (set! parens (new paren-tree% [matches paren-matches]))
            (update-tokens-and-parens min-position (lines:text-length content))]
           [else
            (update-tokens-and-parens pos (- new-len old-len))])]
        [else
         (update-tokens-and-parens pos (- new-len old-len))]))

    (define/private (update-tokens-and-parens pos diff)
      ;; The position from which we need to start re-tokenizing will
      ;; be less than the position of the change, `pos`. To find that:
      ;;
      ;; Find the beginning of the _previous_ token. If this has a
      ;; non-zero `backup` amount, back up more; repeating until we
      ;; find a zero backup. The start of that token is our initial
      ;; position from which to re-tokenize.
      ;;
      ;; However, the lexer mode to use for that position is stored
      ;; with the previous token (if any). So finally back up one more
      ;; to get the mode.
      (define-values (tokenize-from mode)
        (let loop ([pos pos] [first? #t])
          (match (with-semaphore tokens-sema (token-ref pos))
            [#f (values min-position #f)]
            [(list beg _end (struct* data ([backup backup])))
             (cond [(< 0 backup) (loop (- beg backup) #f)]
                   [first?       (loop (sub1 beg)     #f)]
                   [else
                    ;; Get mode from _previous_ token
                    (match (with-semaphore tokens-sema (token-ref (sub1 beg)))
                      [#f (values beg #f)]
                      [(list _beg _end (struct* data ([mode mode])))
                       (values beg mode)])])])))
      ;; Everything before this is valid; allow other threads to
      ;; progress thru that position of this generation.
      (set-update-progress #:position (sub1 tokenize-from))
      ;; (printf "tokenize-from ~v\n" tokenize-from)
      ;; (printf "diff ~v old-len ~v\n" diff old-len)

      ;; Split the token and paren trees.
      (define old-tokens (with-semaphore tokens-sema
                           (send tokens search! tokenize-from)
                           (define-values (t1 t2) (send tokens split-before))
                           (set! tokens t1)
                           t2))
      ;; (-show-tree "tokens" tokens)
      ;; (-show-tree "old-tokens" old-tokens)

      (with-semaphore parens-sema
        (send parens split-tree tokenize-from))
      ;;(local-require racket/pretty)
      ;;(pretty-print (cons 'parens-after-split (send parens test)))

      (define in (lines:open-input-text content tokenize-from))
      (define-values (min-changed-pos max-changed-pos)
        (let tokenize ([pos tokenize-from]
                       [mode mode]
                       [contig-same-count 0]
                       [min-changed-pos #f]
                       [max-changed-pos min-position])
          (define pos/port (add1 pos))
          (define-values (lexeme attribs paren beg/port end/port backup new-mode)
            (lexer in pos/port mode))
          (cond
            [(eof-object? lexeme)
             (values min-changed-pos max-changed-pos)]
            [else
             (define new-beg (sub1 beg/port))
             (define new-end (sub1 end/port))
             (define new-span (- new-end new-beg))
             (define new-tok (data attribs backup new-mode))
             (with-semaphore tokens-sema (insert-last-spec! tokens new-span new-tok))
             (with-semaphore parens-sema (send parens add-token paren new-span))
             (set-update-progress #:position (sub1 new-end))

             ;; Detect whether same as before (maybe just shifted)
             (send old-tokens search! (- new-beg tokenize-from diff))
             (define old-beg (send old-tokens get-root-start-position))
             (define old-end (send old-tokens get-root-end-position))
             (define old-span (- old-end old-beg))
             (define old-tok (send old-tokens get-root-data))
             (define same? (and (equal? new-span old-span)
                                (equal? new-tok old-tok)))
             (define contig-same-goal 3)
             (cond
               [(= (add1 contig-same-count) contig-same-goal) ;; stop early
                (send old-tokens search! old-beg)
                (define-values (_ keep) (send old-tokens split-after))
                ;; (-show-tree "tokens prior to append" tokens)
                ;; (-show-tree "old tokens to append" keep new-end)
                (with-semaphore tokens-sema (insert-last! tokens keep))

                ;; (pretty-print (cons 'parens-before-merge (send parens test)))
                (define paren-keep-span (- (last-position) new-end))
                ;; (printf "paren-keep-span: ~v\n" paren-keep-span)
                (with-semaphore parens-sema (send parens merge-tree paren-keep-span))
                ;; (pretty-print (cons 'parens-after-merge (send parens test)))
                (values min-changed-pos max-changed-pos)]
               [else
                (tokenize new-end
                          new-mode
                          (+ contig-same-count (if same? 1 0))
                          (if same? min-changed-pos (or min-changed-pos new-beg))
                          (if same? max-changed-pos (max max-changed-pos new-end)))])])))
      (when on-notify
        (on-notify 'invalidate generation (or min-changed-pos 0) max-changed-pos))
      (set-update-progress #:position max-position))

    ;; ------------------------------------------------------------
    ;; Methods for Emacs query commands.

    ;; Can be called on any command thread.
    (define/public (classify gen pos)
      ;; (-> generation/c position/c (or/c #f (list/c position/c position/c (or/c symbol? hash-eq?))
      (block-until-updated-thru gen pos)
      (match (with-semaphore tokens-sema (token-ref pos))
        [(list beg end (struct* data ([attribs attribs])))
         (list beg end attribs)]
        [#f #f]))

    ;; Can be called on any command thread.
    (define/public (get-tokens [gen generation]
                               [from min-position]
                               [upto max-position])
      (block-until-updated-thru gen upto)
      (let loop ([pos from])
        (match (with-semaphore tokens-sema (token-ref pos))
          [(list beg end (struct* data ([attribs attribs])))
           (if (<= end upto)
               (cons (list beg end attribs)
                     (loop end))
               null)]
          [#f null])))

    ;; ------------------------------------------------------------
    ;; Methods for Emacs navigation and indent commands.
    ;;
    ;; These command methods work by calling various drracket:xyz
    ;; functions, supplying `this` as the color-textoid<%> argument.
    ;; In other words, those functions will "call back" use the
    ;; textoid methods.
    ;;
    ;; These command methods call block-until-updated-thru, to wait
    ;; until the updater thread has progressed far enough to support
    ;; the command.
    ;;
    ;; These command methods take the tokens and parens semaphores for
    ;; the dynamic extent the call to the drracket:xyz function. As a result
    ;; the textoid methods need not. This is signficantly faster (e.g. 2X).
    ;;

    ;; Can be called on any command thread.
    (define/public (grouping gen pos dir limit count)
      (cond
        [(<= count 0) pos]
        [else
         (block-until-updated-thru gen
                                   (case dir
                                     [(up backward) min-position]
                                     [(down forward) max-position]))
         (let loop ([pos pos]
                    [count count])
           (match (with-semaphore tokens-sema
                    (with-semaphore parens-sema
                      (match (grouping-position this pos limit dir)
                        ;; Handle case where it returns #t, meaning
                        ;; "use default s-expr grouping". That spec
                        ;; slightly predates the addition of
                        ;; syntax-color/racket-navigation --- the
                        ;; availability of which probably means that
                        ;; this #t value should no longer be returned?
                        ;; In other words, if a lang wants s-expr nav,
                        ;; its lang info should either not supply any
                        ;; drracket:grouping-position at all, or,
                        ;; supply racket-grouping-position as that?
                        [#t
                         (when (equal? grouping-position racket-grouping-position)
                           (error 'grouping "racket-grouping-position returned #t"))
                         (racket-grouping-position this pos limit dir)]
                        [v v])))
             [#f #f]
             [(? number? new-pos)
              (cond [(< 1 count) (loop new-pos (sub1 count))]
                    [(= new-pos pos) #f]
                    [else new-pos])]))]))

    ;; Can be called on any command thread.
    (define/public (indent-line-amount gen pos)
      (cond [(not line-indenter) #f]
            [else
             (block-until-updated-thru gen pos)
             (with-semaphore tokens-sema
               (with-semaphore parens-sema
                 (line-indenter this pos)))]))

    ;; Can be called on any command thread.
    (define/public (indent-region-amounts gen from upto)
      (cond [(not range-indenter) #f]
            [else
             (block-until-updated-thru gen upto)
             (with-semaphore tokens-sema
               (with-semaphore parens-sema
                 (range-indenter this from upto)))]))

    ;; ------------------------------------------------------------
    ;; color-textoid<%> methods.
    ;;
    ;; Warning: As discussed above, these are safe to call only from
    ;; the dyanamic extent of the `grouping`, `indent-line-amount`, or
    ;; `indent-region-amounts` methods.

    (define/public (last-position)
      (lines:text-length content))

    (define/public (get-character pos)
      (if (< pos (lines:text-length content))
          (string-ref (lines:get-text content pos (add1 pos)) 0)
          #\nul))

    (define/public (get-text from upto)
      (lines:get-text content from upto))

    (define/public (position-paragraph pos [eol? #f])
      (lines:position->line content pos))

    (define/public (paragraph-start-position para)
      (lines:line->start content (max 0 (min para (lines:text-line-count content)))))

    (define/public (paragraph-end-position para)
      (cond [(<= (lines:text-line-count content) (add1 para))
             (lines:text-length content)]
            [else
             (sub1 (lines:line->start content (add1 para)))]))

    (define/public (classify-position* position)
      (send tokens search! position)
      (match (send tokens get-root-data)
        [(struct* data ([attribs (app attribs->table table)])) table]
        [#f #f]))

    (define/public (classify-position position)
      (send tokens search! position)
      (match (send tokens get-root-data)
        [(struct* data ([attribs (app attribs->type type)])) type]
        [#f #f]))

    (define/public (get-token-range position)
      (send tokens search! position)
      (values (send tokens get-root-start-position)
              (send tokens get-root-end-position)))

    (define/public (get-backward-navigation-limit pos)
      0)

    (define/public (backward-match position cutoff)
      (let ([x (internal-backward-match position cutoff)])
        (cond
          [(or (eq? x 'open) (eq? x 'beginning)) #f]
          [else x])))

    (define/private (internal-backward-match position cutoff)
      (let ([position (skip-whitespace position 'backward #t)])
        (define-values (start end error) (send parens match-backward position))
        (cond
          [(and start end (not error))
           (let ((match-pos start))
             (cond
               ((>= match-pos cutoff) match-pos)
               (else #f)))]
          [(and start end error) #f]
          [else
           (send tokens search! (sub1 position))
           (define tok-start (send tokens get-root-start-position))
           (cond
             [(send parens is-open-pos? tok-start) 'open]
             [(= tok-start position)               'beginning]
             [else                                 tok-start])])))

    (define/public (backward-containing-sexp position cutoff)
      (let loop ([cur-pos position])
        (let ([p (internal-backward-match cur-pos cutoff)])
          (cond
            [(eq? 'open p)
             ;; [Comment from color.rkt: "Should this function skip
             ;; backwards past whitespace? the docs seem to indicate
             ;; it does, but it doesn't really."]
             cur-pos]
            [(eq? 'beginning p) #f]
            [(not p) #f]
            (else (loop p))))))

    (define/public (forward-match position cutoff)
      (do-forward-match position cutoff #t))

    (define/private (do-forward-match position cutoff skip-whitespace?)
      (let ([position (if skip-whitespace?
                          (skip-whitespace position 'forward #t)
                          position)])
        (define-values (start end error) (send parens match-forward position))
        (cond
          [(and start end (not error))
           (cond
             [(<= end cutoff) end]
             [else #f])]
          [(and start end error) #f]
          [else
           (skip-past-token position)])))

    (define/private (skip-past-token position)
      (send tokens search! position)
      (define start (send tokens get-root-start-position))
      (define end (send tokens get-root-end-position))
      (cond
        [(or (send parens is-close-pos? start)
             (= end position))
         #f]
        [else end]))

    (define/public (skip-whitespace position direction comments?)
      (cond
        [(and (eq? direction 'forward) (>= position (last-position))) position]
        [(and (eq? direction 'backward) (<= position 0)) position]
        [else
         (send tokens search! (if (eq? direction 'backward)
                                  (sub1 position)
                                  position))
         (match (send tokens get-root-data)
           [(struct* data ([attribs (app attribs->type type)]))
            (cond
              [(or (eq? 'white-space type)
                   (and comments? (eq? 'comment type)))
               (skip-whitespace (if (eq? direction 'forward)
                                    (send tokens get-root-end-position)
                                    (send tokens get-root-start-position))
                                direction
                                comments?)]
              [else position])]
           [#f position])]))))

(define hash-lang%
  (and color-textoid<%>
       module-lexer*
       racket-amount-to-indent
       racket-grouping-position
       (make-hash-lang%-class)))

(define default-lexer (waive-option module-lexer*))
(define default-paren-matches '((\( \)) (\[ \]) (\{ \})))
(define default-quote-matches '(#\" #\|))

(module+ ex
  (define t (new hash-lang% [on-notify void #;(λ as (println (cons 'NOTIFY as)))
                                       ]))
  (define (match-forward pos)
    (call-with-values (λ () (send (send t -get-parens) match-forward pos)) list))
  (define (match-backward pos)
    (call-with-values (λ () (send (send t -get-parens) match-backward pos)) list))
  (send t update! 1 0 0 "#lang racket\n(1 3)(fooo bar baz)")
  ;;                     0123456789012 34567890123456789012
  ;;                               1          2         3
  ;;(send t get-tokens)
  (send t block-until-updated-thru 1)
  (match-forward 13)
  (match-backward 18)
  (match-forward 18)
  (match-backward 32)
  (send t update! 2 15 0 " 2") ;After ( insert 2 non-paren
  (send t block-until-updated-thru 2)
  ;;(send t get-tokens)
  ;;(send t -get-content)
  (match-forward 13)
  (match-backward 20)
  (match-forward 20)
  (match-backward 34))

(module+ ex1
  (require syntax-color/paren-tree)
  (define t (new paren-tree% [matches default-paren-matches]))
  (define (match-forward pos) (call-with-values (λ () (send t match-forward pos)) list))
  (define (match-backward pos) (call-with-values (λ () (send t match-backward pos)) list))
  ;; "(xx)"
  (define old-len 4)
  (send t add-token '\( 1)
  (send t add-token #f  2)
  (send t add-token '\) 1)
  ;; 1 (, 2 #f, 1 )
  (send t test)
  (match-forward 0)
  (match-backward 4)

  (define insert-pos 1)
  (define insert-len 10)
  (define new-len (+ old-len insert-len))
  (printf "insert at ~v a non-paren of len ~v\nold total len was ~v, new total len ~v\n"
          insert-pos insert-len
          old-len new-len)
  (send t split-tree insert-pos)
  (send t test)
  ;; before: 1 (
  ;; after:  2 #f, 1 )
  (send t add-token #f insert-len)
  (send t test)
  ;; before: 1 (, 10 #f
  ;; after:  2 #f, 1 )
  (define span-to-keep (- old-len insert-pos))
  (printf "span-to-keep: ~v\n" span-to-keep)
  (send t merge-tree span-to-keep)
  (send t test)
  (match-forward 0)
  (match-backward new-len)
  )

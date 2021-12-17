#lang racket/base

(require racket/async-channel
         racket/class
         racket/contract/base
         racket/contract/option
         racket/match
         racket/set
         syntax-color/token-tree
         syntax-color/paren-tree
         (only-in syntax-color/color-textoid color-textoid<%>)
         (only-in syntax-color/module-lexer module-lexer*)
         (only-in syntax-color/racket-indentation racket-amount-to-indent)
         (only-in syntax-color/racket-navigation racket-grouping-position)
         syntax/parse/define
         (prefix-in lines: "text-lines.rkt"))

(provide hash-lang%
         generation/c
         position/c
         min-position)

;; Portions originated from /src/racket-lang/racket/share/pkgs/gui-lib/framework/private


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
(define min-position 0)
(define max-position (sub1 (expt 2 63)))
(define position/c (integer-in min-position max-position))

;; Our data for token-tree%
(struct data (attribs backup mode) #:transparent #:authentic)

(define-simple-macro (with-semaphore sema e:expr ...+)
  (call-with-semaphore sema (λ () e ...)))

(define hash-lang%
  (class* object% (color-textoid<%>)
    (super-new)

    ;; Virtual methods to override for notifications
    (define/public (on-changed-lang-values) (void))
    (define/public (on-changed-tokens gen beg end) (void))

    ;; A new object has an empty string and is at updated-generation
    ;; 0. The creator should then use update! to set the initial
    ;; string value and start the initial tokenization. That way both
    ;; `new` and `update!` return immediately, and all tokenization is
    ;; done on the updater thread.
    (define updated-generation  0)
    (define updated-position (sub1 min-position))

    (define content     lines:empty-text-lines)
    (define tokens      (new token-tree%))
    (define tokens-sema (make-semaphore 1))
    (define parens      (new paren-tree% [matches default-paren-matches]))
    (define parens-sema (make-semaphore 1))

    ;; These members correspond to various lang-info items.
    (define lexer             default-lexer)
    (define paren-matches     default-paren-matches)
    (define quote-matches     default-quote-matches)
    (define grouping-position racket-grouping-position)
    (define line-indenter     racket-amount-to-indent)
    (define range-indenter    #f)

    ;; Some simple public accessors
    (define/public (get-paren-matches) paren-matches)
    (define/public (get-quote-matches) quote-matches)

    ;; We don't expose the grouping or indenter functions to be called
    ;; directly; instead see `grouping` and `indent-x` methods below.
    ;; These are just boolean flags.
    (define/public (racket-grouping-position?)
      (equal? grouping-position racket-grouping-position))
    (define/public (range-indenter?)
      (and range-indenter #t))

    ;; Some methods intended just for tests
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
    (define/public (-get-lexer) lexer)
    (define/public (-get-line-indenter) line-indenter)
    (define/public (-get-range-indenter) range-indenter)
    #;
    (define/private (-show-tree msg t [offset 0])
      (displayln msg)
      (send t for-each
            (λ (-beg len dat)
              (define beg (+ -beg offset))
              (define end (+ beg len))
              (println (vector beg end (lines:get-text content beg end) dat)))))

    ;; position/c -> (or/c #f (list/c position/c position/c token?))
    ;;
    ;; Note: To be thread-safe must use tokens-sema.
    (define/private (token-ref pos)
      (send tokens search! pos)
      (define beg (send tokens get-root-start-position))
      (define end (send tokens get-root-end-position))
      (and (<= beg pos) (< pos end)
           (list beg end (send tokens get-root-data))))

    ;; ----------------------------------------------------------------------
    ;;
    ;; Coordinate progress of tokenizing updater thread

    ;; Allow threads to wait -- safely and without polling -- for the
    ;; updater thread to progress to a certain generation and
    ;; position. [Although I've never worked with "condition
    ;; variables" that seems ~= the synchronization pattern here?]
    (struct waiter (sema pred) #:transparent #:authentic)
    (define waiters-set (mutable-set)) ;(set/c waiter?)
    (define waiters-sema (make-semaphore 1)) ;for modifying waiters-set

    ;; Called from updater thread.
    (define/private (set-update-progress #:generation [g updated-generation]
                                         #:position   p)
      (with-semaphore waiters-sema
        (set! updated-generation g)
        (set! updated-position p)
        (for ([w (in-set waiters-set)])
          (when ((waiter-pred w))
            (semaphore-post (waiter-sema w))))))

    ;; Called from threads that need to wait for update progress to a
    ;; certain generation and position.
    (define/public (block-until-updated-thru gen [pos max-position])
      (define (pred) (and (<= gen updated-generation) (<= pos updated-position)))
      (unless (call-with-semaphore waiters-sema pred)  ;fast path
        (define pred-sema (make-semaphore 0))
        (define w (waiter pred-sema pred))
        (with-semaphore waiters-sema (set-add! waiters-set w))
        (semaphore-wait pred-sema) ;block
        (with-semaphore waiters-sema (set-remove! waiters-set w))))

    ;; -----------------------------------------------------------------
    ;;
    ;; Tokenizer updater thread

    ;; Entry thunk of our updater thread, which gets items from the
    ;; async channel `update-chan`, put there by the public `update!`
    ;; method.
    ;;
    ;; The only complexity here is that we tolerate update requests
    ;; arriving with out-of-order generation numbers. (This could
    ;; result from update! being called from various threads. For
    ;; example Racket Mode commands are each handled on their own
    ;; thread, much like a web server. As a rough analogy, this is
    ;; like handling TCP packets arriving possibly out of order.)
    ;;
    ;; TODO: Does this complexity belong here in this class, or should
    ;; it move outside? Strictly speaking this is about coordinating
    ;; multi-thread calls to our public update! method -- not about
    ;; coordinating our updater thread with other threads. This could
    ;; as easily live in e.g. hash-lang-bridge.rkt instead of here.
    (define update-chan (make-async-channel))
    (thread
     (λ ()
       (define pending-updates (make-hash))
       (let get ([next-update-gen 1])
         (match (async-channel-get update-chan)
           ['quit null] ;exit thread
           [(list 'update gen pos old-len new-str)
            (hash-set! pending-updates gen (list pos old-len new-str))
            (let do-pending ([next-update-gen next-update-gen])
              (match (hash-ref pending-updates next-update-gen #f)
                [(list pos old-len new-str)
                 (hash-remove! pending-updates next-update-gen)
                 (do-update! next-update-gen pos old-len new-str)
                 (do-pending (add1 next-update-gen))]
                [#f (get next-update-gen)]))]))))

    ;; Runs on updater thread.
    (define/private (do-update! gen pos old-len new-str)
      ;; Initial progress for other threads: Nothing yet within this
      ;; new generation.
      (set-update-progress #:generation gen
                           #:position (sub1 min-position))

      ;; Update the text-lines data structure.
      (when (< 0 old-len)
        (set! content (lines:delete content pos (+ pos old-len))))
      (define new-len (string-length new-str))
      (when (< 0 new-len)
        (set! content (lines:insert content pos new-str)))

      ;; If lang lexer changed, it could result in entirely different
      ;; tokens and parens, so in that case restart from scratch.
      (cond [(check-lang-info/lexer-changed? gen pos)
             (set! tokens (new token-tree%))
             (set! parens (new paren-tree% [matches paren-matches]))
             (update-tokens-and-parens min-position
                                       (lines:text-length content))]
            [else
             (update-tokens-and-parens pos
                                       (- new-len old-len))]))

    ;; Detect whether #lang changed AND ALSO (to avoid excessive
    ;; notifications and work) whether that changed any lang info
    ;; values we use. Notify if any changed, or if this is the first
    ;; generation. Return true IFF the lexer changed. For example this
    ;; will return false for a change from #lang racket to
    ;; racket/base.
    (define last-lang-end-pos 1)
    (define/private (check-lang-info/lexer-changed? gen pos)
      (define original-lexer lexer)
      (when (< pos last-lang-end-pos)
        (define in (lines:open-input-text content 0))
        (define info (or (with-handlers ([values (λ _ #f)])
                           (read-language in (λ _ #f)))
                         (λ (_key default) default)))
        (define-values (_line _col end-pos) (port-next-location in))
        (set! last-lang-end-pos end-pos) ;for checking next time

        (define any-changed? #f)
        (define-simple-macro (set!? var:id e:expr)
          (let ([val e])
            (unless (equal? var val)
              (set! var val)
              (set! any-changed? #t))))
        (set!? lexer             (info 'color-lexer default-lexer))
        (set!? paren-matches     (info 'drracket:paren-matches default-paren-matches))
        (set!? quote-matches     (info 'drracket:quote-matches default-quote-matches))
        (set!? grouping-position (info 'drracket:grouping-position racket-grouping-position))
        (set!? line-indenter     (info 'drracket:indentation racket-amount-to-indent))
        (set!? range-indenter    (info 'drracket:range-indentation #f))
        (when (or any-changed? (= gen 1))
          (on-changed-lang-values)))
      (not (equal? original-lexer lexer)))

    (define/private (update-tokens-and-parens pos diff)
      ;; Determine the position from which we need to start
      ;; re-tokenizing (this will be less than the edit position) and
      ;; the initial lexer mode.
      (define-values (initial-pos initial-mode)
        (with-semaphore tokens-sema
          ;; Find beginning of the token, if any, corresponding to the
          ;; edit position.
          (match (token-ref pos)
            [#f (values min-position #f)]
            [(list beg _end (struct* data ([backup backup])))
             ;; Initially back up by at least 1 (i.e. to the previous
             ;; token) or by this token's `backup` amount.
             (let loop ([pos (- beg (max 1 backup))])
               (match (token-ref pos)
                 [#f (values min-position #f)]
                 [(list beg _end (struct* data ([backup backup])))
                  (if (< 0 backup)
                      (loop (- beg backup))
                      ;; Finally, back up one more to get the initial
                      ;; lexer mode, if any. (Why: The mode stored
                      ;; with a token is state with which to read the
                      ;; _next_ token.)
                      (match (token-ref (sub1 beg))
                        [#f (values beg #f)]
                        [(list _beg _end (struct* data ([mode mode])))
                         (values beg mode)]))]))])))
      ;; Everything before this is valid; allow other threads to
      ;; progress thru that position of this generation.
      (set-update-progress #:position (sub1 initial-pos))

      ;; Split the token and paren trees.
      (define old-tokens (with-semaphore tokens-sema
                           (send tokens search! initial-pos)
                           (define-values (t1 t2) (send tokens split-before))
                           (set! tokens t1)
                           t2))
      (with-semaphore parens-sema
        (send parens split-tree initial-pos))

      (define in (lines:open-input-text content initial-pos))
      (define-values (min-changed-pos max-changed-pos)
        (let tokenize ([pos initial-pos]
                       [mode initial-mode]
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

             ;; Detect whether same as before (just shifted by `diff`)
             (send old-tokens search! (- new-beg initial-pos diff))
             (define old-beg (send old-tokens get-root-start-position))
             (define old-end (send old-tokens get-root-end-position))
             (define old-span (- old-end old-beg))
             (define old-tok (send old-tokens get-root-data))
             (define same? (and (equal? new-span old-span)
                                (equal? new-tok old-tok)))
             (define new-contig-same-count (+ contig-same-count (if same? 1 0)))
             (cond
               [(= new-contig-same-count 3)
                (send old-tokens search! old-beg)
                (define-values (_ keep) (send old-tokens split-after))
                (with-semaphore tokens-sema (insert-last! tokens keep))
                (define paren-keep-span (- (last-position) new-end))
                (with-semaphore parens-sema (send parens merge-tree paren-keep-span))
                (values min-changed-pos max-changed-pos)]
               [else
                (tokenize new-end
                          new-mode
                          new-contig-same-count
                          (if same? min-changed-pos (or min-changed-pos new-beg))
                          (if same? max-changed-pos (max max-changed-pos new-end)))])])))
      (on-changed-tokens updated-generation
                         (or min-changed-pos min-position)
                         max-changed-pos)
      (set-update-progress #:position max-position))

    ;; ------------------------------------------------------------
    ;;
    ;; Public methods for Emacs commands.

    (define/public (delete)
      (async-channel-put update-chan 'quit))

    ;; This method is safe to call from various threads.
    ;;
    ;; The method signature here is similar to that of Emacs'
    ;; after-change functions: Something changed starting at POS. The
    ;; text there used to be OLD-LEN chars long, but is now NEW-STR.
    (define/public (update! gen pos old-len new-str)
      ;;(-> generation/c position/c exact-nonnegative-integer? string? any)
      (unless (< updated-generation gen)
        (raise-argument-error 'update! "valid generation" 0 gen pos old-len new-str))
      (unless (<= min-position pos)
        (raise-argument-error 'update! "valid position" 1 gen pos old-len new-str))
      (async-channel-put update-chan
                         (list 'update gen pos old-len new-str)))

    ;; Can be called on any command thread.
    (define/public (classify gen pos)
      ;; (-> generation/c position/c (or/c #f (list/c position/c position/c (or/c symbol? hash-eq?))
      (block-until-updated-thru gen pos)
      (match (with-semaphore tokens-sema (token-ref pos))
        [(list beg end (struct* data ([attribs attribs])))
         (list beg end attribs)]
        [#f #f]))

    ;; Can be called on any command thread.
    (define/public (get-tokens gen
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
      (block-until-updated-thru gen pos)
      (with-semaphore tokens-sema
        (with-semaphore parens-sema
          (or (line-indenter this pos) ;may return #f meaning...
              (racket-amount-to-indent this pos)))))

    ;; Can be called on any command thread.
    (define/public (indent-range-amounts gen from upto)
      (cond [(not range-indenter) #f]
            [else
             (block-until-updated-thru gen upto)
             (with-semaphore tokens-sema
               (with-semaphore parens-sema
                 (range-indenter this from upto)))]))

    ;; -----------------------------------------------------------------
    ;; color-textoid<%> methods.
    ;;
    ;; Warning: As discussed above, these are thread-safe to call only
    ;; from the dyanamic extent of the `grouping`,
    ;; `indent-line-amount`, or `indent-range-amounts` methods.

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

(define default-lexer (waive-option module-lexer*))
(define default-paren-matches '((\( \)) (\[ \]) (\{ \})))
(define default-quote-matches '(#\" #\|))

(define (attribs->type attribs)
  (if (symbol? attribs)
      attribs
      (hash-ref attribs 'type 'unknown)))

(define (attribs->table attribs)
  (if (symbol? attribs)
      (hasheq 'type attribs)
      attribs))

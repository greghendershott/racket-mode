;; Copyright (c) 2020-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/async-channel
         racket/class
         racket/contract/base
         racket/contract/option
         racket/format
         racket/match
         syntax-color/token-tree
         syntax-color/paren-tree
         (only-in syntax-color/lexer-contract dont-stop)
         (only-in syntax-color/color-textoid color-textoid<%>)
         (only-in syntax-color/module-lexer module-lexer*)
         (only-in syntax-color/racket-lexer racket-lexer)
         (only-in syntax-color/racket-indentation racket-amount-to-indent)
         (only-in syntax-color/racket-navigation racket-grouping-position)
         syntax/parse/define
         "lang-info.rkt"
         (prefix-in lines: "text-lines.rkt")
         "util.rkt")

(provide hash-lang%
         generation/c
         position/c
         min-position
         (struct-out lang-info))

;; Overview
;;
;; An instance of a hash-lang% object can be used to represent program
;; source text and obtain information based on the #lang.
;;
;; The hash-lang% `update!` method may be called safely from any
;; thread to change the program source text (e.g. as the result of a
;; human editing the text). The `update!` method returns immediately;
;; the actual updating work is handled by a dedicated thread.
;; Furthermore the updater minimizes the work done for a change. As a
;; result it is fine to call `update!` frequently for edits that
;; insert or delete a single character, as well as for bigger changes.
;;
;; Each update! must specify a "generation", which is a strictly
;; successive increasing exact integer. A new object is generation 0;
;; the first update! must be generation 1. [It is fine if update!
;; calls are made from multiple threads and arrive with out-of-order
;; generation numbers; they are automatically queued and handled in
;; the correct order.]
;;
;; Other public methods -- `classify`, `get-tokens`, `grouping`,
;; `indent-line`, `indent-range` -- take both a generation and a
;; position. They automatically block until the updating thread has
;; progressed through that generation and position.
;;
;; The generation number is intended to support "distributed" use
;; patterns, where the editor might live in a different process or
;; even on a remote machine.
;;
;; As the updater thread works, it may produce "notifications" by
;; calling the `on-changed-lang-info` and `on-changed-token` methods.
;; This happens on the updater thread; the recipient should only queue
;; these (e.g. in an async channel) to handle later in some other
;; thread, and return immediately.
;;
;; `on-changed-lang-info` is called for the generation 1 update, as
;; well as for updates that change the #lang meaningfully (change lang
;; info values such as 'color-lexer or 'drracket:indentation).
;;
;; `on-changed-tokens` is called when an update! results in different
;; tokens for some span. The recipient should simply queue this
;; information in an async channel. What should it do when retrieving
;; them later? It depends on the program. One approach is to call
;; `get-tokens` eagerly for the entire invalid span and use the tokens
;; to color/propertize the entire span. Another approach is to record
;; the invalid span, but let some other mechanism call `get-tokens`
;; only if/as/when portions of the invalid span become visible to the
;; user, such as when they scroll. (The latter approach is what we use
;; in Emacs: Clear a "fontified" property for the invalid region, and
;; let the normal font-lock mechanism ask us to fontify visible
;; non-fontified areas.)
;;
;; Although this class implements the color-textoid<%> interface,
;; those methods are NOT intended to be used directly by a tool ---
;; for speed they are intentionally NOT thread-safe! Instead the
;; `grouping` and `indent-x` methods work by supplying these methods
;; to a lang grouper or indenter, within a single dynamic extent where
;; it is thread-safe to call them.
;;
;;
;; Portions originated from
;; /src/racket-lang/racket/share/pkgs/gui-lib/framework/private

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
    (define/public (on-changed-lang-info gen li) (void))
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

    ;; By default the lang is read from `content`, for when that
    ;; represents a source file containing #lang or a file module.
    ;; However `other-lang-source` may be a string used instead to
    ;; read the language, for a REPL buffer that should use the lang
    ;; from the file for which it is a REPL.
    (init-field [other-lang-source #f])
    (define lang-info (if other-lang-source
                        (read-lang-info (open-input-string other-lang-source))
                        default-lang-info))
    (define/public (get-lang-info) lang-info)

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
    ;; updater thread to progress to at least a given generation and
    ;; position.
    (define monitor (make-monitor))

    ;; Called from updater thread.
    (define/private (set-update-progress #:generation [g updated-generation]
                                         #:position   p)
      (progress monitor
                (λ ()
                  (set! updated-generation g)
                  (set! updated-position p))))

    ;; Called from threads that need to wait for update progress to a
    ;; certain generation and position.
    (define/public (block-until-updated-thru gen [pos max-position])
      (wait monitor
            (λ ()
              (and (<= gen updated-generation)
                   (<= pos updated-position)))))

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
         (match-define (cons gen more) (async-channel-get update-chan))
         (hash-set! pending-updates gen more)
         (let do-pending ([next-update-gen next-update-gen])
           (match (hash-ref pending-updates next-update-gen #f)
             [(list pos old-len new-str)
              (hash-remove! pending-updates next-update-gen)
              (do-update! next-update-gen pos old-len new-str)
              (do-pending (add1 next-update-gen))]
             [#f (get next-update-gen)])))))

    ;; Runs on updater thread.
    (define/private (do-update! gen pos old-len new-str)
      (define new-len (string-length new-str))
      ;; Initial progress for other threads: Nothing yet within this
      ;; new generation.
      (set-update-progress #:generation gen
                           #:position   (sub1 min-position))
      ;; Update the text-lines data structure.
      (when (< 0 old-len)
        (set! content (lines:delete content pos (+ pos old-len))))
      (when (< 0 new-len)
        (set! content (lines:insert content pos new-str)))
      ;; Update tokens and parens trees. If lang lexer changed, it
      ;; could result in entirely different tokens and parens, so in
      ;; that case restart from scratch.
      (cond [(check-lang-info/lexer-changed? gen pos)
             (set! tokens (new token-tree%))
             (set! parens (new paren-tree%
                               [matches (lang-info-paren-matches lang-info)]))
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
    (define last-lang-end-pos (add1 min-position))
    (define/private (check-lang-info/lexer-changed? gen pos)
      (define new-lang-info
        (cond
          [other-lang-source lang-info]
          [else
           (cond
             [(< pos last-lang-end-pos)
              (define in (lines:open-input-text content 0))
              (define-values (new-lang-info end-pos) (read-lang-info* in))
              (set! last-lang-end-pos end-pos) ;for checking next time
              new-lang-info]
             [else lang-info])]))
      (define any-changed? (not (equal? lang-info
                                        new-lang-info)))
      (define lexer-changed? (not (equal? (lang-info-lexer lang-info)
                                          (lang-info-lexer new-lang-info))))
      (set! lang-info new-lang-info)
      (when (or any-changed? (= gen 1))
        (on-changed-lang-info gen new-lang-info))
      lexer-changed?)

    (define/private (update-tokens-and-parens edit-pos diff)
      (define raw-lexer (if other-lang-source
                            (lang-info-lexer lang-info)
                            (waive-option module-lexer*)))
      ;; Determine the position from which we need to start
      ;; re-tokenizing (this will be less than the edit position) and
      ;; the initial lexer mode.
      (define-values (initial-pos initial-mode effective-lexer)
        (cond
          [(procedure-arity-includes? raw-lexer 3)
           (with-semaphore tokens-sema
             ;; Find beginning of the token, if any, corresponding to the
             ;; edit position.
             ;;
             ;; An update at the end can result in token-ref returning #f
             ;; so make an initial adjustment of edit-pos to give to
             ;; token-ref.
             (send tokens search! edit-pos)
             (define pos (send tokens get-root-start-position))
             (match (token-ref pos)
               [(list beg _end (struct* data ([backup backup])))
                ;; Initially back up by at least 1 (i.e. to the previous
                ;; token) or by this token's `backup` amount.
                (let loop ([pos (- beg (max 1 backup))])
                  (match (token-ref pos)
                    [(list beg _end (struct* data ([backup backup])))
                     (if (< 0 backup)
                         (loop (- beg backup))
                         ;; Finally, back up one more to get the initial
                         ;; lexer mode, if any. (Why: The mode stored
                         ;; with a token is state with which to read the
                         ;; _next_ token.)
                         (match (token-ref (sub1 beg))
                           [(list _beg _end (struct* data ([mode mode])))
                            (values beg mode raw-lexer)]
                           [#f (values beg #f raw-lexer)]))]
                    [#f (values min-position #f raw-lexer)]))]
               [#f (values min-position #f raw-lexer)]))]
          [(procedure-arity-includes? raw-lexer 1)
           (values min-position
                   'dummy-mode
                   (λ (port _pos _mode)
                     (define-values (lexeme attribs paren beg end)
                       (raw-lexer port))
                     (values lexeme attribs paren beg end beg 'dummy-mode)))]
          [else
           (error 'update-tokens-and-parens "Unknown lexer arity")]))
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

      ;; Run the lexer until it produces sufficient unchanged tokens.
      ;; Update token-tree and paren-tree. Track bounds of visible
      ;; changes to notify via on-changed-tokens.
      (define in (lines:open-input-text content initial-pos))
      (define-values (min-changed-pos max-changed-pos)
        (let tokenize ([pos initial-pos]
                       [mode initial-mode]
                       [previous-same? #f]
                       [contig-same-count 0]
                       [min-changed-pos max-position]
                       [max-changed-pos min-position])
          (define pos/port (add1 pos))
          (define-values (lexeme attribs paren beg/port end/port backup new-mode/ds)
            (effective-lexer in pos/port mode))
          (define-values (new-mode may-stop?)
            (match new-mode/ds
              [(struct* dont-stop ([val v])) (values v #f)]
              [v                             (values v #t)]))
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
             (define new-contig-same-count (if (and previous-same? same?)
                                               (add1 contig-same-count)
                                               0))
             (cond
               [(and may-stop?
                     ;; If enough same tokens in a row, assume
                     ;; tokenization has "converged" with old one and
                     ;; there is no need to continue. Here "3" is a
                     ;; WAG. [IIUC the framework colorer feels "1" is
                     ;; enough and relies on lexer dont-stop.]
                     (>= new-contig-same-count 3))
                (send old-tokens search! old-beg)
                (define-values (_ keep) (send old-tokens split-after))
                (with-semaphore tokens-sema (insert-last! tokens keep))
                (define paren-keep-span (- (last-position) new-end))
                (with-semaphore parens-sema (send parens merge-tree paren-keep-span))
                (values min-changed-pos max-changed-pos)]
               [else
                ;; For purposes of notifying clients to re-color we
                ;; use a stricter sense of "same" than we do for
                ;; deciding whether to continue lexing. Here we care
                ;; only whether the span and attributes are the same
                ;; (not whether backup or mode changed; those are N/A
                ;; for visible coloring changes).
                (define same-span/attribs?
                  (and (equal? new-span old-span)
                       (equal? (data-attribs new-tok) (data-attribs old-tok))))
                (tokenize new-end
                          new-mode
                          same?
                          new-contig-same-count
                          (if same-span/attribs?
                              min-changed-pos
                              (min min-changed-pos new-beg))
                          (if same-span/attribs?
                              max-changed-pos
                              (max max-changed-pos new-end)))])])))
      (on-changed-tokens updated-generation
                         min-changed-pos
                         max-changed-pos)
      (set-update-progress #:position max-position))

    ;; ------------------------------------------------------------
    ;;
    ;; Public methods for Emacs commands.

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
                         (list gen pos old-len new-str)))

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
         (define grouping-position (lang-info-grouping-position lang-info))
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
          (or ((lang-info-line-indenter lang-info) this pos) ;may return #f meaning...
              (racket-amount-to-indent this pos)))))

    ;; Can be called on any command thread.
    (define/public (indent-range-amounts gen from upto)
      (define range-indenter (lang-info-range-indenter lang-info))
      (cond [(not range-indenter) #f]
            [else
             (block-until-updated-thru gen upto)
             (with-semaphore tokens-sema
               (with-semaphore parens-sema
                 (range-indenter this from upto)))]))

    ;; Can be called on any command thread.
    (define/public (submit-predicate in eos?)
      (match (lang-info-submit-predicate lang-info)
        [(? procedure? p) (p in eos?)]
        [_ #f]))

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
      (lines:get-text content from (if (eq? upto 'eof) (last-position) upto)))

    (define/public (position-paragraph pos [eol? #f])
      (lines:position->line content (min pos (last-position))))

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
           [#f position])]))

    (define/public (get-regions)
      '((0 end)))))

(define default-lexer racket-lexer)
(define default-module-language #f)
(define default-paren-matches '((\( \)) (\[ \]) (\{ \})))
(define default-quote-matches '(#\" #\|))

(define default-lang-info
  (lang-info default-module-language
             default-lexer
             default-paren-matches
             default-quote-matches
             racket-grouping-position
             racket-amount-to-indent
             #f
             #f
             #f))

(define (read-lang-info* in)
  (define info (or (with-handlers ([values (λ _ #f)])
                     (read-language in (λ _ #f)))
                   (λ (_key default) default)))
  (define-values (_line _col end-pos) (port-next-location in))
  (define mod-lang (safe-info-module-language info))
  (values (lang-info mod-lang
                     (info 'color-lexer default-lexer)
                     (info 'drracket:paren-matches default-paren-matches)
                     (info 'drracket:quote-matches default-quote-matches)
                     (info 'drracket:grouping-position racket-grouping-position)
                     (info 'drracket:indentation racket-amount-to-indent)
                     (info 'drracket:range-indentation #f)
                     (info 'drracket:submit-predicate #f)
                     (comment-delimiters info mod-lang))
          end-pos))

;; Handle the module-language lang info key, as documented at
;; <https://docs.racket-lang.org/syntax/reader-helpers.html#%28mod-path._syntax%2Fmodule-reader%29>.
;; (info-proc -> (or/c #f string?)
(define (safe-info-module-language info)
  (define (handle v)
    (match v
      [(== default-module-language) default-module-language]
      [(? module-path? mp)
       (~a mp)]
      [(? syntax? stx)
       #:when (module-path? (syntax->datum stx))
       (~a (syntax->datum stx))]
      [(? procedure? p)
       (handle v)]
      [hopeless
       (log-racket-mode-debug "Ignoring value returned for module-language key: ~v"
                              info hopeless)
       default-module-language]))
  (handle (info 'module-language default-module-language)))

;; Return (list start continue end padding)
(define (comment-delimiters info mod-lang)
  (define (fallback)
    ;; Fallback when langs don't support the info key, or the value
    ;; isn't as expected.
    (define (root mp-str) ;e.g. 'racket and 'racket/base => 'racket
      (match mp-str
        [(pregexp "^([^/]+)" (list _ str))
         (string->symbol str)]
        [_ #f]))
    (match (root mod-lang)
      ["scribble" '("@;" "@;" "" " ")]
      ["rhombus"  '("//" "//" "" " ")]
      [_          '(";;" ";;" "" " ")]))
  (match (info 'drracket:comment-delimiters #f)
    [#f (fallback)]
    [(list* (list 'line (? string? start) (? string? padding))
            _other-styles)
     (list start start "" padding)]
    [(list* (list 'region (? string? start) (? string? continue) (? string? end) (? string? padding))
            _other-styles)
     (list start continue end padding)]
    [unexpected
     (log-racket-mode-warning
      "drracket:comment-delimiters from mod-lang ~v\n  unexpected value: ~v"
      mod-lang
      unexpected)
     (fallback)]))

(define (read-lang-info in)
  (define-values (v _pos) (read-lang-info* in))
  v)

(define (attribs->type attribs)
  (match attribs
    [(? symbol? s) s]
    [(? hash? ht) (hash-ref ht 'type 'unknown)]
    [_ 'unknown]))

(define (attribs->table attribs)
  (if (symbol? attribs)
      (hasheq 'type attribs)
      attribs))

;; This could be moved to its own file.
(module monitor racket/base
  (require racket/match
           syntax/parse/define)

  (provide make-monitor
           monitor?
           progress
           wait
           wait-evt)

  (struct monitor ([waiters #:mutable] sema) #:authentic)

  (struct waiter (pred sema) #:transparent #:authentic)

  (define (make-monitor)
    (monitor null (make-semaphore 1)))

  (define-simple-macro (with-semaphore sema e:expr ...+)
    (call-with-semaphore sema (λ () e ...)))

  ;; To be called by a worker thread, to make progress that might cause
  ;; some waiter's predicate to become true. The thunk is called within
  ;; the monitor's semaphore, so it is safe for it to e.g. set! multiple
  ;; variables.
  (define (progress m thunk)
    (with-semaphore (monitor-sema m)
      (thunk)
      (set-monitor-waiters!
       m
       (let loop ([waiters (monitor-waiters m)])
         (match waiters
           [(list) (list)]
           [(cons w more)
            (cond [((waiter-pred w))
                   (semaphore-post (waiter-sema w))
                   (loop more)] ;remove
                  [else ;keep
                   (cons w (loop more))])])))))

  ;; To be called by any number of observer threads, to wait until a
  ;; predicate becomes true. The predicate is checked initially in case
  ;; it is already true, but thereafter only whenever a worker thread
  ;; calls `progress`. The predicate is called within the monitor's
  ;; semaphore (if the `progress` thunk set!s multiple vars, it's safe
  ;; for the pred to check them).
  (define (wait m pred)
    (unless (call-with-semaphore (monitor-sema m) pred) ;fast path
      (semaphore-wait (wait-evt m pred))))

  ;; Like `wait` but returns a synchronizable event.
  (define (wait-evt m pred)
    (cond [(call-with-semaphore (monitor-sema m) pred) ;fast path
           always-evt]
          [else
           (define pred-sema (make-semaphore 0))
           (with-semaphore (monitor-sema m)
             (set-monitor-waiters! m (cons (waiter pred pred-sema)
                                           (monitor-waiters m))))
           pred-sema]))

  (module+ example
    ;; Some variables that a worker thread will increase monotonically.
    (define i 0)
    (define j 0)
    ;; A monitor object
    (define m (make-monitor))
    ;; Some threads that want to wait for certain values.
    (void (thread (λ ()
                    (define (pred-0) (and (<= 0 i)))
                    (wait m pred-0)
                    (displayln "pred-0 became true (fast path)"))))
    (void (thread (λ ()
                    (define (pred-i-3-j-6) (and (<= 3 i) (<= 6 j)))
                    (wait m pred-i-3-j-6)
                    (displayln "pred-i-3-j-6 became true"))))
    (void (thread (λ ()
                    (define (pred-i-5) (<= 5 i))
                    (wait m pred-i-5)
                    (displayln "pred-i-5 became true"))))
    ;; A worker thread.
    (let loop ()
      (progress m (λ ()
                    (set! i (add1 i))
                    (set! j (add1 j))
                    (displayln (list i j))))
      (when (< i 10)
        (sleep 0.5)
        (loop)))))
(require 'monitor)

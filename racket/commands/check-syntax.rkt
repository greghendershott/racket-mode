#lang racket/base

(require data/interval-map
         racket/dict
         racket/format
         racket/match
         racket/set
         racket/string
         racket/class
         drracket/check-syntax
         "../imports.rkt"
         "../online-check-syntax.rkt"
         "../syntax.rkt"
         "../util.rkt")

(provide check-syntax)

(module+ test
  (require rackunit))

;; Our front-end issues check-syntax requests after the user edits a
;; buffer, plus a short idle delay (e.g. 1 second).
;;
;; On complex inputs it can take many seconds to expand and analyze.
;; (By far mostly the expansion; although we do cache expanded syntax,
;; that doesn't always help.)
;;
;; As a result, we might be called to do a path-str for which a
;; previous command thread is still running. Although that will work
;; _correctly_, eventually, it is wasteful -- both here in the
;; back-end (calculating) and in the front-end (updating buffer
;; properties).
;;
;; Instead: We'd like to kill the old command thread and inform our
;; front-end that the old command was "canceled". We can do so here
;; simply by `break-threading`ing the thread already running the
;; command for the same file.
;;
;; How/why this works: See how command-server.rkt handles exn:break by
;; returning a `(break)` response, and, how racket-cmd.el handles that
;; by doing nothing (except cleaning up its nonce->callback
;; hash-table).

(define check-syntax
  (let ([sema (make-semaphore 1)] ;guard concurrent use of ht
        [ht   (make-hash)])       ;path-str -> thread
    (λ (path-str code-str)
      (dynamic-wind
        (λ () (call-with-semaphore
               sema
               (λ ()
                 (define (break-thread/log thd)
                   (log-racket-mode-info "cancel ~v" thd)
                   (break-thread thd))
                 (cond [(hash-ref ht path-str #f) => break-thread/log])
                 (hash-set! ht path-str (current-thread)))))
        (λ () (do-check-syntax path-str code-str))
        (λ () (call-with-semaphore
               sema
               (λ ()
                 (log-racket-mode-debug "(current-memory-use) ~v"
                                        (current-memory-use))
                 (hash-remove! ht path-str))))))))

;; Note: Instead of using the `show-content` wrapper, we give already
;; fully expanded syntax directly to `make-traversal`. Why? Expansion
;; can be slow. 1. We need exp stx for other purposes here. Dumb to
;; expand twice. 2. We might not even need to expand once, now:
;; string->expanded-syntax maintains a cache.
;;
;; [One nuance of caching expanded syntax is that we also cache the
;; namespace used while expanding -- that needs to be the
;; current-namespace for things like module->imports. Likewise
;; current-load-relative-directory. That is why
;; string->expanded-syntax uses a "call-with" "continuation style": it
;; sets parameters when calling the continuation function.]

(define (do-check-syntax path-str code-str)
  (define path (string->path path-str))
  (parameterize ([current-annotations   (new annotations-collector%
                                             [src      path]
                                             [code-str code-str])]
                 [error-display-handler (our-error-display-handler path-str code-str)]
                 [pre-exn-errors        '()])
    (with-handlers ([exn:fail? (handle-fail path-str code-str)])
      (with-time/log (~a "total " path-str)
        (string->expanded-syntax path code-str analyze)))))

(define (analyze stx)
  (define-values (expanded-expression expansion-completed)
    (make-traversal (current-namespace)
                    (current-load-relative-directory)))
  (with-time/log 'drracket/check-syntax/expanded-expression
    (expanded-expression stx))
  (with-time/log 'drracket/check-syntax/expansion-completed
    (expansion-completed))

  (define annotations
    (with-time/log 'get-annotations
      (send (current-annotations) get-annotations)))

  (define completions-set (send (current-annotations) get-local-completion-candidates))
  (with-time/log 'imports
    (imports stx completions-set))
  (define completions (sort (set->list completions-set)
                            string<=?))

  (define imenu (send (current-annotations) get-imenu-index))

  (list 'check-syntax-ok
        (cons 'completions completions)
        (cons 'imenu       imenu)
        (cons 'annotations annotations)))

(define annotations-collector%
  (class (annotations-mixin object%)
    (init-field src code-str)

    (define im-mouse-overs (make-interval-map))
    (define im-jumps (make-interval-map))
    (define im-docs (make-interval-map))
    (define im-unused-requires (make-interval-map))
    (define ht-defs/uses (make-hash))
    (define ht-imenu (make-hash))
    (define local-completion-candidates (mutable-set))
    (define ht-tails (make-hash))

    ;; I've seen drracket/check-syntax return bogus positions for e.g.
    ;; add-mouse-over-status so here's some validation.
    (define code-len (string-length code-str))
    (define (valid-pos? pos) (and (<= 0 pos) (< pos code-len)))
    (define (valid-beg/end? beg end)
      (and (< beg end) (valid-pos? beg) (valid-pos? end)))

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    (define/override (syncheck:add-arrow/name-dup/pxpy
                      _def-src def-beg def-end _def-px _def-py
                      _use-src use-beg use-end _use-px _use-py
                      _actual? _level require-arrow? _name-dup?)
      (when (and (valid-beg/end? def-beg def-end)
                 (valid-beg/end? use-beg use-end))
        ;; Consolidate the add-arrow/name-dup items into a hash table
        ;; with one item per definition. The key is the definition position.
        ;; The value is the set of its uses' positions.
        (hash-update! ht-defs/uses
                      (list (substring code-str def-beg def-end)
                            (match require-arrow?
                              ['module-lang 'module-lang]
                              [#t           'import]
                              [#f           'local])
                            (add1 def-beg)
                            (add1 def-end))
                      (λ (v) (set-add v (list (add1 use-beg)
                                              (add1 use-end))))
                      (set))
        (unless require-arrow?
          (send this syncheck:add-mouse-over-status "" use-beg use-end "defined locally"))))

    (define/override (syncheck:add-tail-arrow from-src from-pos to-src to-pos)
      ;; Note: "from" and "to" are in terms of DrRacket _arrow_
      ;; direction, which it draws _opposite_ of the _jump_ direction.
      ;; Therefore we reverse the positions below.
      (define head from-pos)
      (define tail to-pos)
      ;; AFAICT the sources should always = the source being analyzed
      ;; -- i.e. the head and tail should be in the same source file.
      ;; If a macro has neglected to supply good srcloc, and so e.g.
      ;; the srcloc of the tail target is inside the macro source, we
      ;; have no good way to show that to the user, so ignore it.
      (match* [from-src to-src src]
        [[v v v]
         ;; Consolidate to hash-table much like defs/uses
         (hash-update! ht-tails
                       (add1 head)
                       (λ (v) (set-add v (add1 tail)))
                       (set))]
        [[_ _ _]
         (log-racket-mode-warning
          "Ignoring syncheck:add-tail-arrow because sources differ ~v"
          (list from-src to-src src))]))

    (define/override (syncheck:add-mouse-over-status _src beg end status)
      (when (valid-beg/end? beg end)
        ;; Avoid silly "imported from “\"file.rkt\"”"
        (define cleansed (regexp-replace* #px"[“””]" status ""))
        ;; Automatically append multiple mouse-over messages for the
        ;; same interval.
        (interval-map-update*! im-mouse-overs beg end
                               (λ (s) (set-add s cleansed))
                               (set cleansed))
        ;; Find local completion candidates by looking at "bound
        ;; occurrences" mouseovers. Why not look at the definitions in
        ;; syncheck:add-arrow annotations? Because there won't be any
        ;; annotation when a definition isn't yet _used_ (drracket
        ;; doesn't need to draw an arrow from nothing to something).
        ;; There _will_ however be a "no bound occurrences" mouseover.
        ;; Therefore, although it's hacky to match on "occurrences"
        ;; strings here, it's the least worst way to get _all_ local
        ;; completion candidates. It's the same reason why we go to
        ;; the work in imported-completions to find _everything_
        ;; imported that _could_ be used.
        (when (or (regexp-match? #px"^\\d+ bound occurrences?$" status)
                  (equal? status "no bound occurrences"))
          (set-add! local-completion-candidates (substring code-str beg end)))))

    ;; These are module-level definitions (not lexical bindings). So
    ;; they are useful for things like imenu. Also these are a good
    ;; source of extra completion candidates, because they can include
    ;; macro-generated bindings -- like `foo?` and `foo-bar` from
    ;; `(struct foo (bar))` -- which wouldn't necessarily otherwise be
    ;; annotated by syncheck:add-arrow or syncheck:add-mouse-over. But
    ;; for that same reason, they're not useful for ht-def/uses
    ;; because many of them may overlap for the same source span,
    ;; which won't work well for front-end features.
    (define/override (syncheck:add-definition-target _src beg _end symbol rev-mods)
      (let trie-set! ([ht   ht-imenu]
                      [keys (reverse
                             (cons (~a symbol)
                                   (for/list ([s (in-list rev-mods)])
                                     (~a "Module: " s))))])
        (match keys
          [(list key)      (hash-set! ht key (add1 beg))]
          [(cons key more) (trie-set! (hash-ref! ht key (make-hash)) more)]))
      (set-add! local-completion-candidates (~a symbol)))

    (define/override (syncheck:add-jump-to-definition _src beg end id-sym path submods)
      ;; - drracket/check-syntax only reports the file, not the
      ;;   position within. We can find that using our def-in-file.
      ;;
      ;; - drracket/check-syntax uses identifier-binding which can't
      ;;   follow some contracting and renaming provides. As a result,
      ;;   the value of the id here can be wrong. For example it will
      ;;   report "provide/contract-id-make-traversal.1" for
      ;;   "make-traversal". When the reported id differs from that in
      ;;   the source text, we report both to try with def-in-file.
      ;;
      ;; However, calling def-in-file here/now for all jumps would be
      ;; quite slow. Futhermore, a user might not actually use the
      ;; jumps -- maybe not any, probably not most, certainly not all.
      ;;
      ;; Sound like a job for a thunk, e.g. racket/promise
      ;; delay/force? We can't marshal a promise between Racket back
      ;; end and Emacs front end. We can do the moral equivalent:
      ;; Simply return the info that the front end should give to the
      ;; "def/drr" command if/as/when needed.
      (when (and (valid-beg/end? beg end) (file-exists? path))
        (define src-str (substring code-str beg end))
        (define drracket-id-str (symbol->string id-sym))
        (interval-map-set! im-jumps beg end
                           (list (path->string path)
                                 submods
                                 (if (equal? drracket-id-str src-str)
                                     (list drracket-id-str)
                                     (list drracket-id-str src-str))))))

    (define/override (syncheck:add-docs-menu _src beg end _sym _label path _anchor anchor-text)
      (when (valid-beg/end? beg end)
        (interval-map-set! im-docs beg end
                           (list (path->string path)
                                 anchor-text))))

    (define/override (syncheck:add-unused-require _src beg end)
      (when (valid-beg/end? beg end)
        (interval-map-set! im-unused-requires beg end (list))))

    (define/public (get-annotations)
      ;; Obtain any online-check-syntax log message values and treat
      ;; them as mouse-overs.
      (for ([v (in-set (current-online-check-syntax))])
        (match-define (list beg end str) v)
        (send this syncheck:add-mouse-over-status "" beg end str))

      ;; Convert ht-defs/uses to a list of defs, each of whose uses
      ;; are sorted by positions.
      (define defs/uses
        (with-time/log 'defs/uses
          (for/list ([(def uses) (in-hash ht-defs/uses)])
            (match-define (list sym req def-beg def-end) def)
            (list 'def/uses
                  def-beg def-end
                  req sym
                  (sort (set->list uses) < #:key car)))))
      (define targets/tails
        (for/list ([(target tails) (in-hash ht-tails)])
          (list 'target/tails target (sort (set->list tails) <))))
      ;; Convert the interval maps for other annotations into simple
      ;; lists of the form (list symbol beg end value ...). Also add1
      ;; positions for Emacs.
      (define (im->list im sym [proc values])
        (for/list ([(beg/end vs) (in-dict im)])
          (match-define (cons beg end) beg/end)
          (list* sym (add1 beg) (add1 end) (proc vs))))
      (define (mouse-over-set->result v)
        (list ;im->list expects a list
         (string-join (sort (set->list v) string<=?)
                      "; ")))
      ;; Append all and sort by `beg` position
      (sort (append
             defs/uses
             targets/tails
             (im->list im-mouse-overs     'info mouse-over-set->result)
             (im->list im-jumps           'jump)
             (im->list im-docs            'doc)
             (im->list im-unused-requires 'unused-require))
            < #:key cadr))

    (define/public (get-local-completion-candidates)
      local-completion-candidates)

    (define/public (get-imenu-index)
      (let ht->alist ([ht ht-imenu])
        (sort (for/list ([(k v) (in-hash ht)])
                (cons k (match v
                          [(? hash? ht) (ht->alist ht)]
                          [(? number? n) n])))
              <
              #:key (match-lambda [(cons _ (? number? n)) n]
                                  [_ 0]))))

    (super-new)))

;; Typed Racket can report multiple errors. The protocol: it calls
;; error-display-handler for each one. There is a final, actual
;; exn:fail:syntax raised, but it's not useful for us: Although its
;; srclocs correspond to the locations, its message is just a summary.
;; Here we collect each message and location in a parameter, and when
;; the final summary exn is raised, we ignore it and use these. Note
;; that Typed Racket is the only such example I'm aware of, but if
;; something else wanted to report multiple errors, and it used a
;; similar approach, we'd handle it here, too.
(define pre-exn-errors (make-parameter '()))
(define ((our-error-display-handler path-str code-str) msg exn)
  (when (and (exn:fail:syntax? exn)
             (exn:srclocs? exn))
    (pre-exn-errors (append (pre-exn-errors)
                            (exn->errors path-str code-str exn)))))

(define ((handle-fail path-str code-str) e)
  (define errors
    (cond
      ;; Multiple errors. See comment above.
      [(not (null? (pre-exn-errors)))
       (pre-exn-errors)]
      ;; The intended use of exn:srclocs is a _single_ error, with zero
      ;; or more locations from least to most specific -- not multiple
      ;; errors.
      [(exn:srclocs? e)
       (exn->errors path-str code-str e)]
      ;; A single error with no srcloc at all. Although probably
      ;; unlikely with exn:fail:syntax during expansion (as opposed to
      ;; runtime errors) do handle it:
      [else
       (default-errors path-str code-str e)]))

  ;; Even if expansion failed, there might be annotations from the
  ;; online-check-syntax-logger protocol -- indeed that scenario is a
  ;; motivation for the protocol -- so be sure to use them here.
  (define annotations (send (current-annotations) get-annotations))

  (list 'check-syntax-errors
        (cons 'errors      errors)
        (cons 'annotations annotations)))

(define (exn->errors path-str code-str e)
  (define (->path-string v)
    (match v
      [(? path-string? v) v]
      [(? path? v)        (path->string v)]))
  (match ((exn:srclocs-accessor e) e)
    [(list)
     (match e
       ;; exn:fail:syntax and exn:fail:read can have empty srclocs
       ;; list -- but additional struct member has list of syntaxes
       ;; from least to most specific. Use the most-specific, only.
       [(or (exn:fail:syntax msg _marks (list _ ... stx))
            (exn:fail:read   msg _marks (list _ ... stx)))
        #:when (not (exn:fail:read:eof? e))
        (define pos  (syntax-position stx))
        (define span (syntax-span stx))
        (cond [(and pos span)
               (list
                (list 'error
                      (->path-string (or (syntax-source stx) path-str))
                      pos
                      (+ pos span)
                      msg))]
              [else (default-errors path-str code-str e)])]
       [_ (default-errors path-str code-str e)])]
    [(list _ ... (? srcloc? most-specific))
     (match-define (srcloc path _ _ pos span) most-specific)
     (list
      (list 'error
            (->path-string path)
            pos
            (+ pos span)
            (exn-message e)))]
    [_ (default-errors path-str code-str e)]))

(define (default-errors path-str code-str e)
  ;; As a fallback, here, we extract position from the exn-message.
  ;; Unfortunately that's line:col and we need to return beg:end.
  (define pos (exn-message->pos code-str (exn-message e)))
  (list
   (list 'error
         path-str
         pos
         (add1 pos)
         (exn-message e))))

(define (exn-message->pos code-str msg)
  (match msg
    [(pregexp "^.+?:(\\d+)[:.](\\d+): "
              (list _ (app string->number line) (app string->number col)))
     (define in (open-input-string code-str))
     (port-count-lines! in)
     (let loop ([n 1])
       (cond [(= n line)                   (+ 1 (file-position in) col)]
             [(eof-object? (read-line in)) 1]
             [else                         (loop (add1 n))]))]
    [_ 1]))

(module+ test
  (check-equal?
   (exn-message->pos "12\n4567\n9"
                     ;      ^
                     "/path/to/foo.rkt:2:2: some problem")
   6)
  (check-equal?
   (exn-message->pos "012\n4567\n9"
                     ;       ^
                     "/path/to/foo.rkt:999:2: some problem")
   1))

(module+ example
  (require racket/file)
  (define (check-file path)
    (time (do-check-syntax path (file->string path))))
  (check-file (path->string (syntax-source #'here))))

(module+ test
  (require racket/file)
  (define (check-this-file path)
    (check-not-exn
     (λ ()
       (time (do-check-syntax path (file->string path))))))
  ;; Twice to exercise and test cache
  (check-equal? (check-this-file (path->string (syntax-source #'here)))
                (check-this-file (path->string (syntax-source #'here)))))

(module+ slow-test
  ;; To a large extent this is a test of the syntax cache in
  ;; syntax.rkt -- a sanity check that the eviction strategy is
  ;; working to avoid an unbounded and excessive growth in
  ;; current-memory-use.
  ;;
  ;; Probably most consistent way to run is outside Emacs with:
  ;;
  ;;   raco test --submodule slow-test check-syntax.rkt
  (require rackunit
           racket/file
           racket/path)
  (for ([_ 2]) (collect-garbage))
  (define start (current-seconds))
  (define least (current-memory-use))
  (define most  least)
  (define count 0)
  (for* ([roots (in-list '(("racket.rkt" "typed")
                           ("core.rkt" "typed-racket")
                           ("main.rkt" "racket")))]
         [path  (in-directory
                 (path-only
                  (apply collection-file-path roots)))]
         #:when (equal? #"rkt" (filename-extension path)))
    (set! count (add1 count))
    (check-syntax (path->string path) (file->string path))
    (define after (current-memory-use))
    (printf "~a, ~a, ~v\n" count after (path->string path))
    (set! least (min least after))
    (set! most  (max most  after)))
  (printf "Time:  ~a seconds\n" (- (current-seconds) start))
  (printf "Least: ~a bytes\n" least)
  (printf "Most:  ~a bytes\n" most)
  (define mem-use-diff (- most least))
  (printf "Diff:  ~a bytes\n" mem-use-diff)
  (check-true (< mem-use-diff 700000000)))

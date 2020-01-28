#lang racket/base

(require racket/format
         (only-in racket/list remove-duplicates)
         racket/match
         racket/path
         racket/promise
         racket/set
         racket/class
         drracket/check-syntax
         ;; drracket/check-syntax does not re-provide build-trace%
         (only-in drracket/private/syncheck/traversals
                  build-trace%)
         "../identifier.rkt"
         "../imports.rkt"
         "../syntax.rkt"
         "../util.rkt")

(provide check-syntax)

(define (check-syntax path-str code-str)
  ;; Note: We adjust all positions to 1-based Emacs `point' values.
  ;;
  ;; Note: Instead of using `show-content`, pass already-expanded stx
  ;; directly to `make-traversal`. Why? 1. We also need expanded stx
  ;; for other purposes, below. Expansion can be slow. Dumb to do
  ;; twice. 2. Furthermore, we maintain a cache of expanded stx in
  ;; syntax.rkt. Smarter to do zero or one times.
  ;;
  ;; One nuance of caching expanded syntax is that we also cache the
  ;; namespace used while expanding -- that needs to be the
  ;; current-namespace for things like module->imports.
  (define path (string->path path-str))
  (define dir (path-only path))
  (with-handlers ([exn:fail? (handle-fail path-str)])
    (define-values (stx ns)
      (parameterize ([current-namespace (make-base-namespace)])
        (string->expanded-syntax-and-namespace path code-str)))
    (define o (new build-trace% [src path]))
    (parameterize ([current-annotations o])
      (define-values (expanded-expression expansion-completed)
        (make-traversal ns path))
      (parameterize ([current-namespace ns])
        (expanded-expression stx))
      (expansion-completed))
    (define synchecks (send o get-trace))

    ;;
    ;; Annotations
    ;;;

    ;; I've seen drracket/check-syntax return bogus positions for e.g.
    ;; add-mouse-over-status so here's some validation.
    (define code-len (string-length code-str))
    (define (valid-pos? pos) (and (<= 0 pos) (< pos code-len)))
    (define (valid-beg/end? beg end)
      (and (< beg end) (valid-pos? beg) (valid-pos? end)))

    ;; [1] Most syncheck:xxx items we simply transform 1:1.
    (define infos
      (remove-dupes-and-falses
       (for/list ([x (in-list synchecks)])
         ;; Give these all a common prefix so we can sort.
         (define (item sym beg end . more)
           (list* sym (add1 beg) (add1 end) more))
         (match x
           [(vector 'syncheck:add-mouse-over-status beg end str)
            #:when (valid-beg/end? beg end)
            ;; Avoid silly "imported from “\"file.rkt\"”"
            (define cleansed (regexp-replace* #px"[“””]" str ""))
            (item 'info beg end
                  cleansed)]
           [(vector 'syncheck:add-docs-menu
                    beg
                    end
                    _sym
                    _
                    help-path
                    _anchor
                    help-anchor-text)
            (item 'doc beg end
                  (path->string help-path)
                  help-anchor-text)]
           [(vector 'syncheck:add-jump-to-definition
                    beg
                    end
                    _sym ;unreliable, see comment below
                    path
                    submods)
            ;; Note: It would be much too slow to find all definitions
            ;; _within_ files, now. Instead, just supply supply this
            ;; information, and the front-end can give it to a new
            ;; command `find-definition-in-file`, if/as/when the user
            ;; wants to visit something.
            ;;
            ;; Note: drracket/check-syntax isn't as smart about
            ;; contracting and/or renaming provides as is our own
            ;; find.rkt. As a result, the value of `sym` here can be
            ;; wrong. e.g. For get-pure-port from net/url it will say
            ;; the sym is "provide/contract-id-make-traversal.1". So,
            ;; just ignore the sym and use the text from the source
            ;; code. Usually our find-definition-in-file can find it
            ;; when given the original user text.
            (item 'external-def beg end
                  (substring code-str beg end)
                  ;; FIXME: I'm seeing bogus path here e.g. open /home/greg/src/racket-lang/racket/src/expander/namespace then put point on "namespace-get-root-expand-ctx" near end of file and path is "/home/greg/src/racket-lang/racket/src/expander/namespace/api.rkt/namespace.rkt" -- bug in drracket/check-syntax ??
                  (path->string path)
                  submods)]
           [_ #f]))))
    ;; [2] Consolidate the add-arrow/name-dup items into a hash table
    ;; with one item per definition. The key is the definition
    ;; position. The value is the set of its uses' positions.
    (define ht-defs/uses (make-hash))
    (for ([x (in-list synchecks)])
      (match x
        [(or (vector 'syncheck:add-arrow/name-dup
                     def-beg def-end
                     use-beg use-end
                     _ _ req _)
             (vector 'syncheck:add-arrow/name-dup/pxpy
                     def-beg def-end _ _
                     use-beg use-end _ _
                     _ _ req _))
         (hash-update! ht-defs/uses
                       (list (substring code-str def-beg def-end)
                             (match req
                               ['module-lang 'module-lang]
                               [#t           'import]
                               [#f           'local])
                             (add1 def-beg)
                             (add1 def-end))
                       (λ (v) (set-add v (list (add1 use-beg)
                                               (add1 use-end))))
                       (set))]
        [_ #f]))
    ;; Convert the hash table into a list, sorting the usage positions.
    (define defs/uses
      (for/list ([(def uses) (in-hash ht-defs/uses)])
        (match-define (list sym req def-beg def-end) def)
        (list 'def/uses
              def-beg def-end
              req sym
              (sort (set->list uses) < #:key car))))
    ;; [3] `annotations` = `infos` + `defs/uses`
    (define annotations (sort (append infos defs/uses) < #:key cadr))

    ;;
    ;; Completion candidates (including their defn locs)
    ;;
    (define completions (make-hash))

    ;; [1] Locals. When a definition isn't yet used, there will be no
    ;; syncheck:add-arrow annotation because drracket doesn't need to
    ;; draw an arrow from something to nothing. There _will_ however
    ;; be an "no bound occurrences" mouseover. Although it's hacky to
    ;; match on a string like that, it's the best way to get _all_
    ;; local completion candidates. It's the same reason why we go to
    ;; the work in imported-completions to find _everything_ imported,
    ;; that _could_ be used.
    (for ([x (in-list synchecks)])
      (match x
        [(vector 'syncheck:add-mouse-over-status beg end
                 (or "no bound occurrences"
                     (pregexp "^\\d+ bound occurrences?$")))
         #:when (valid-beg/end? beg end)
         (hash-set! completions
                    (substring code-str beg end)
                    (add1 beg))]
        [_ (void)]))
    ;; [2] Imports
    (parameterize ([current-load-relative-directory dir]
                   [current-namespace               ns])
      (define (location-info v)
        ;; Simplify what identifier-binding* returns into an
        ;; actionable list of 0, 1, or 2 locations that later could
        ;; be given to the find-def-in-files command.
        (match (identifier-binding* v #:expanded-module-syntax stx)
          [(? list? xs)
           (remove-dupes-and-falses
            (for/list ([x (in-list xs)])
              (match x
                [(cons _ 'kernel) #f] ;omit kernel
                [(list* (app symbol->string sym) (app path->string path) subs)
                 ;; When sym is same use #f instead of repeating
                 (define maybe-sym (and (not (equal? sym v)) sym))
                 (list maybe-sym path subs)])))]
          [_ (list)]))
      (for ([v (in-set (imports stx (set)))])
        (hash-set! completions
                   v
                   (location-info v))))

    ;;
    ;; Final answer for Emacs front-end
    ;;
    (list 'check-syntax-ok
          (cons 'completions completions)
          (cons 'annotations annotations))))

(define ((handle-fail path) e)
  (cons 'check-syntax-errors
        (cond [(exn:srclocs? e)
               ;; FIXME: This isn't satisfactory with e.g. Typed Racket.
               ;; The messages are all e.g. "3 type-check problems",
               ;; not the actual specific problems/locations.
               (for/list ([sl (in-list ((exn:srclocs-accessor e) e))])
                 (match sl
                   [(srcloc path _ _ ofs span)
                    (list 'error (path->string path)
                          ofs (+ ofs span)
                          (exn-message e))]))]
              [else
               (list
                (list 'error path 1 0 (exn-message e)))])))

(define (remove-dupes-and-falses xs)
  (remove-duplicates (filter values xs)))

(module+ example
  (require racket/file)
  (define path (path->string (syntax-source #'here)))
  (check-syntax path (file->string path)))

(module+ test
  (require rackunit
           racket/file)
  (define (check-this-file path)
    (check-not-exn
     (λ ()
       (time (void (check-syntax path (file->string path)))))))
  (check-this-file (path->string (syntax-source #'here)))
  (check-this-file (path->string (syntax-source #'here))))

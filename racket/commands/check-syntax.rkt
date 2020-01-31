#lang racket/base

(require racket/format
         (only-in racket/list remove-duplicates)
         racket/match
         racket/path
         racket/promise
         racket/set
         racket/class
         syntax/parse/define
         drracket/check-syntax
         ;; drracket/check-syntax does not re-provide build-trace%
         (only-in drracket/private/syncheck/traversals
                  build-trace%)
         "../imports.rkt"
         "../syntax.rkt")

(provide check-syntax)

(define-logger racket-mode-check-syntax)

(define (time-apply/log what proc args)
  (define-values (vs cpu real gc) (time-apply proc args))
  (define (fmt n) (~v #:align 'right #:min-width 4 n))
  (log-racket-mode-check-syntax-info "~a cpu | ~a real | ~a gc <= ~a"
                                     (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

(define (check-syntax path-str code-str)
  (with-time/log (~a "total " path-str)
    (-check-syntax path-str code-str)))

(define (-check-syntax path-str code-str)
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
  (parameterize ([current-directory               dir]
                 [current-load-relative-directory dir])
    (with-handlers ([exn:fail? (handle-fail path-str)])
      (define-values (stx ns)
        (parameterize ([current-namespace (make-base-namespace)])
          (with-time/log (~a "expand-maybe-from-cache " path-str)
            (string->expanded-syntax-and-namespace path code-str))))
      (define o (new build-trace% [src path]))
      (parameterize ([current-annotations o])
        (define-values (expanded-expression expansion-completed)
          (make-traversal ns dir))
        (parameterize ([current-namespace ns])
          (with-time/log 'drracket/check-syntax/expanded-expression
            (expanded-expression stx)))
        (with-time/log 'drracket/check-syntax/expansion-completed
          (expansion-completed)))
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
        (with-time/log 'infos
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
                        (app path->string help-path-str)
                        _anchor
                        help-anchor-text)
                (item 'doc beg end
                      help-path-str
                      help-anchor-text)]
               [(and (vector 'syncheck:add-jump-to-definition
                             beg
                             end
                             (app symbol->string drracket-id-str)
                             path
                             submods)
                     syncheck)
                ;; drracket/check-syntax only reports the file, not
                ;; the position within. We can find that using our
                ;; def-in-file.
                ;;
                ;; drracket/check-syntax uses identifier-binding which
                ;; isn't smart about contracting and/or renaming
                ;; provides. As a result, the value of the id here can
                ;; be wrong. e.g. For "make-traversal" it will report
                ;; "provide/contract-id-make-traversal.1". It's good
                ;; to try both ids with def-in-file.
                ;;
                ;; However, doing so here/now would be quite slow.
                ;; Futhermore, a user might not actually use the jumps
                ;; -- maybe not any, probably not most, certainly not
                ;; all. So instead do a "lazy" approach here where we
                ;; simply return both possible id names, and the
                ;; filename and submods. The front end can call a
                ;; find-definition/drracket-jump command, to "force"
                ;; if/as/when needed.
                (cond [(file-exists? path)
                       (define orig-str (substring code-str beg end))
                       (item 'external-def beg end
                             path
                             submods
                             (if (equal? drracket-id-str orig-str)
                                 (list drracket-id-str)
                                 (list drracket-id-str orig-str)))]
                      [else
                       ;; https://gist.github.com/greghendershott/5dd59c00f8daa2ce0987ad343244489e
                       (log-racket-mode-check-syntax-error "bad path in ~v" syncheck)])]
               [_ #f])))))
      ;; [2] Consolidate the add-arrow/name-dup items into a hash table
      ;; with one item per definition. The key is the definition
      ;; position. The value is the set of its uses' positions.
      (define ht-defs/uses (make-hash))
      (with-time/log 'make-ht-defs/uses
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
            [_ #f])))
      ;; Convert the hash table into a list, sorting the usage positions.
      (define defs/uses
        (with-time/log 'defs/uses
          (for/list ([(def uses) (in-hash ht-defs/uses)])
            (match-define (list sym req def-beg def-end) def)
            (list 'def/uses
                  def-beg def-end
                  req sym
                  (sort (set->list uses) < #:key car)))))
      ;; [3] `annotations` = `infos` + `defs/uses`
      (define annotations (sort (append infos defs/uses) < #:key cadr))

      ;;
      ;; Completion candidates (including their defn locs)
      ;;
      (define completions-set (mutable-set))

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
           (set-add! completions-set
                     (substring code-str beg end))]
          [_ (void)]))
      ;; [2] Imports
      (parameterize ([current-namespace ns])
        (with-time/log 'imports
          (imports stx completions-set)))
      (define completions (sort (set->list completions-set)
                                string<=?))

      ;;
      ;; Final answer for Emacs front-end
      ;;
      (list 'check-syntax-ok
            (cons 'completions completions)
            (cons 'annotations annotations)))))

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
  (define (check-file path)
    (time (check-syntax path (file->string path))))
  (check-file (path->string (syntax-source #'here))))

(module+ test
  (require rackunit
           racket/file)
  (define (check-this-file path)
    (check-not-exn
     (λ ()
       (time (void (check-syntax path (file->string path)))))))
  (check-this-file (path->string (syntax-source #'here)))
  ;; Again to exercise and test cache
  (check-this-file (path->string (syntax-source #'here))))

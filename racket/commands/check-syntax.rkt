#lang racket/base

(require racket/format
         (only-in racket/list remove-duplicates)
         racket/match
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

;; Note: Instead of using the `show-content` wrapper, we give already
;; fully expanded syntax directly to `make-traversal`. Why? Expansion
;; can be slow. 1. We need exp stx for other purposes here. Dumb to
;; expand twice. 2. We might not even need to expand once, now:
;; string->expanded-syntax maintains a cache.
;;
;; [One nuance of caching expanded syntax is that we also cache the
;; namespace used while expanding -- that needs to be the
;; current-namespace for things like module->imports. Likewise
;; currently-load-relative-directory. That is why
;; string->expanded-syntax uses a "call-with" "continuation style": it
;; sets parameters when calling the continuation function.]

(define (check-syntax path-str code-str)
  (define path (string->path path-str))
  (parameterize ([error-display-handler our-error-display-handler]
                 [pre-exn-errors '()])
    (with-handlers ([exn:fail? (handle-fail path-str)])
      (with-time/log (~a "total " path-str)
        (string->expanded-syntax path
                                 code-str
                                 (analyze path code-str))))))

(define ((analyze path code-str) stx)
  (define o (new build-trace% [src path]))
  (parameterize ([current-annotations o])
    (define-values (expanded-expression expansion-completed)
      (make-traversal (current-namespace)
                      (current-load-relative-directory)))
    (with-time/log 'drracket/check-syntax/expanded-expression
      (expanded-expression stx))
    (with-time/log 'drracket/check-syntax/expansion-completed
      (expansion-completed)))
  (define synchecks (send o get-trace))

  ;;
  ;; Annotations
  ;;

  ;; Note: We adjust all positions to 1-based Emacs `point' values.

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
            ;; - drracket/check-syntax only reports the file, not the
            ;;   position within. We can find that using our
            ;;   def-in-file.
            ;;
            ;; - drracket/check-syntax uses identifier-binding which
            ;;   isn't smart about contracting and/or renaming
            ;;   provides. As a result, the value of the id here can
            ;;   be wrong. e.g. For "make-traversal" it will report
            ;;   "provide/contract-id-make-traversal.1". It's good to
            ;;   try both ids with def-in-file.
            ;;
            ;; However, calling def-in-file here/now for all jumps
            ;; would be quite slow. Futhermore, a user might not
            ;; actually use the jumps -- maybe not any, probably not
            ;; most, certainly not all.
            ;;
            ;; Sound like a job for a thunk, e.g. racket/promise
            ;; delay/force? We can't marshal a promise between Racket
            ;; back end and Emacs front end. We can do the moral
            ;; equivalent: Simply return the info that the front end
            ;; should give to the "def/drr" command if/as/when needed.
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
                   (log-racket-mode-check-syntax-warning "bad path in ~v" syncheck)
                   #f])]
           [(vector 'syncheck:add-unused-require beg end)
            (item 'unused-require beg end)]
           [_ #f])))))

  ;; [2] Consolidate the add-arrow/name-dup items into a hash table
  ;; with one item per definition. The key is the definition position.
  ;; The value is the set of its uses' positions.
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
  ;; Convert the hash table into a list sorted by use positions.
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
  ;; draw an arrow from something to nothing. There _will_ however be
  ;; a "no bound occurrences" mouseover. Although it's hacky to match
  ;; on a string like that, it's the best way to get _all_ local
  ;; completion candidates. It's the same reason why we go to the work
  ;; in imported-completions to find _everything_ imported, that
  ;; _could_ be used.
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
  (with-time/log 'imports
    (imports stx completions-set))
  (define completions (sort (set->list completions-set)
                            string<=?))

  ;;
  ;; Final answer for Emacs front-end
  ;;
  (list 'check-syntax-ok
        (cons 'completions completions)
        (cons 'annotations annotations)))

(define (remove-dupes-and-falses xs)
  (remove-duplicates (filter values xs)))

;; Typed Racket can report multiple errors. The protcol is it calls
;; error-display-handler for each one. There is a final, actual
;; exn:fail:syntax raised, but it's not useful for us: Although its
;; srclocs correspond the locations, its message is just a summary.
;; Here we collect each message and location in a parameter, and when
;; the final summary exn is raised, we ignore it and use these. Note
;; that Typed Racket is the only such example I'm aware of, but if
;; something else wanted to report multiple errors, and it used a
;; similar approach, we'd handle it here, too.
(define pre-exn-errors (make-parameter '()))
(define (our-error-display-handler msg exn)
  (when (and (exn:fail:syntax? exn)
             (exn:srclocs? exn))
    (pre-exn-errors (append (pre-exn-errors)
                            (exn-srclocs->our-list exn)))))

(define ((handle-fail path) e)
  (cons 'check-syntax-errors
        (cond
          ;; Multiple errors. See comment above.
          [(not (null? (pre-exn-errors)))
           (pre-exn-errors)]
          ;; A single error, with one or more locations from least to
          ;; most specific. This is the intended use of
          ;; exn:fail:syntax -- not multiple errors.
          [(exn:srclocs? e)
           (exn-srclocs->our-list e)]
          ;; A single error with no srcloc at all. Although this might
          ;; happen with arbitrary runtime errors (?), it's unlikely
          ;; with exn:fail:syntax during expansion.
          [else
           (list
            (list 'error path 1 0 (exn-message e)))])))

(define (exn-srclocs->our-list e)
  (for/list ([sl (in-list ((exn:srclocs-accessor e) e))])
    (match sl
      [(srcloc path _ _ ofs span)
       (list 'error (path->string path)
             ofs (+ ofs span)
             (exn-message e))])))

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

;;; logger / timing

(define-logger racket-mode-check-syntax)

(define (time-apply/log what proc args)
  (define-values (vs cpu real gc) (time-apply proc args))
  (define (fmt n) (~v #:align 'right #:min-width 4 n))
  (log-racket-mode-check-syntax-info "~a cpu | ~a real | ~a gc <= ~a"
                                     (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

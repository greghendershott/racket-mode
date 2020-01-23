#lang racket/base

(require racket/format
         racket/list
         racket/match
         racket/path
         racket/promise
         racket/set
         syntax/modread
         racket/class
         drracket/check-syntax
         ;; drracket/check-syntax does not re-provide build-trace%
         (only-in drracket/private/syncheck/traversals
                  build-trace%)
         "../util.rkt")

(provide check-syntax)

(define (check-syntax path-str code-str)
  ;; Note: We adjust all positions to 1-based Emacs `point' values.
  (define path (string->path path-str))
  (define dir (path-only path))
  (define ns (make-base-namespace))
  (define stx (parameterize ([current-namespace ns])
                (define in (open-input-string code-str))
                (port-count-lines! in)
                (with-module-reading-parameterization
                  (λ ()
                    (read-syntax path in)))))
  (with-handlers ([exn:fail? (handle-fail path-str)])
    ;; FIXME: Use syntax cache
    (define expanded-stx (parameterize ([current-load-relative-directory dir]
                                        [current-namespace               ns])
                           (expand stx)))
    ;; Instead of using `show-content`, pass already-expanded stx
    ;; directly to `make-traversal`. We also want to use expanded stx
    ;; for `imported-completions` below, so it would be dumb to
    ;; expand it twice. Furthermore we maintain a cache of expanded
    ;; stx in syntax.rkt, and plan to hook that up here, too.
    (define o (new build-trace% [src path]))
    (parameterize ([current-annotations o])
      (define-values (expanded-expression expansion-completed)
        (make-traversal ns path))
      (parameterize ([current-namespace ns])
        (expanded-expression expanded-stx))
      (expansion-completed))
    (define xs (send o get-trace))

    ;; Most kinds of items we simply transform. Collect those now.
    (define infos
      (remove-duplicates
       (filter
        values
        (for/list ([x (in-list xs)])
          (define (item sym beg end . more)
            (list* sym (add1 beg) (add1 end) more))
          (match x
            [(vector 'syncheck:add-mouse-over-status beg end str)
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
                     sym
                     path
                     submods)
             ;; Note: It would be much too slow to find all
             ;; definitions within files, now. Instead, the front-end
             ;; can supply this information to a new command
             ;; `find-definition-in-file`, if/as/when needed.
             (item 'external-def beg end
                   (symbol->string sym)
                   (path->string path)
                   submods)]
            [_ #f])))))
    ;; Consolidate the add-arrow/name-dup items into a hash table
    ;; with one item per definition. The key is the definition
    ;; position. The value is the set of its uses' positions.
    (define ht-defs/uses (make-hash))
    (for ([x (in-list xs)])
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
    (define annotations (sort (append infos defs/uses) < #:key cadr))

    (define local-completions (for*/set ([x (in-hash-keys ht-defs/uses)]
                                         #:when (eq? 'local (cadr x)))
                                (car x)))
    (define completions
      (parameterize ([current-load-relative-directory dir]
                     [current-namespace               ns])
        (sort (set->list
               (imported-completions expanded-stx local-completions))
              string<=?)))

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

(module+ test
  (require rackunit
           racket/file)
  (define (do path)
    (check-not-exn (λ ()
                     (time (void (check-syntax path (file->string path)))))))
  (do (path->string (syntax-source #'here))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Finding completion candidates from imports

;; drracket/check-syntax tells us about local definitions (which is
;; great!), and, tells us about imported definitions -- but only those
;; already _used_. Obviously, a major use case for completion is
;; typing _new_ uses of available definitions, too. e.g. "What is that
;; string-xxx function I'm not yet using in this file?" So we want to
;; supply that full set.
;;
;; Furthermore, import names can be munged (e.g. `prefix-in` or
;; `rename-in`) so module->exports isn't the quite the right answer.
;;
;; If you have a namespace from module->namespace, you can use
;; namespace-mapped-symbols -- easy! However we do NOT want to
;; instantiate the module, i.e. "run the user's code". We want to
;; supply this information using the same sort of "passive" analaysis
;; done by check-syntax, before the user even runs the file (if ever).
;;
;; AFAICT there is no good way to get completions from all imported
;; identifiers, except attempting to parse the complete #%require
;; grammar including `prefix` and renaming forms like `just-meta`, and
;; apply that information to tweak the answer from module->exports.

;; It is important to run this with the correct parameterization of
;; current-namespace and current-load-relative-directory.
(define (imported-completions stx [sos (set)])
  (syntax-case* stx (module #%module-begin #%plain-module-begin #%require) symbolic-compare?
    [(module _ lang (#%module-begin e ...))
     (handle-module-level #'(e ...) sos #'lang)]
    [(module _ lang (#%plain-module-begin e ...))
     (handle-module-level #'(e ...) sos #'lang)]
    [_ sos]))

(define (handle-module-level es sos lang)
  (for/fold ([sos (module-exported-strings sos lang lang)])
            ([e (in-syntax es)])
    (syntax-case* e (#%require) symbolic-compare?
      [(#%require e ...)
       (for/fold ([sos sos]) ([spec (in-syntax #'(e ...))])
         (handle-raw-require-spec spec sos lang))]
      [_ sos])))

(define (handle-raw-require-spec spec sos lang)
  (syntax-case* spec (for-meta for-syntax for-template for-label just-meta) symbolic-compare?
    [(for-meta _phase specs ...)
     (for/fold ([sos sos]) ([spec (in-syntax #'(specs ...))])
       (handle-phaseless-spec spec sos lang))]
    [(for-syntax specs ...)
     (for/fold ([sos sos]) ([spec (in-syntax #'(specs ...))])
       (handle-phaseless-spec spec sos lang))]
    [(for-template specs ...)
     (for/fold ([sos sos]) ([spec (in-syntax #'(specs ...))])
       (handle-phaseless-spec spec sos lang))]
    [(for-label specs ...)
     (for/fold ([sos sos]) ([spec (in-syntax #'(specs ...))])
       (handle-phaseless-spec spec sos lang))]
    [(just-meta phase specs ...)
     (for/fold ([sos sos]) ([spec (in-syntax #'(specs ...))])
       (handle-raw-require-spec spec sos lang))]
    [raw-module-path
     (handle-phaseless-spec #'raw-module-path sos lang)]))

(define (handle-phaseless-spec spec sos lang)
  (syntax-case* spec (only prefix all-except prefix-all-except rename)
      symbolic-compare?
    [(only raw-module-path id ...)
     (set-union sos
                (syntax->string-set #'(id ...)))]
    [(prefix prefix-id raw-module-path)
     (module-exported-strings sos
                              #'raw-module-path
                              lang
                              #:prefix #'prefix-id)]
    [(all-except raw-module-path id ...)
     (module-exported-strings sos
                              #'raw-module-path
                              lang
                              #:except (syntax->string-set #'(id ...)))]
    [(prefix-all-except prefix-id raw-module-path id ...)
     (module-exported-strings sos
                              #'raw-module-path
                              lang
                              #:prefix #'prefix-id
                              #:except (syntax->string-set #'(id ...)))]
    [(rename raw-module-path local-id exported-id)
     (set-union (set-remove sos
                            (if (eq? (syntax-e #'raw-module-path) (syntax-e lang))
                                (set)
                                (->str #'exported-id)))
                (set (->str #'local-id)))]
    [raw-module-path
     (module-path? (syntax->datum #'raw-module-path))
     (module-exported-strings sos
                              #'raw-module-path
                              lang)]))

(define (->str v)
  (match v
    [(? syntax?) (->str (syntax-e v))]
    [(? symbol?) (symbol->string v)]
    [(? string?) v]))

(define (syntax->string-set s)
  (for/set ([s (in-syntax s)])
    (->str s)))

(define (module-exported-strings sos
                                 raw-module-path
                                 lang
                                 #:except [exceptions (set)]
                                 #:prefix [prefix #'""])
  ;; NOTER: Important to run this with the correct parameterization of
  ;; current-namespace and current-load-relative-directory.
  (define (add-exports mp)
    (define-values (vars stxs) (module->exports mp))
    (define orig (list->set
                  (filter
                   values
                   (flatten
                    (for/list ([xs (in-list (list vars stxs))])
                      (for/list ([phase+vs (in-list xs)])
                        (match phase+vs
                          [(cons 0 vs)
                           (for/list ([v (in-list vs)])
                             (match v [(cons sym _) (->str sym)]))]
                          [_ #f])))))))
    (define prefixed (for/set ([v (in-set orig)])
                       (~a (->str prefix) v)))
    (set-union (if (eq? (syntax-e raw-module-path) (syntax-e lang))
                   ;; {except rename prefix}-in don't remove original
                   (set-union sos orig)
                   ;; {except rename prefix}-in do remove original
                   (set-subtract sos orig exceptions))
               prefixed))
  ;; Ignore non-external module paths: module->exports can't handle
  ;; them, and anyway, drracket/check-syntax will contribute
  ;; completion candidates for local definitions, we don't need to
  ;; find them here.
  (syntax-case* raw-module-path (quote submod) symbolic-compare?
    [(quote _)        sos]
    [(submod "." . _) sos]
    [_                (add-exports (syntax->datum raw-module-path))]))

(define (symbolic-compare? x y)
  (eq? (syntax-e x) (syntax-e y)))

(module+ completions-example
  (parameterize ([current-namespace (make-base-namespace)])
    (define stx
      (expand
       #'(module m racket/base
           (module sub racket/base (void))
           (require racket/require
                    (submod "." sub)
                    (except-in "../error.rkt" show-full-path-in-errors)
                    (prefix-in XXX: (except-in racket/file other-write-bit))
                    (rename-in racket/path [path-only PATH-ONLY])))))
    (syntax->datum stx)
    (imported-completions stx)))

(module+ test
  (require version/utils)
  ;; Compare the results to namespace-mapped-symbols.
  (module mod racket/base
    (module sub racket/base
      (define provided-by-submodule 42)
      (provide provided-by-submodule))
    (require (rename-in racket/path
                        [path-only PATH-ONLY])
             (except-in racket/base println)
             (rename-in racket/base
                        [display DISPLAY])
             (prefix-in PREFIX: (only-in racket/base displayln))
             (for-syntax (rename-in racket/syntax [format-id FORMAT-ID]))
             (submod "." sub))
    (define-namespace-anchor nsa)
    (define nsms (map symbol->string
                      (namespace-mapped-symbols
                       (namespace-anchor->namespace nsa))))
    (provide nsms))
  (require 'mod)
  (define mod/stx
    (expand
     #`(module mod racket/base
         (module sub racket/base
           (define provided-by-submodule 42)
           (provide provided-by-submodule))
         (require (rename-in racket/path
                             [path-only PATH-ONLY])
                  (except-in racket/base println)
                  (rename-in racket/base
                             [display DISPLAY])
                  (prefix-in PREFIX: (only-in racket/base displayln))
                  (for-syntax (rename-in racket/syntax [format-id FORMAT-ID]))
                  (submod "." sub))
         (eprintf "I should not print!"))))
  (let (;; The world according to namespace-mapped-symbols
        [nsms (list->set nsms)]
        ;; The world according to our imported-completions
        [cs (parameterize ([current-namespace (make-base-namespace)])
              (imported-completions (expand mod/stx)))])
    ;; Test {prefix rename except}-in, keeping mind that they work
    ;; differently for requires that modify the module language
    ;; imports.
    (check-false (set-member? cs "path-only")
                 "rename-in not from module language hides old name")
    (check-true (set-member? cs "PATH-ONLY")
                "rename-in not from module language has new name ")
    (check-true (set-member? cs "display")
                "rename-in from module language does not hide old name")
    (check-true (set-member? cs "DISPLAY")
                "rename-in from module language has new name")
    (check-true (set-member? cs "displayln")
                "prefix-in from module language does not hide old name")
    (check-true (set-member? cs "PREFIX:displayln")
                "prefix-in from module language is available under new name")
    ;; namespace-mapped-symbols will return some definitions beyond
    ;; those imported -- it includes {top module}-level bindings. This
    ;; test accounts for that with a dumb ad hoc list. (More nifty
    ;; would be to walk our test stx and build that list.)
    ;;
    ;; FIXME? Travis CI says this test fails prior to Racket 7.0:
    ;; namespace-mapped-symbols reports ~400 more symbols --
    ;; apparently from full racket (should be racket/base). Huh??
    ;; Well, _our_ results are correct. For now, let's just do the
    ;; test on Racket 7.0+.
    (when (version<=? "7.0" (version))
      (check-equal? (set-subtract nsms cs)
                    (set "tmp.1" "nsms" "nsa" "provided-by-submodule")
                    "namespace-mapped-symbols returns only a few more, non-imported definitions"))))

(module+ slow-test
  ;; Exercise our parsing of the #%require grammar: Try doing
  ;; (check-not-exn (imported-completions)) on many files in the
  ;; Racket distribution. Grammar mistakes will raise syntax failures.
  (require rackunit
           syntax/modresolve
           racket/path
           "../syntax.rkt")
  (define (check path)
    (define ns (make-base-namespace))
    (define stx (file->expanded-syntax path #:namespace ns))
    (parameterize ([current-load-relative-directory (path-only path)]
                   [current-namespace               ns])
      (check-not-exn (λ () (imported-completions stx))
                     (format "#%require grammar handles ~v" path))))
  (for ([roots (in-list '(("racket.rkt" "typed")
                          ("core.rkt" "typed-racket")
                          ("main.rkt" "racket")))])
    (for* ([v (in-directory
               (path-only
                (apply collection-file-path roots)))]
           #:when (equal? #"rkt" (filename-extension v)))
      (println v)
      (check v))))

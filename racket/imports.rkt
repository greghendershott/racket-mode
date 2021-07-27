#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/set
         "util.rkt")

(provide imports)

;;; Finding completion candidates from imports

;; drracket/check-syntax tells us about local definitions (which is
;; great!), and, tells us about imported definitions -- but only those
;; already _used_. Obviously, a major use case for completion is
;; typing _new_ uses of available definitions, too. e.g. "What is that
;; string-xxx function I'm not yet using in this file?" So we want to
;; supply that full set.
;;
;; If you have a namespace from module->namespace, you can use
;; namespace-mapped-symbols -- easy! However we do NOT want to
;; instantiate the module, i.e. "run the user's code". We want to
;; supply this information using the same sort of "passive" analaysis
;; done by check-syntax, before the user even runs the file (if ever).
;;
;; module->exports is a good starting point, but not the whole answer:
;; Imports can be filtered and renamed -- e.g. only-in, except-in,
;; prefix-in, rename-in.
;;
;; AFAICT there is no good way to get completions from all imported
;; identifiers, except attempting to parse the complete #%require
;; grammar including `prefix` and renaming forms like `just-meta`, and
;; apply that information to tweak the answer from module->exports.

;; It is important to run this with the correct parameterization of
;; current-namespace and current-load-relative-directory.
(define/contract (imports stx [sos (mutable-set)])
  (->* (syntax?) (set-mutable?) set-mutable?)

  (define (handle-module stx)
    (syntax-case stx (module #%module-begin #%plain-module-begin #%require)
      [(module _id lang (#%module-begin e ...))
       (handle-module-level #'(e ...) #'lang)]
      [(module _id lang (#%plain-module-begin e ...))
       (handle-module-level #'(e ...) #'lang)]))

  (define (handle-module-level es lang)
    (module-exported-strings lang lang)
    (for ([e (in-syntax es)])
      (syntax-case* e (#%require module module*) symbolic-compare?
        [(#%require e ...)
         (for ([spec (in-syntax #'(e ...))])
           (handle-raw-require-spec spec lang))]
        [(module _id sub-mod-lang (_mb e ...))
         (handle-module-level #'(e ...) #'sub-mod-lang)]
        [(module* _id sub-mod-lang (_mb e ...))
         (handle-module-level #'(e ...) (if (syntax-e #'sub-mod-lang)
                                            #'sub-mod-lang
                                            lang))]
        [ _ (void)])))

  (define (handle-raw-require-spec spec lang)
    (let loop ([spec spec])
      (syntax-case* spec
          (for-meta for-syntax for-template for-label just-meta for-space just-space)
          symbolic-compare?
        [(for-meta _phase specs ...)
         (for ([spec (in-syntax #'(specs ...))])
           (loop spec))]
        [(for-syntax specs ...)
         (for ([spec (in-syntax #'(specs ...))])
           (loop spec))]
        [(for-template specs ...)
         (for ([spec (in-syntax #'(specs ...))])
           (loop spec))]
        [(for-label specs ...)
         (for ([spec (in-syntax #'(specs ...))])
           (loop spec))]
        [(just-meta _phase specs ...)
         (for ([spec (in-syntax #'(specs ...))])
           (loop spec))]
        [(for-space _space specs ...)
         (for ([spec (in-syntax #'(specs ...))])
           (loop spec))]
        [(just-space _space specs ...)
         (for ([spec (in-syntax #'(specs ...))])
           (loop spec))]
        [raw-module-path
         (handle-phaseless-spec #'raw-module-path lang)])))

  (define (handle-phaseless-spec spec lang)
    (syntax-case* spec (only prefix all-except prefix-all-except rename)
        symbolic-compare?
      [(only _raw-module-path id ...)
       (set-union! sos
                   (syntax->string-set #'(id ...)))]
      [(prefix prefix-id raw-module-path)
       (module-exported-strings #'raw-module-path
                                lang
                                #:prefix #'prefix-id)]
      [(all-except raw-module-path id ...)
       (module-exported-strings #'raw-module-path
                                lang
                                #:except (syntax->string-set #'(id ...)))]
      [(prefix-all-except prefix-id raw-module-path id ...)
       (module-exported-strings #'raw-module-path
                                lang
                                #:prefix #'prefix-id
                                #:except (syntax->string-set #'(id ...)))]
      [(rename raw-module-path local-id exported-id)
       (begin
         (unless (eq? (syntax-e #'raw-module-path) (syntax-e lang))
           (set-remove! sos (->str #'exported-id)))
         (set-add! sos (->str #'local-id)))]
      [raw-module-path
       (module-path? (syntax->datum #'raw-module-path))
       (module-exported-strings #'raw-module-path
                                lang)]))

  (define (module-exported-strings raw-module-path
                                   lang
                                   #:except [exceptions (set)]
                                   #:prefix [prefix #'""])
    ;; NOTE: Important to run module->exports with the correct
    ;; parameterization of current-namespace and
    ;; current-load-relative-directory.
    ;;
    ;; Ignore module paths module->exports can't handle, including
    ;; paths like 'foo or (submod "." _) or (submod ".." _). We get
    ;; completion candidates from drracket/check-syntax for
    ;; non-imported bindings. Our contribution is imported
    ;; definitions.
    (with-handlers ([exn:fail? (λ _ sos)])
      (define-values (vars stxs)
        (module->exports (syntax->datum raw-module-path)))
      (define orig
        (for*/mutable-set ([vars+stxs (in-list (list vars stxs))]
                           [phases    (in-list vars+stxs)]
                           [export    (in-list (cdr phases))])
          (->str (car export))))
      ;; If imports are from the module language, then {except rename
      ;; prefix}-in do NOT remove imports under the original name.
      ;; Otherwise they do.
      (if (eq? (syntax-e raw-module-path) (syntax-e lang))
          (set-union! sos orig)
          (set-subtract! sos orig exceptions))
      (for ([v (in-set orig)])
        (set-add! sos (~a (->str prefix) v)))))

  (handle-module stx)
  sos)

(define (->str v)
  (match v
    [(? syntax?) (->str (syntax-e v))]
    [(? symbol?) (symbol->string v)]
    [(? string?) v]))

(define (syntax->string-set s)
  (for/mutable-set ([s (in-syntax s)])
    (->str s)))

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
    (imports stx)))

(module+ test
  (require rackunit
           version/utils)
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
  (let (;; The world according to `namespace-mapped-symbols`
        [nsms (list->set nsms)]
        ;; The world according to our `imports`
        [cs (parameterize ([current-namespace (make-base-namespace)])
              (define stx (expand mod/stx))
              (time (imports stx)))])
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
                    "namespace-mapped-symbols returns only a few more, non-imported definitions")))
  ;; Issue 481
  (check-not-exn (λ () (imports
                        (expand
                         #`(module mod racket/base
                             (module sub1 racket/base)
                             (module sub2 racket/base
                               (require (submod ".." sub1))))))))
  )

(module+ slow-test
  ;; Exercise our parsing of the #%require grammar: Try doing
  ;; (check-not-exn (imports stx)) on many files in the Racket
  ;; distribution. Grammar mistakes will raise exn:fail:syntax.
  (require rackunit
           racket/path
           "syntax.rkt")
  (define (check path)
    (parameterize ([current-load-relative-directory (path-only path)]
                   [current-namespace               (make-base-namespace)])
      (file->expanded-syntax
       path
       (λ (stx)
         (check-not-exn (λ () (imports stx))
                        (format "#%require grammar handles ~v" path))))))
  (for* ([roots (in-list '(("racket.rkt" "typed")
                           ("core.rkt" "typed-racket")
                           ("main.rkt" "racket")))]
         [path  (in-directory
                 (path-only
                  (apply collection-file-path roots)))]
         #:when (equal? #"rkt" (filename-extension path)))
    (println path)
    (check path)))

#lang racket/base

(require racket/contract
         (only-in racket/function curry)
         racket/list
         racket/match
         "identifier.rkt"
         "syntax.rkt")

(provide find-definition
         find-definition/drracket-jump
         find-signature)

(module+ test
  (require rackunit
           racket/format))

;; Note: Unfortunately identifier-binding can't report the definition
;; id in the case of a contract-out and a rename-out, both. For
;; `(provide (contract-out [rename orig new contract]))`
;; identifier-binding reports (1) the contract wrapper as the id, and
;; (2) `new` as the nominal-id -- but NOT (3) `orig`. We handle such
;; cases; see `find-def-in-file` and its use of `$renaming-provde`,
;; below.
;;
;; Another tricky case: "foo" is defined in def.rkt. repro.rkt
;; requires def.rkt and re-provides "foo" using contract-out. When
;; user.rkt requires repro.rkt, identifier-binding will report "foo"
;; the id (yay!) but report the defining file is repro.rkt -- not
;; def.rkt (boo!). We handle such cases.

(define location/c (list/c path-string? natural-number/c natural-number/c))

;; Try to find a definition, using as a head start information
;; supplied by drracket/check-syntax. It did the "fast" work for all
;; uses (calling identifier-binding) and we recorded that answer to
;; give the front end. If the user wants to visit any of those, the
;; front end gives us that info, and we do the "slow" work.
(define/contract (find-definition/drracket-jump how-path src-path submods id-strs)
  (-> (and/c how/c (not/c 'namespace)) path-string? (listof symbol?) (listof string?)
      (or/c #f 'kernel location/c))
  (or (for/or ([id-str (in-list id-strs)])
        (match (find-def-in-file (string->symbol id-str) how-path src-path submods)
          [(list stx path _submods)
           (list (->path-string (or (syntax-source stx) path))
                 (or (syntax-line stx) 1)
                 (or (syntax-column stx) 0))]
          [v v]))
      ;; Handle possible re-provide with a contract: Try again
      ;; starting with that other src-path. i.e. Do automatically what
      ;; the user could: Open that file, and try visit-definition
      ;; again, there. from that.
      (and (not (path-string-equal? how-path src-path))
           (for/or ([id-str (in-list id-strs)])
             (find-definition src-path id-str)))
      ;; As a final fallback, return the reported file:1:0. At least
      ;; give user a head start.
      (list src-path 1 0)))

;; Try to find a definition.
(define/contract (find-definition how str)
  (-> how/c string?
      (or/c #f 'kernel location/c))
  (match (find-def how str)
    [(list stx path _submods)
     (list (->path-string (or (syntax-source stx) path))
           (or (syntax-line stx) 1)
           (or (syntax-column stx) 0))]
    [v v]))

;; Try to find the definition of `str`, returning its signature or #f.
;; When defined in 'kernel, returns a form saying so, not #f.
(define/contract (find-signature how str)
  (-> how/c string?
      (or/c #f pair?))
  (match (find-def how str)
    ['kernel '("defined in #%kernel, signature unavailable")]
    [(list id-stx path submods)
     (get-syntax how path
                 (λ (mod-stx)
                   (match ($signature (syntax-e id-stx)
                                      (submodule-syntax submods mod-stx))
                     [(? syntax? stx) (syntax->datum stx)]
                     [_ #f])))]
    [v v]))

(define stx+path+mods/c (list/c syntax? path-string? (listof symbol?)))

(define/contract (find-def how str)
  (-> how/c string?
      (or/c #f 'kernel stx+path+mods/c))
  (->identifier-resolved-binding-info
   how str
   (λ (results)
     (match results
       [(? list? bindings)
        (or (for/or ([x (in-list (remove-duplicates bindings))])
              (match x
                [(cons _id 'kernel) 'kernel]
                [(list* id path submods) (find-def-in-file id how path submods)]))
            ;; Handle possible re-provide with a contract: Try again
            ;; starting with that other src-path. i.e. Automatically
            ;; do what the user could: Open that file, and try
            ;; visit-definition again, there.
            (match results
              [(list (list* src-id src-path src-subs)
                     (list* nom-id _))
               (or (and (or (equal? how 'namespace)
                            (not (path-string-equal? how src-path)))
                        (for/or ([id (in-list (list src-id nom-id))])
                          (find-def (path->string src-path) (symbol->string id))))
                   ;; As a final fallback, return the reported
                   ;; file:1:0. At least give user a head start.
                   (list (datum->syntax #f src-id (list src-path 1 0 #f #f))
                         src-path
                         '()))]
              [_ #f]))]
       [_ #f]))))

(define/contract (find-def-in-file id-sym how path submods)
  (-> symbol? how/c path-string? (listof symbol?)
      (or/c #f stx+path+mods/c))
  (define subs (curry submodule-syntax submods))
  (match (or (get-expanded-syntax
              how path
              (λ (stx)
                ($definition id-sym (subs stx))))
             (get-syntax
              how path
              (λ (stx)
                (match ($renaming-provide id-sym (subs stx))
                  [(? identifier? id)
                   (define id-sym (syntax-e id))
                   (get-expanded-syntax
                    how path
                    (λ (stx)
                      ($definition id-sym (subs stx))))]
                  [_ #f]))))
    [(? syntax? stx) (list stx path submods)]
    [_  #f]))

;; Given a submodule path as a list of symbols, and the syntax for a
;; file's entire module form: Return the (sub)module contents as
;; #'(begin . contents).
(define/contract (submodule-syntax sub-mod-syms stx)
  (-> (listof symbol?) syntax? (or/c #f syntax?))
  ;; Prepend #f as the outermost module name to match, meaning "any".
  (sub-stx (cons #f sub-mod-syms) stx))

(define (sub-stx mods stx)
  (match-define (cons this more) mods)
  (define (subs stxs)
    (if (empty? more)
        #`(begin . #,stxs)
         (ormap (λ (stx) (sub-stx more stx))
                (syntax->list stxs))))
  (syntax-case* stx (module #%module-begin) syntax-e-eq?
    [(module name _ (#%module-begin . stxs))
     (or (not this) (eq? this (syntax-e #'name)))
     (subs #'stxs)]
    [(module name _ . stxs)
     (or (not this) (eq? this (syntax-e #'name)))
     (subs #'stxs)]
    [_ #f]))

(module+ test
  (check-equal? (syntax->datum
                 (submodule-syntax '(a b c)
                                   #'(module file racket
                                       (module a racket
                                         (module not-b racket #f)
                                         (module b racket
                                           (module not-c racket #f)
                                           (module c racket "bingo")
                                           (module not-c racket #f))
                                         (module not-b racket #f)))))
                '(begin "bingo")))

;; Given a symbol and syntax, return syntax corresponding to the
;; definition. Intentionally does NOT walk into module forms, so, give
;; us the module bodies wrapped in begin.
;;
;; If `stx` is expanded we can find things defined via definer
;; macros.
;;
;; If `stx` is not expanded, we will miss some things, however the
;; syntax will be closer to what a human expects -- e.g. `(define (f
;; x) x)` instead of `(define-values (f) (lambda (x) x))`.
(define ($definition sym stx) ;;symbol? syntax? -> syntax?
  (define eq-sym? (make-eq-sym? sym))
  ;; This is a hack to handle definer macros that neglect to set
  ;; srcloc properly using syntax/loc or (format-id ___ #:source __):
  ;; If the stx lacks srcloc and its parent stx has srcloc, return the
  ;; parent stx instead. Caveats: 1. Assumes caller only cares about
  ;; the srcloc. 2. We only check immediate parent. 3. We only use
  ;; this for define-values and define-syntaxes, below, on the
  ;; assumption that this only matters for fully-expanded syntax.
  (define (loc s)
    (if (and (not (syntax-line s))
             (syntax-line stx))
        stx
        s))
  (syntax-case* stx
      (begin define-values define-syntaxes
             define define/contract
             define-syntax struct define-struct)
      syntax-e-eq?
    [(begin . stxs)                 (ormap (λ (stx) ($definition sym stx))
                                           (syntax->list #'stxs))]
    [(define          (s . _) . _)  (eq-sym? #'s) stx]
    [(define/contract (s . _) . _)  (eq-sym? #'s) stx]
    [(define s . _)                 (eq-sym? #'s) stx]
    [(define-values (ss ...) . _)   (ormap eq-sym? (syntax->list #'(ss ...)))
                                    (loc (ormap eq-sym? (syntax->list #'(ss ...))))]
    [(define-syntax (s .  _) . _)   (eq-sym? #'s) stx]
    [(define-syntax s . _)          (eq-sym? #'s) stx]
    [(define-syntaxes (ss ...) . _) (ormap eq-sym? (syntax->list #'(ss ...)))
                                    (loc (ormap eq-sym? (syntax->list #'(ss ...))))]
    [(define-struct s . _)          (eq-sym? #'s) stx]
    [(define-struct (s _) . _)      (eq-sym? #'s) stx]
    [(struct s . _)                 (eq-sym? #'s) stx]
    [(struct (s _) . _)             (eq-sym? #'s) stx]
    [_                              #f]))

;; Given a symbol and syntax, return syntax corresponding to the
;; function definition signature. The input syntax should NOT be
;; `expand`ed. This intentionally does NOT walk into module forms, so,
;; give us the module bodies wrapped in begin.
(define ($signature sym stx) ;;symbol? syntax? -> (or/c #f list?)
  (define eq-sym? (make-eq-sym? sym))
  (syntax-case* stx (begin define define/contract case-lambda) syntax-e-eq?
    [(begin . stxs)                               (ormap (λ (stx) ($signature sym stx))
                                                         (syntax->list #'stxs))]
    [(define          (s . as) . _)               (eq-sym? #'s) #'(s . as)]
    [(define/contract (s . as) . _)               (eq-sym? #'s) #'(s . as)]
    [(define s (case-lambda [(ass ...) . _] ...)) (eq-sym? #'s) #'((s ass ...) ...)]
    [_                                            #f]))

;; Find sym in a contracting and/or renaming provide, and return the
;; syntax for the ORIGINAL identifier (before being contracted and/or
;; renamed). The input syntax should NOT be expanded.
(define ($renaming-provide sym stx) ;;symbol? syntax? -> syntax?
  (define eq-sym? (make-eq-sym? sym))
  (syntax-case* stx (begin provide provide/contract) syntax-e-eq?
    [(begin . stxs)
     (ormap (λ (stx) ($renaming-provide sym stx))
            (syntax->list #'stxs))]
    [(provide/contract . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case stx ()
         [(s _) (eq-sym? #'s)]
         [_     #f]))]
    [(provide . stxs)
     (for/or ([stx (syntax->list #'stxs)])
       (syntax-case* stx (contract-out rename-out) syntax-e-eq?
         [(contract-out . stxs)
          (for/or ([stx (syntax->list #'stxs)])
            (syntax-case* stx (rename) syntax-e-eq?
              [(rename orig s _) (eq-sym? #'s) #'orig]
              [(s _)             (eq-sym? #'s) #'s]
              [_                 #f]))]
         [(rename-out . stxs)
          (for/or ([stx (syntax->list #'stxs)])
            (syntax-case* stx () syntax-e-eq?
              [(orig s) (eq-sym? #'s) #'orig]
              [_        #f]))]
         [_ #f]))]
    [_ #f]))

(module+ test
  ;; Just a quick smoke test. See test/find.rkt for many more tests.
  ;;
  ;; Exercise where the "how" is a path-string, meaning look up that
  ;; path from our cache, not on disk.
  (let ([path-str "/tmp/x.rkt"]
        [code-str (~a `(module x racket/base
                        (define (module-function-binding x y z) (+ 1 x))
                        (define module-variable-binding 42)))])
    (string->expanded-syntax path-str code-str void)
    (check-equal? (find-signature path-str "module-function-binding")
                  '(module-function-binding x y z))
    (check-equal? (find-definition path-str "module-function-binding")
                  `(,path-str 1 31))
    (check-equal? (find-definition path-str "module-variable-binding")
                  `(,path-str 1 79)))
  ;; Exercise the "make-traversal" scenario described in comments
  ;; above.
  (let ([path-str "/tmp/x.rkt"]
        [code-str (~a `(module x racket/base
                        (require drracket/check-syntax)
                        "make-traversal"))])
    (string->expanded-syntax path-str code-str void)
    (check-match (find-definition path-str "make-traversal")
                 (list (pregexp "private/syncheck/traversals.rkt$") _ _))))

;; These `get-syntax` and `get-expanded-syntax` functions handle where
;; we get the syntax.
;;
;; The special case is when `how` is a path-string. That path doesn't
;; necessarily exist as a file, or the file may be outdated. The path
;; may simply be the syntax-source for a string from an unsaved Emacs
;; buffer. So when we need to get syntax for such a path, we need to
;; get it from our cache -- NOT from a file. (How it got in the cache
;; previously was from some check-syntax.)
;;
;; Things like identifier-binding may tell us to look at such a path,
;; or at a path for a real existing/updated file. This helps sort out
;; the various cases.

(define (get-syntax how path-str k)
  (match how
    ['namespace                (file->syntax path-str k)]
    [(? path-string? how-path) (if (path-string-equal? path-str how-path)
                                   (path->existing-syntax path-str k)
                                   (file->syntax          path-str k))]))

;; For when we use syntax-case* simply for syntax-e equality.
(define (syntax-e-eq? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define ((make-eq-sym? sym) stx)
  (and (eq? sym (syntax-e stx)) stx))

(define (get-expanded-syntax how path-str k)
  (match how
    ['namespace                (file->expanded-syntax path-str k)]
    [(? path-string? how-path) (if (path-string-equal? path-str how-path)
                                   (path->existing-expanded-syntax path-str k)
                                   (file->expanded-syntax          path-str k))]))

(define (path-string-equal? a b)
  (equal? (->path-string a)
          (->path-string b)))

(define (->path-string v)
  (cond [(path? v)        (path->string v)]
        [(path-string? v) v]
        [else             (error 'path-string-equal? "not a path or path-string?" v)]))

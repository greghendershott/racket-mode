#lang racket/base

(require racket/contract
         racket/list
         racket/match
         "identifier.rkt"
         "syntax.rkt")

(provide find-definition
         find-definition/drracket-jump
         find-signature)

;; Note: Unfortunately identifier-binding can't report the definition
;; id in the case of a contract-out and a rename-out, both. For
;; `(provide (contract-out [rename orig new contract]))`
;; identifier-binding reports (1) the contract-wrapper as the id, and
;; (2) `new` as the nominal-id -- but NOT (3) `orig`. So we try other
;; strategies here.

(define location/c (list/c path-string? natural-number/c natural-number/c))

;; Try to find a definition.
(define/contract (find-definition how str)
  (-> how/c string?
      (or/c #f 'kernel location/c))
  (match (def how str)
    [(list stx path _submods)
     (list (->path-string (or (syntax-source stx) path))
           (or (syntax-line stx) 1)
           (or (syntax-column stx) 0))]
    [v v]))

;; Likewise, but using information already supplied by
;; drracket/check-syntax, i.e. this is how to "force" that "delay".
;; (We could have done this earlier and returned this information as
;; part of the original check-syntax result -- but doing so would have
;; been too slow.)
(define/contract (find-definition/drracket-jump how path submods id-strs)
  (-> (and/c how/c (not/c 'namespace)) path-string? (listof symbol?) (listof string?)
      (or/c #f 'kernel location/c))
  (for/or ([id-str (in-list id-strs)])
    (match (def-in-file (string->symbol id-str) how path submods)
      [(list stx path _submods)
       (list (->path-string (or (syntax-source stx) path))
             (or (syntax-line stx) 1)
             (or (syntax-column stx) 0))]
      [v v])))

;; Try to find the definition of `str`, returning its signature or #f.
;; When defined in 'kernel, returns a form saying so, not #f.
(define/contract (find-signature how str)
  (-> how/c string?
      (or/c #f pair?))
  (match (def how str)
    ['kernel '("defined in #%kernel, signature unavailable")]
    [(list id-stx path submods)
     (get-syntax how path
                 (λ (mod-stx)
                   (define sub-stx (submodule path submods mod-stx))
                   (match ($signature (syntax-e id-stx) sub-stx)
                     [(? syntax? stx) (syntax->datum stx)]
                     [_ #f])))]
    [v v]))

(define stx+path+mods/c (list/c syntax? path-string? (listof symbol?)))

(define/contract (def how str)
  (-> how/c string?
      (or/c #f 'kernel stx+path+mods/c))
  (->identifier-resolved-binding-info
   how str
   (λ (results)
     (match results
       [(? list? bindings)
        (for/or ([x (in-list (remove-duplicates bindings))])
          (match x
            [(cons _id 'kernel) 'kernel]
            [(list* id path submods) (def-in-file id how path submods)]))]
       [_ #f]))))

(define/contract (def-in-file id how path submods)
  (-> symbol? how/c path-string? (listof symbol?)
      (or/c #f stx+path+mods/c))
  (define (sub-stx stx)
    (submodule path submods stx))
  (match (or (get-expanded-syntax
              how path
              (λ (stx)
                ($definition id (sub-stx stx))))
             (get-syntax
              how path
              (λ (stx)
                (match ($renaming-provide id (sub-stx stx))
                  [(? syntax? s)
                   (get-expanded-syntax
                    how path
                    (λ (stx)
                      ($definition (syntax-e s) (sub-stx stx))))]
                  [_ #f]))))
    [(? syntax? stx) (list stx path submods)]
    [_  #f]))

;; For use with syntax-case*. When we use syntax-case for syntax-e equality.
(define (syntax-e-eq? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define ((make-eq-sym? sym) stx)
  (and (eq? sym (syntax-e stx)) stx))

(define (file-module path)
  (match (path->string (last (explode-path path)))
    [(pregexp "(.+?)\\.rkt$" (list _ v)) (string->symbol v)]))

;; Return bodies (wrapped in begin) of the module indicated by
;; file and sub-mod-syms.
(define (submodule path sub-mod-syms stx)
  (submodule* (cons (file-module path) sub-mod-syms) stx))

(define (submodule* mods stx)
  (match-define (cons this more) mods)
  (define (subs stxs)
    (if (empty? more)
        #`(begin . #,stxs)
         (ormap (λ (stx) (submodule* more stx))
                (syntax->list stxs))))
  (syntax-case* stx (module #%module-begin) syntax-e-eq?
    [(module name _ (#%module-begin . stxs))
     (eq? this (syntax-e #'name))
     (subs #'stxs)]
    [(module name _ . stxs)
     (eq? this (syntax-e #'name))
     (subs #'stxs)]
    [_ #f]))

(module+ test
  (require rackunit)
  (check-equal? (syntax->datum
                 (submodule "/path/to/file.rkt" '(a b c)
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
  ;; Note: Many more tests in test/find.rkt.
  ;;
  ;; Exercise where the "how" is a path-string, meaning look up that path from our cache, not on disk.
  (require racket/format
           version/utils)
  (when (version<=? "6.5" (version))
    (define path-str "/tmp/x.rkt")
    (define code-str (~a `(module x racket/base
                           (define (module-function-binding x y z) (+ 1 x))
                           (define module-variable-binding 42))))
    (string->expanded-syntax path-str code-str void)
    (check-equal? (find-signature path-str "module-function-binding")
                  '(module-function-binding x y z))
    (check-equal? (find-definition "/tmp/x.rkt" "module-function-binding")
                  `(,path-str 1 31))
    (check-equal? (find-definition "/tmp/x.rkt" "module-variable-binding")
                  `(,path-str 1 79))))

;; These `get-syntax` and `get-expanded-syntax` functions handle where
;; we get the syntax.
;;
;; The special case is when `how` is a path-string. That path doesn't
;; necessarily exist as a file, or the file may be outdated. The path
;; may simply be the syntax-source for a string we read and expanded,
;; e.g. from an unsaved Emacs buffer. So when we need to get syntax
;; for such a path, we need to get it from our cache, from a file.
;; (How it got in the cache previously was from some check-syntax.)
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

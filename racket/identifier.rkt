;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/format
         racket/match
         "syntax.rkt")

(provide how/c
         ->identifier
         ->identifier-resolved-binding-info)

;;; Creating identifiers from symbols or strings

;; A simplifying helper for commands that want to work both ways, and
;; accept a first "how" or "context" argument that is either
;; 'namespace or a path-string.
(define how/c (or/c 'namespace path-string?))

(define/contract (->identifier how v k)
  (-> how/c (or/c symbol? string?) (-> syntax? any) any)
  (match how
    ['namespace                       (->identifier/namespace   v k)]
    [(? (and string? path-string?) p) (->identifier/expansion p v k)]))

(define/contract (->identifier/namespace v k)
  (-> (or/c symbol? string?) (-> identifier? any/c) any/c)
  (define sym->id namespace-symbol->identifier)
  (k (cond [(string? v) (sym->id (string->symbol v))]
           [(symbol? v) (sym->id v)])))

;; We use path-str to get expanded module syntax from the cache via
;; path->existing-expanded-syntax, and use the 'module-body-context
;; syntax property -- starting in Racket 6.5 -- which can be used as
;; lexical context to make an identifier. This lets identifier-binding
;; work for identifiers as if they were in that body's lexical context
;; -- including imported identifiers that aren't actually used as
;; bindings in the module body.
(define/contract (->identifier/expansion path-str v k)
  (-> path-string?
      (or/c symbol? string?)
      (-> identifier? any/c)
      any/c)
  (path->existing-expanded-syntax
   path-str
   (λ (stx)
     (define (sym->id v)
       (expanded-module+symbol->identifier path-str stx v))
     (k (cond [(string? v) (sym->id (string->symbol v))]
              [(symbol? v) (sym->id v)])))))

(define/contract (expanded-module+symbol->identifier path-str exp-mod-stx sym)
  (-> path-string? syntax? symbol? identifier?)
  ;; For imported bindings, this creates syntax where
  ;; identifier-binding will report a module-path-index that can be
  ;; resolved to a path that exists. Great!
  ;;
  ;; For module bindings, identifier-binding will say that the binding
  ;; exists. Good! But. Until a module declaration is evaluated, the
  ;; module has no name. As a result, the module-path-index is
  ;; reported as #<module-path-index='|expanded module|>. That would
  ;; resolve to <path:"/path/to/expanded module.rkt"> -- wrong.
  ;;
  ;; Work-around: Let's record the path in the identifier's
  ;; syntax-source. Doing so won't change what identifier-binding
  ;; reports, but it means mpi->path can handle such a module path
  ;; index by instead using the path from syntax-source.
  (datum->syntax (syntax-property exp-mod-stx 'module-body-context)
                 sym
                 (list (string->path path-str) #f #f #f #f)))


;;; Massaging values returned by identifier-binding

 ;; A composition that does the right thing, including when making an
;; identifier that is a module binding.
(define (->identifier-resolved-binding-info how v k)
  (->identifier how v
                (λ (id)
                  (k (resolve-identifier-binding-info
                      id
                      (identifier-binding id))))))

;; Given an identifier and the result from identifier-binding, returns
;; a subset of the information, where the module path indexes are
;; resolved to actual paths, and where the 'lexical value is treated
;; as #f.
(define/contract (resolve-identifier-binding-info id binding-info)
  (-> identifier?
      (or/c 'lexical
            #f
            (list/c module-path-index?
                    symbol?
                    module-path-index?
                    symbol?
                    exact-nonnegative-integer?
                    (or/c exact-integer? #f)
                    (or/c exact-integer? #f))
            (list/c symbol?))
      (or/c #f
             (listof (cons/c symbol?
                             (or/c 'kernel
                                   (cons/c path-string? (listof symbol?)))))))
  (match binding-info
     [(list source-mpi         source-id
            nominal-source-mpi nominal-source-id
            source-phase
            import-phase
            nominal-export-phase)
      (list (cons source-id         (id+mpi->path id source-mpi))
            (cons nominal-source-id (id+mpi->path id nominal-source-mpi)))]
     [_ #f]))

(define/contract (id+mpi->path id mpi)
  (-> identifier?
      module-path-index?
      (or/c 'kernel
            (cons/c path-string? (listof symbol?))))
  (cond [;; We could check below for the interned -- or not in older
         ;; Rackets -- symbol '|expanded module|. That seems smelly.
         ;; Instead if we're a "self" module, and if the identifier
         ;; has a location -- probably supplied above by our
         ;; expanded-module+symbol->identifier -- use that source.
         (and (self-module? mpi)
              (syntax-source id))
         (list (syntax-source id))]
        [else
         (match (resolved-module-path-name
                 (module-path-index-resolve mpi))
           [(? hash-percent-symbol) 'kernel]
           [(? path-string? path)   (list path)]
           [(? symbol? sym)
            (list (build-path (current-load-relative-directory)
                              (~a sym ".rkt")))]
           [(list (? path-string? path) (? symbol? subs) ...)
            (list* path subs)]
           ;; I've seen this odd case occur only when running
           ;; test/find.rkt. The module path index is
           ;; #<module-path-index:(submod "." m) + '|expanded
           ;; module|>, and resolving that is (find-examples m) when
           ;; it should be '(#</path/to/find-example.rkt> m).
           [(list (? symbol?) (? symbol? subs) ...)
            (list* (syntax-source id) subs)])]))

(define (self-module? mpi)
  (define-values (a b) (module-path-index-split mpi))
  (and (not a) (not b)))

(define (hash-percent-symbol v)
  (and (symbol? v)
       (regexp-match? #px"^#%" (symbol->string v))))

(module+ test
  (require rackunit
           "syntax.rkt")
  ;; Check something that is in the namespace resulting from
  ;; module->namespace on, say, this source file.
  (parameterize ([current-namespace (module->namespace (syntax-source #'here))])
    (check-not-false (->identifier-resolved-binding-info 'namespace 'match values))
    (check-not-false (->identifier-resolved-binding-info 'namespace "match" values)))

  ;; Check something that is not in the current namespace, but is an
  ;; identifier in the lexical context of an expanded module form --
  ;; including imported identifiers -- from the expanded syntax
  ;; cache.
  (define top (case (system-type) [(windows) "C:\\"] [(unix macosx) "/"]))
  (define path-str (path->string (build-path top "path" "to" "foobar.rkt")))
  (define code-str (~a '(module foobar racket/base
                         (require net/url racket/set)
                         (let ([a-lexical-binding 42])
                          a-lexical-binding)
                         (define a-module-binding 42)
                         a-module-binding)))
  ;; Get the expanded syntax in our cache
  (string->expanded-syntax path-str code-str void)
  ;; Simple imported binding
  (check-not-false (->identifier-resolved-binding-info path-str 'set? values))
  (check-not-false (->identifier-resolved-binding-info path-str "set?" values))
  ;; Import where renaming/contracting is involved
  (check-not-false (->identifier-resolved-binding-info path-str 'get-pure-port values))
  (check-not-false (->identifier-resolved-binding-info path-str "get-pure-port" values))
  ;; Get a module binding
  (check-equal? (->identifier-resolved-binding-info path-str "a-module-binding" values)
                (let ([path (string->path path-str)])
                  `((a-module-binding ,path)
                    (a-module-binding ,path))))
  ;; Get a lexical binding: Should return false
  (check-false (->identifier-resolved-binding-info path-str "a-lexical-binding" values))
  ;; Get something that's not a binding in at all: Should return false
  (check-false (->identifier-resolved-binding-info path-str "ASDFASDFDS" values))
  ;; Get whatever in some file not in expanded syntax cache: Should return false
  (check-false (->identifier-resolved-binding-info "not/yet/expanded.rkt" "whatever" values)))

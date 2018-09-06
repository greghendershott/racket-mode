#lang at-exp racket/base

(require racket/contract
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         syntax/modread
         racket/path
         syntax/parse
         syntax/strip-context
         syntax/stx
         (only-in "error.rkt" display-exn)
         "mod.rkt"
         (only-in "util.rkt" display-commented))

(provide dynamic-require/some-namespace)

;; A composition of dynamic-require and module->namespace that tries
;; to tolerate syntax errors. It tries to return a namespace with at
;; least some identifiers from the file -- such as from module
;; languages, requires, and definitions.
;;
;; Motivation:
;;
;;   https://github.com/greghendershott/racket-mode/issues/272
;;
;; You're working in #lang racket/base. You're partway through writing
;; a some expression, and realize you need to add (say)
;; with-module-reading-parameterization. You add syntax/modread to
;; your require.
;;
;; Now, you want to type with-m and hit TAB to complete. Plus after
;; that, you might want to C-. a.k.a. M-x racket-describe to read
;; docs.
;;
;; But you need to re-run, first, for the new require to take effect
;; and make the syntax/modread exports available.
;;
;; But if you re-run, your half-written expression results in a syntax
;; or runtime error. Now your REPL is just an empty racket/base.
;;
;; Annoying!
;;
;; Strategy: When dynamic-require fails, try again using a custom load
;; handler that rewrites the file -- "distill" it to a skeleton of
;; module forms, requires, and define-values. Try again using that.
;;
;; Note that it's important for the skeleton to include submodules,
;; because racket-mode lets you "enter" a submodule and work with
;; identifiers inside it (and only inside it).

(define is-skeleton
  "[Due to errors, REPL is just module language, requires, and stub definitions]")
(define is-base
  "[Due to errors, REPL is just racket/base]")

;; A composition of dynamic-require and module->namespace, but which
;; tries to tolerate errors in the source file and return _some_
;; namespace more useful than racket/base (if possible).
(define/contract (dynamic-require/some-namespace mod)
  (-> mod? namespace?)
  (parameterize ([current-load-relative-directory (mod-dir mod)]
                 [current-directory               (mod-dir mod)])
    (cond [(normal   mod) => values]
          [(skeletal mod) => (λ (ns)
                               (display-commented is-skeleton)
                               ns)]
          [else           (display-commented is-base)
                          (make-base-namespace)])))

(define/contract (normal mod)
  (-> mod? (or/c #f namespace?))
  (with-handlers ([exn:fail? (λ (e) (display-exn e) #f)])
    (dynamic-require (mod-rmp mod) #f)
    (module->namespace (mod-rmp mod))))

(define/contract (skeletal mod)
  (-> mod? (or/c #f namespace?))
  (with-handlers ([exn:fail? (const #f)]) ;don't show errors again
    (parameterize ([current-load      (make-load mod)]
                   ;; Module is cached in old namespace, so for `load`
                   ;; to be called, we need a fresh namespace.
                   [current-namespace (make-base-namespace)])
      (dynamic-require (mod-rmp mod) #f)
      (module->namespace (mod-rmp mod)))))

(define/contract (make-load mod)
  (-> mod? any)
  (define original-load (current-load))
  (define special-path (build-path (mod-dir mod) (mod-file mod)))
  (λ (path module-name)
    (if (equal? path special-path)
        (eval (skeleton (read-module-file path)))
        (original-load path module-name))))

(define (read-module-file file) ;Path-String -> Syntax
  (with-module-reading-parameterization
    (λ ()
      (parameterize ([read-accept-compiled #f])
        (with-input-from-file file read-syntax)))))

(define no-op-expr    #'(void))
(define no-op-def-val #''|Due to errors in source file, this value is from a "stub" define-values|)

(define (skeleton stx) ;Syntax -> Syntax
  ;; We got here because `stx` has either a syntax error or a runtime
  ;; error. If it has a syntax error, we can't `expand` it as whole.
  ;; Let's try to distill it to a skeleton of things that create
  ;; runtime, module-level bidings: requires and defines.
  ;;
  ;; To get #%require and define-values, we want to work with
  ;; fully-expanded syntax as much as possible. But we have to catch
  ;; syntax errors and replace each with #'(void). Also we want to
  ;; walk submodule forms for their bindings, but we can't expand a
  ;; submodule forms in isolation (that's a syntax error).
  ;;
  ;; So, the idea is to preserve the nested modules skeleton, and only
  ;; try to expand each of their module-level expressions to discover
  ;; bindings.
  ;;
  ;; Our final result should, as a whole, work with (eval (expand)).
  (strip-context
   ;; Unlike expand-syntax-to-top-form, expand-to-top-form does
   ;; namespace-syntax-introduce before expanding to top form.
   (let recur ([stx (expand-to-top-form stx)])
     (syntax-parse stx
       #:literal-sets   (kernel-literals)
       #:datum-literals (#%module-begin module+)
       ;; Note: A #lang file has #%module-begin even on initial read
       ;; and without calling `expand`. However, a (module) expression
       ;; file -- even when using with-module-reading-parameterization
       ;; -- doesn't. That only gets added by `expand`. But we can't
       ;; use `expand`. Anyway, it hardly matters as we're going to
       ;; remove everything interesting that a #%module-begin might
       ;; transform (IIUC). Just treat #%module-begin as begin.
       [((~and mod (~or module module*)) name:id lang:expr . es)
        #`(mod name lang . #,(stx-map recur #'es))]
       [(#%module-begin . es)
        #`(begin . #,(stx-map recur #'es))]
       [(module+ name:id . es)
        #`(module+ name . #,(stx-map recur #'es))]
       [_
        (let ([stx (with-handlers ([exn:fail:syntax? (const no-op-expr)])
                     (expand stx))])
          (syntax-parse stx
            #:literal-sets (kernel-literals)
            [(begin . es)                 #`(begin . #,(stx-map recur #'es))]
            [(#%require . _)              stx]
            [(define-values (id ...) . _) #`(define-values (id ...)
                                              (values
                                               #,@(stx-map (const no-op-def-val)
                                                           #'(id ...))))]
            [_                            no-op-expr]))]))))

(module+ test
  (require rackunit
           racket/set
           version/utils)

  ;; A example of the transformation we do.
  ;;
  ;; Note: Prior to Racket 6.3, expansion of `require` with
  ;; non-existent modules seems to be a syntax error. So in this test,
  ;; use modules that actually exist in minimal Racket.
  (check-equal? (syntax->datum
                 (skeleton
                  #'(module m racket/base
                      (#%module-begin
                       (require racket/pretty
                                racket/list)
                       (if)      ;stx err
                       (/ 1 0)   ;runtime err
                       (define foo 42)
                       (define-values (bar baz) (values 43 44))
                       (define (f x) (+ x 1))
                       (module* m #f
                         (require net/url)
                         (if) ;stx err
                         (/ 1 0)) ;runtime err
                       (module+ test
                         (require rackunit)
                         (if)) ;stx err
                       (module m typed/racket/base
                         (#%module-begin
                          (require racket/function)
                          (define id 42)
                          (if))))))) ;stx err
                (let ([no-op-expr    (syntax->datum no-op-expr)]
                      [no-op-def-val (syntax->datum no-op-def-val)])
                  `(module m racket/base
                    (begin
                     (begin (#%require racket/pretty) (#%require racket/list))
                     ,no-op-expr
                     ,no-op-expr
                     (define-values (foo) (values ,no-op-def-val))
                     (define-values (bar baz) (values ,no-op-def-val ,no-op-def-val))
                     (define-values (f) (values ,no-op-def-val))
                     (module* m #f
                      (#%require net/url)
                      (void)
                      (void))
                     (module+ test
                      (#%require rackunit)
                      ,no-op-expr)
                     (module m typed/racket/base
                      (begin
                        (#%require racket/function)
                        (define-values (id) (values ,no-op-def-val))
                        ,no-op-expr))))))

  ;; Helpers to write text or sexpr to a tempory .rkt file, then run
  ;; through dynamic-require/some-namespace and get the
  ;; namespace-mapped-symbols.

  (define/contract (call-with-temporary-file v proc)
    (-> any/c (-> mod? any/c) any/c)
    (define file #f)
    (dynamic-wind
      (λ ()
        (set! file (make-temporary-file "call-with-temporary-file-~a.rkt"))
        (call-with-output-file file #:exists 'replace
          (λ (out)
            (cond [(string? v) (display v out)]
                  [else        (write v out)]))))
      (λ () (proc (->mod/existing file)))
      (λ () (delete-file file))))

  (define/contract (syms mod)
    (-> mod? (listof symbol?))
    (namespace-mapped-symbols
     (parameterize ([current-namespace (make-base-empty-namespace)])
       (dynamic-require/some-namespace mod))))

  (define (do v)
    (define op (open-output-string))
    (define result (parameterize ([current-error-port op])
                     (call-with-temporary-file v syms)))
    (check-match (get-output-string op)
                 (regexp (string-append (regexp-quote is-skeleton) "\n$")))
    result)

  ;; Despite a syntax error and a runtime error, a binding provided by
  ;; a require is available in the namespace in both:

  ;; (a) A #lang file:
  (check-not-false
   (memq 'pretty-print (do @~a{#lang racket/base
                               (if)
                               (require racket/pretty)})))

  ;; (b) A module expression file:
  (check-not-false
   (memq 'pretty-print (do `(module m racket/base
                             (if)
                             (require racket/pretty)))))

  ;; Requiring exactly 1 binding adds exactly that symbol to the
  ;; namespace:
  (check-equal? (set-subtract
                 (list->set
                  (do `(module m racket/base
                        (/ 1 0)
                        (require (only-in racket/pretty pretty-print)))))
                 (list->set
                  (do `(module n racket/base
                        (/ 1 0)))))
                (set 'pretty-print)))

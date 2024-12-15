;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require (only-in racket/list
                  append*
                  add-between
                  cartesian-product)
         racket/match
         racket/set
         syntax/parse/pre
         (only-in syntax/stx
                  stx-map)
         "../util.rkt")

(provide requires/tidy
         requires/trim
         requires/base)

(module+ test
  (require rackunit
           racket/format))

(define (requires/tidy reqs)
  ;; (-> (listof require-form?) string?)
  (require-pretty-format
   (tidy reqs)))

(module+ test
  (check-equal?
   (requires/tidy '((require z/z)
                    (require (prefix-in b: b))
                    (require (combine-in)) ;=> no-op
                    (require (combine-in (only-in "a.rkt"))) ;=> just "a.rkt"
                    (require (combine-in (rename-in "b.rkt"))) ;=> just "b.rkt"
                    (require (combine-in (except-in "c.rkt"))) ;=> just "c.rkt"
                    (require (combine-in z y x "w.rkt" "x.rkt")) ;sort
                    (require a)
                    (require c d e)))
   @~a{(require a
                (prefix-in b:
                           b)
                c
                d
                e
                (combine-in x
                            y
                            z
                            "w.rkt"
                            "x.rkt")
                z/z
                "a.rkt"
                "b.rkt"
                "c.rkt")

      }))

;; Note: Why pass in a list of the existing require forms -- why not
;; just use the "keep" list from show-requires? Because the keep list
;; only states the module name, not the original form. Therefore if
;; the original require has a subform like `(only-in mod f)` (or
;; rename-in, except-in, &c), we won't know how to preserve that
;; unless we're given it. That's why our strategy must be to look for
;; things to drop, as opposed to things to keep.
(define (requires/trim path-str reqs)
  ;; (-> path-string? (listof require-form?) string?)
  (require-pretty-format
   (tidy reqs
         #:drops (drops (analyze path-str)))))

(define (requires/base path-str reqs)
  ;; (-> path-string? (listof require-form?) string?)
  (define a (analyze path-str))
  (require-pretty-format
   (tidy reqs
         #:adds  (adds/racket->racket/base a)
         #:drops (drops a))))

;;; analysis by macro-debugger/analysis/check-requires

(define (error-thunk)
  (error 'requires
         "Cannot work until you `raco pkg install macro-debugger-lib`"))

(define show-requires
  (safe-dynamic-require 'macro-debugger/analysis/check-requires
                        'show-requires
                        error-thunk))

(define (analyze path-str)
  (define-values (base name _) (split-path (string->path path-str)))
  (parameterize ([current-load-relative-directory base]
                 [current-directory base])
    (show-requires name)))

(define (drops analyses)
  (for*/list ([a (in-list analyses)]
              [v (in-value
                  (match a
                    [(list 'drop mod level) (cons mod level)]
                    [_ #f]))]
              #:when v)
    v))

;; Use `bypass` directives to indicate what explicit requires must be
;; added when changing #lang from racket to racket/base.
;;
;; Note: Because `show-requires` seems to miss the need to (require
;; (for-syntax racket/base)), we're making the choice to always add
;; that, even when the specific user program doesn't need it.
(define (adds/racket->racket/base analyses)
  (for*/list ([a (in-list analyses)]
              [replacements (in-value
                             (match a
                               [(list 'bypass 'racket _level rs)
                                (cons '(racket/base 1 #f)
                                      rs)]
                               [_ null]))]
              [replacement (in-list replacements)]
              [v (in-value
                  (match replacement
                    ;; Don't need racket/base when that will be the
                    ;; #lang, so ignore.
                    [(list 'racket/base 0 _) #f]
                    ;; We're ignoring the "rename" field because it's
                    ;; N/A for our specfic scenario, changing racket
                    ;; to racket/base.
                    [(list mod level _) (cons mod level)]))]
              #:when v)
    v))

;;; tidy

(define current-phase-level (make-parameter 0))
(define current-drops (make-parameter null)) ; (listof (cons mod level))

;; The `sortable` (names are hard) struct represents the syntax for a
;; require-spec, along with information about how to sort it.
;;
;; The `group` is a number, used to order things like absolute path
;; symbols above relative path strings.
;;
;; The `key` is a symbol or string, used to sort alphabetically within
;; each group.
;;
;; The sortable-group+key function produces a string comprising both,
;; suitable to give to `sort` as the #:key, with of course `string<?`
;; as the less-than arg.
;;
;; The simplest require-spec is a root-module-path like racket/string
;; or "foo.rkt".
;;
;; Some require-specs contain multiple module paths, for example
;; combine-in. When we parse these, we produce a `sortable` from the
;; component require-specs; see `copy` and `combine`.
(struct sortable (stx group key) #:transparent)

(define group-id     0)
(define group-lib    1)
(define group-file   2)
(define group-string 3)
(define group-quote  4)

(define (sortable-group+key v)
  (format "~a:~a" (sortable-group v) (sortable-key v)))

;; Because the unused requires analysis works at the level of module
;; paths, here we possibly return #f instead of a `sortable` struct.
;; This value may percolate up through more complex require-specs that
;; wrap a module path, causing them to be false and elided.
(define (modpath->sortable sort-group modpath-stx)
  (define mod (syntax->datum modpath-stx))
  (and (not (member (cons mod (current-phase-level))
                    (current-drops)))
       (sortable modpath-stx sort-group mod)))

(define (copy orig stx)
  (and orig
       (sortable stx
                 (sortable-group orig)
                 (sortable-key orig))))

(define (combine sortables stx-preface)
  (match (sort (filter values sortables) ;non-false
               #:key sortable-group+key
               string<?)
    [(list) #f]  ;elide
    [(list v) v] ;reduce
    [vs          ;use first for sort, plus all stxs
     (copy (car vs)
           #`(#,@stx-preface #,@(map sortable-stx vs)))]))

;; These syntax classes generally follow the grammar from the docs.

(define-syntax-class root-module-path
  #:attributes (s)
  #:datum-literals (quote lib file) ;intentionally omit `planet`
  (pattern id:id
           #:attr s (modpath->sortable group-id #'id))
  (pattern (lib) ;elide
           #:attr s #f)
  (pattern (lib str:string ...+)
           #:attr s
           (combine (stx-map (λ (str) (modpath->sortable group-lib str))
                             #'(str ...))
                    #'(lib)))
  (pattern (file str:string)
           #:attr s (modpath->sortable group-file #'str))
  (pattern str:string
           #:attr s (modpath->sortable group-string #'str))
  (pattern (quote id:id)
           #:attr s (modpath->sortable group-quote #'(quote id))))

(define-syntax-class submod-path-element
  (pattern _id:id)
  (pattern ".."))

(define-syntax-class module-path
  #:attributes (s)
  #:datum-literals (submod)
  (pattern rmp:root-module-path
           #:attr s (attribute rmp.s))
  (pattern (submod rmp:root-module-path _:submod-path-element ...+)
           #:attr s (attribute rmp.s))
  (pattern (submod (~or "." "..") _:submod-path-element ...+)
           #:attr s (sortable this-syntax 99 ".")))

(define-syntax-class require-spec
  #:attributes (s)
  #:datum-literals (;racket/base
                    only-in except-in prefix-in rename-in combine-in
                    relative-in only-meta-in only-space-in for-space
                    ;racket/require
                    matching-identifiers-in subtract-in filtered-in
                    path-up multi-in)
  #:local-conventions ([spec require-spec])

  ;; Main jobs here:
  ;;
  ;; 1. Calculate a `sortable` as the value for the `s` attribute. For
  ;; "complex" specs this uses the sort info from one of its
  ;; sub-specs; see `copy` and `combine`.
  ;;
  ;; 2. Simplify specs. For example `(only-in m)` -- with no
  ;; exceptions listed -- is legal but can be reduced to just `m`.
  ;; Furthermore, `(only-in m)` and `m` both elide to nothing when `m`
  ;; is a member of `current-drops`, in which case the `a` attribute
  ;; is #f. Likewise (combine-in) elides to #f.

  (pattern mp:module-path
           #:attr s (attribute mp.s))

  (pattern (only-in spec) ;reduce
           #:attr s (attribute spec.s))
  (pattern (only-in spec _id-maybe-renamed ...+)
           #:attr s (copy (attribute spec.s) this-syntax))

  (pattern (except-in spec) ;reduce
           #:attr s (attribute spec.s))
  (pattern (except-in spec _id ...+)
           #:attr s (copy (attribute spec.s) this-syntax))

  (pattern (prefix-in _prefix:id spec)
           #:attr s (copy (attribute spec.s) this-syntax))

  (pattern (rename-in spec) ;reduce
           #:attr s (attribute spec.s))
  (pattern (rename-in spec [_orig:id _bind:id] ...)
           #:attr s (copy (attribute spec.s) this-syntax))

  (pattern (relative-in _mp:module-path) ;elide
           #:attr s #f)
  (pattern (relative-in mp:module-path spec ...+)
           #:attr s (combine (attribute spec.s)
                             #'(relative-in mp)))

  (pattern (combine-in spec ...)
           #:attr s (combine (attribute spec.s)
                             #'(combine-in)))

  (pattern (only-meta-in phase spec ...)
           #:attr s (combine (attribute spec.s)
                             #`(only-meta-in phase)))

  (pattern (only-space-in space spec ...)
           #:attr s (combine (attribute spec.s)
                             #`(only-space-in space)))

  (pattern (for-space space spec ...)
           #:attr s (combine (attribute spec.s)
                             #`(for-space space)))

  (pattern (matching-identifiers-in _regexp spec)
           #:attr s (copy (attribute spec.s) this-syntax))

  (pattern (subtract-in spec) ;reduce
           #:attr s (attribute spec.s))
  (pattern (subtract-in spec _subtracted-spec ...)
           #:attr s (copy (attribute spec.s) this-syntax))

  (pattern (filtered-in _proc spec)
           #:attr s (copy (attribute spec.s) this-syntax))

  (pattern (path-up) ;elide
           #:attr s #f)
  (pattern (path-up str:string ...+)
           #:attr s
           (combine (stx-map (λ (str) (modpath->sortable 1 str))
                             #'(str ...))
                    #'(path-up)))

  (pattern (multi-in) ;elide
           #:attr s #f)

  (pattern (multi-in id:id ...+) ;reduce to id/id ..
           #:attr s
           (modpath->sortable group-id
                              #`#,(join (syntax->datum #'(id ...))
                                        symbol->string
                                        string->symbol)))
  (pattern (multi-in str:string ...+) ;reduce to str/str ..
           #:attr s
           (modpath->sortable group-string
                              #`#,(join (syntax->datum #'(str ...)))))

  (pattern (multi-in id:id ...+ p:multi-in-id-subs) ;1xN
           #:attr s
           (multi-in-1xN (syntax->datum #'(id ...))
                         (attribute p.ids)
                         symbol->string
                         string->symbol))
  (pattern (multi-in str:string ...+ p:multi-in-string-subs) ;1xN
           #:attr s
           (multi-in-1xN (syntax->datum #'(str ...))
                         (attribute p.strs)))

  (pattern (multi-in p:multi-in-id-subs ...+)
           #:attr s (multi-in-general (attribute p.ids)
                                      symbol->string
                                      string->symbol))
  (pattern (multi-in p:multi-in-string-subs ...+)
           #:attr s (multi-in-general (attribute p.strs))))

(define-syntax-class multi-in-string-subs
  #:attributes (strs)
  (pattern str:string
           #:attr strs (syntax->datum #'(str)))
  (pattern (str:string ...+)
           #:attr strs (syntax->datum #'(str ...))))

(define-syntax-class multi-in-id-subs
  #:attributes (ids)
  (pattern id:id
           #:attr ids (syntax->datum #'(id)))
  (pattern (id:id ...+)
           #:attr ids (syntax->datum #'(id ...))))

(define (join vs [->str values] [str-> values])
  (str->
   (apply string-append
          (add-between (map ->str vs)
                       "/"))))

;; The simple case of one or more single subs and a final list of subs
;; -- e.g. (multi-in typed racket (string format)) -- which allows us
;; to handle dropped modules by removing an item from the final subs.
;; Or, if all final subs removed, elide the multi-in form entirely.
(define (multi-in-1xN single-subs final-subs [->str values] [str-> values])
  (define sorted (sort final-subs #:key ->str string<?))
  (define filtered
    (for*/list ([p (in-list sorted)]
                [mod (in-value (join (append single-subs (list p))
                                     ->str str->))]
                #:unless (member (cons mod (current-phase-level))
                                 (current-drops)))
      p))
  (cond
    [(null? filtered) #f] ;nothing remains, elide entirely
    [else
     (sortable #`(multi-in #,@single-subs
                           #,(simplify-subs filtered))
               (if (equal? ->str values) group-string group-id)
               (join (append single-subs (list (car final-subs)))
                         ->str str->))]))

;; The general case of any cartesian product.
;;
;; Expects every subs to be normalized to a list, even if it contains
;; just one item.
(define (multi-in-general subs [->str values] [str-> values])
  (define sorted (map (λ (sub-path)
                        (sort sub-path #:key ->str string<?))
                      subs))
  (define sort-key (join (map car sorted) ->str str->))
  (define sort-group (if (string? sort-key) group-string group-id))
  (define products (apply cartesian-product sorted))
  (define mods (for/list ([vs (in-list products)])
                 (join vs ->str str->)))
  (define drops (for*/list ([mod (in-list mods)]
                            #:when (member (cons mod (current-phase-level))
                                           (current-drops)))
                  mod))
  (cond
    ;; Trivial case: As-is, just simplify.
    [(null? drops)
     (sortable #`(multi-in #,@(simplify-subs sorted))
               sort-group
               sort-key)]
    [else
     ;; General case for any cartesian product: IIUC the best we can
     ;; do is subtract, i.e. wrap the multi-in in subtract-in.
     ;;
     ;; Example: (multi-in (a b) (0 1) (x y))
     ;;
     ;; If we're to drop only b/1/y -- but keep a/1/y -- how else
     ;; could we rewrite this?
     (sortable #`(subtract-in (multi-in #,@(simplify-subs sorted))
                              #,@drops)
               sort-group
               sort-key)]))

(define (simplify-subs subs)
  (for/list ([s (in-list subs)])
    (match s
      [(list v) v]
      [v v])))

(module+ example
  (syntax-parse #'(multi-in "math" "matrix" "utils.rkt")
    [spec:require-spec (attribute spec.s)])
  (syntax-parse #'(multi-in "math" ("matrix.rkt" "foo.rkt"))
    [spec:require-spec (attribute spec.s)])
  (parameterize ([current-drops (list (cons 'racket/format 0))])
    (syntax-parse #'(multi-in racket (string format match))
      [spec:require-spec (attribute spec.s)]))
  (parameterize ([current-drops (list (cons 'racket/format 0))])
    (syntax-parse #'(multi-in racket (string format))
      [spec:require-spec (attribute spec.s)]))
  (parameterize ([current-drops (list (cons 'racket/string 0))])
    (syntax-parse #'(multi-in (foobar racket) (string format))
      [spec:require-spec (attribute spec.s)])))

(define-syntax-class phased-require-spec
  #:attributes (phase specs)
  #:datum-literals (racket/require for-syntax for-template for-label for-meta)
  #:local-conventions ([spec require-spec])
  (pattern racket/require
           #:attr phase 'racket/require
           #:with specs #'(racket/require))
  ;; Note: We don't attempt to recognize nested phased specs such as
  ;; (for-syntax (for-template _)) and reduce to a net phase level.
  ;; Although such forms are legal, and might be generated by macros,
  ;; instead our goal here is to parse specs that a human is likely to
  ;; write by hand.
  (pattern (for-syntax spec ...)
           #:attr phase 1
           #:with specs #'(spec ...))
  (pattern (for-template spec ...)
           #:attr phase -1
           #:with specs #'(spec ...))
  (pattern (for-label spec ...)
           #:attr phase #f
           #:with specs #'(spec ...))
  (pattern (for-meta phase-level spec ...)
           #:attr phase (syntax->datum #'phase-level)
           #:with specs #'(spec ...))
  (pattern spec
           #:attr phase 0
           #:with specs #'(spec)))

(define (tidy reqs #:adds [adds null] #:drops [drops null])
  (define ht (make-hash)) ;phase-level => "top-level" req specs
  (define (add! v)
    (hash-update! ht
                  (current-phase-level)
                  (λ (s) (set-add s v))
                  set))
  ;; Denormalize
  (syntax-parse reqs
    #:datum-literals (require)
    [((require spec:phased-require-spec ...) ...)
     (for ([spec (in-list (syntax->list #'(spec ... ...)))])
       (syntax-parse spec
         [v:phased-require-spec
          (parameterize ([current-phase-level (attribute v.phase)]
                         [current-drops drops])
            (for ([spec (in-list (syntax->list #'v.specs))])
              (syntax-parse spec
                [spec:require-spec
                 (define vs (match (attribute spec.s)
                              [#f              null]
                              [(? sortable? v) (list v)]
                              [(? list? vs)    vs]))
                 (for ([v (in-list vs)])
                   (when v (add! v)))])))]))])
  (for ([mod+level (in-list adds)])
    (match-define (cons mod level) mod+level)
    (parameterize ([current-phase-level level])
      (define group (if (string? mod) group-string group-id))
      (define v (modpath->sortable group #`#,mod))
      (when v (add! v))))
  ;; Normalize
  (define (for-level level k)
    (define sorted (sort (set->list (hash-ref ht level set))
                         #:key sortable-group+key
                         string<?))
    (define datums (for/list ([v (in-list sorted)])
                     (syntax->datum (sortable-stx v))))
    (cond [(null? datums) null]
          [else           (k datums)]))
  (define ((preface . pres) specs)
    `((,@pres ,@specs)))
  (define meta-levels
    (sort (filter (match-lambda
                    [(or -1 0 1) #f]
                    [(? number?) #t]
                    [_           #f])
                  (hash-keys ht))
          <))
  `(require
    ,@(for-level 'racket/require values)
    ,@(for-level  1 (preface 'for-syntax))
    ,@(for-level -1 (preface 'for-template))
    ,@(for-level #f (preface 'for-label))
    ,@(append* (for/list ([level (in-list meta-levels)])
                 (for-level level (preface 'for-meta level))))
    ,@(for-level 0 values)))

(module+ test
  (check-equal? (tidy #:drops (list (cons 'racket/string 0)
                                    (cons 'bar/b 0))
                      '((require z c b a)
                        (require racket/require)
                        (require (multi-in mi-z (mi-z0 mi-z1)))
                        (require (multi-in mi-a (mi-a1 mi-a0)))
                        (require (multi-in baz (a b)))
                        (require (multi-in racket (format string match)))
                        (require (multi-in racket string)) ;disappears
                        (require (multi-in racket list)) ;=> racket/list
                        (require (multi-in "dir" "file.rkt")) ;=> "dir/file.rkt"
                        (require (multi-in (foo bar) (a b c)))
                        (require (for-meta 4 m41 m40))
                        (require (for-meta -4 m-41 m-40))
                        (require (for-label l1 l0))
                        (require (for-template t1 t0))
                        (require (for-syntax s1 s0))
                        (require
                         "a.rkt" "b.rkt" "c.rkt" "z.rkt"
                         (only-in "mod.rkt" oi)
                         (only-in mod oi))))
                '(require
                  racket/require
                  (for-syntax s0 s1)
                  (for-template t0 t1)
                  (for-label l0 l1)
                  (for-meta -4 m-40 m-41)
                  (for-meta 4 m40 m41)
                  a
                  b
                  (subtract-in ;drop bar/b but keep foo/b
                   (multi-in (bar foo) (a b c))
                   bar/b)
                  (multi-in baz (a b))
                  c
                  (multi-in mi-a (mi-a0 mi-a1))
                  (multi-in mi-z (mi-z0 mi-z1))
                  (only-in mod oi)
                  (multi-in racket (format match)) ;racket/string dropped
                  racket/list
                  z
                  "a.rkt" "b.rkt" "c.rkt"
                  "dir/file.rkt"
                  (only-in "mod.rkt" oi)
                  "z.rkt")))

;;; pretty

(define (require-pretty-format x)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (require-pretty-print x))
  (get-output-string out))

(module+ test
  (check-equal? (require-pretty-format
                 '(require a))
                @~a{(require a)

                    })
  (check-equal? (require-pretty-format
                 '(require a b))
                @~a{(require a
                             b)

                    })
  (check-equal? (require-pretty-format
                 '(require (for-syntax a b) (for-meta 2 c d) e f))
                @~a{(require (for-syntax a
                                         b)
                             (for-meta 2
                                       c
                                       d)
                             e
                             f)

                    })
  (check-equal? (require-pretty-format
                 `(require (only-in m a b) (except-in m a b)))
                @~a{(require (only-in m
                                      a
                                      b)
                             (except-in m
                                        a
                                        b))

                    }))

;; Pretty print a require form.
;;
;; All forms are indented with `require` indent style.
;;
;; Unlike pretty-print, we always print the head item plus the first
;; arg on the same line, then any subsequent items each on their own
;; line indented with the first arg. We don't use pretty-print's
;; approach of hot or cold porridge -- i.e. trying to squeeze forms
;; onto the same line <= pretty-print-columns, otherwise every item on
;; its own line all indented with the head. Instead we want warm
;; uniformity. (But we break that rule for module-path forms like
;; `submod`, which we print on one line.)
;;
;; Also there are details like printing `[old-id new-id]` forms with
;; square brackets, by convention.
;;
;; Note: Although it might be more robust to write this in a grammar
;; style, the following is less tedious and seems OK.
(define (require-pretty-print v)
  (struct text (v)) ;a value to `display` not `write`
  (let prn ([v v] [indent? #t] [indent 0])
    (define (maybe-indent)
      (when indent?
        (for ([_ (in-range indent)])
          (display #\space))))
    (define (prn-form head this more)
      (define head-str (symbol->string head))
      (define new-indent (+ indent 2 (string-length head-str)))
      (maybe-indent)
      (printf "(~a " head-str)
      (prn this #f new-indent)
      (for ([v more])
        (newline)
        (let ([v (match v
                   [(list old new)
                    #:when (memq head '(rename-in only-in))
                    (text (format "[~a ~a]" old new))]
                   [v v])])
          (prn v #t new-indent)))
      (display ")"))
    (match v
      [(and (or (? string?)
                (? symbol?)
                (cons (or 'submod 'quote 'lib 'file 'planet '= '+ '-) _))
            mod-path)
       (maybe-indent)
       (write mod-path)]
      [(list* (? symbol? head) this more)
       (prn-form head this more)
       (when (eq? head 'require)
         (newline))]
      [(text s)
       (maybe-indent)
       (display s)]
      [v ;Some other value, like number or #f for phase-level
       (maybe-indent)
       (write v)])))

(module+ test
  (check-equal? (require-pretty-format
                 '(require
                   (prefix-in pre:
                    (combine-in
                     (only-in "a.rkt" b [c d])
                     (rename-in (submod m sub) [y z] [p q])))))
                @~a{(require (prefix-in pre:
                                        (combine-in (only-in "a.rkt"
                                                             b
                                                             [c d])
                                                    (rename-in (submod m sub)
                                                               [y z]
                                                               [p q]))))

                    }))

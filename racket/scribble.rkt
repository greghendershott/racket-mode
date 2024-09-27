;; Copyright (c) 2013-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/format
         racket/match
         racket/promise
         racket/set
         racket/string
         (only-in scribble/core
                  tag?
                  content->string)
         scribble/blueboxes
         scribble/manual-struct
         scribble/xref
         scribble/tag
         setup/main-doc
         setup/xref
         (only-in setup/dirs get-doc-search-dirs)
         syntax/parse/define
         version/utils
         "define-fallbacks.rkt"
         "elisp.rkt")

;; Fallbacks when new index structs aren't available (before
;; scribble-lib 1.54, which ~= Racket 8.14.0.6).
(define-fallbacks scribble/manual-struct
  [(exported-index-desc*? _) #f]
  [(exported-index-desc*-extras _) #hasheq()]
  [(index-desc? _) #f]
  [(index-desc-extras _) #hasheq()])

(provide binding->path+anchor
         identifier->bluebox
         bluebox-command
         doc-index
         libs-exporting-documented
         module-doc-path
         refresh-module-doc-path-index!)

(module+ test
  (require rackunit))

;; When running on a machine with little memory, such as a small VPS
;; or AWS instance, I have seen the oom-killer terminate the process
;; after we try to handle a back end command that does some of these
;; documentation operations. Presumably they use enough memory that
;; Racket asks the OS for more? To make that less likely, do a major
;; GC before/after. So far this seems to be a successful mitigation,
;; although it also seems like a kludge.
(define (call-avoiding-oom-killer thunk)
  (collect-garbage 'major)
  (begin0 (thunk)
    (collect-garbage 'major)))

(define-simple-macro (with-less-memory-pressure e:expr ...+)
  (call-avoiding-oom-killer (λ () e ...)))

(define/contract (binding->path+anchor stx)
  (-> identifier? (or/c #f (cons/c path-string? (or/c #f string?))))
  (with-less-memory-pressure
    (let* ([xref (load-collections-xref)]
           [tag  (xref-binding->definition-tag xref stx 0)]
           [p+a  (and tag (tag->path+anchor xref tag))])
      p+a)))

(define (tag->path+anchor xref tag)
  (define-values (path anchor) (xref-tag->path+anchor xref tag))
  (and path anchor (cons path anchor)))

;;; Blueboxes

(define racket-newer-than-6.12 (version<? "6.12" (version)))

(define bluebox-cache #f)

(define (get-bluebox-string tag)
  (unless bluebox-cache
    (set! bluebox-cache (make-blueboxes-cache #t)))
  (match (and racket-newer-than-6.12
              (fetch-blueboxes-strs tag
                                    #:blueboxes-cache bluebox-cache))
    [(list* _kind strs)
     (string-replace (string-join strs "\n")
                     "\u00A0"
                     " ")]
    [_ #f]))

(define/contract (identifier->bluebox stx)
  (-> identifier? (or/c #f string?))
  (match (xref-binding->definition-tag (load-collections-xref) stx 0)
    [(? tag? tag) (get-bluebox-string tag)]
    [_ #f]))

(define (bluebox-command str)
  (match (read (open-input-string str))
    [(? tag? tag) (get-bluebox-string tag)]
    [_ #f]))

(module+ test
  ;; This test succeeds on all Racket versions before and after 6.10.
  ;; I spent an hour installing 6.10 locally and exploring the problem
  ;; but so far have no clue. As neither 6.10 nor I are getting any
  ;; younger, I am choosing to ignore this, for now.
  ;;
  ;; Probably https://github.com/racket/drracket/issues/118
  (when racket-newer-than-6.12
    (check-equal? (identifier->bluebox #'list)
                  "(list v ...) -> list?\n  v : any/c"))
  (check-false (identifier->bluebox (datum->syntax #f (gensym)))))

;;; Documentation index

;; Note that `xref-index` returns a list of 30K+ `entry` structs. We
;; can't avoid that with the official API. That will bump peak memory
;; use. :( Best we can do is sandwich it in major GCs, to avoid the
;; peak going even higher. Furthermore in doc-index-names we avoid
;; making _another_ 30K+ list, by returning a thunk for elisp-write
;; to call, to do "streaming" writes.

;; This is like an HTTP request with an If-None-Match header.
(define (doc-index last-etag)
  (match (doc-index-etag)
    [(== last-etag) 'not-modified]
    [etag (make-doc-index-thunk etag)]))

(define (doc-index-etag)
  (~s
   (for/list ([dir (in-list (get-doc-search-dirs))])
     (define file (build-path dir "docindex.sqlite"))
     (cons dir
           (and (file-exists? file)
                (file-or-directory-modify-seconds file))))))

(define (hide-desc? desc)
  ;; Don't show doc for constructors; class doc suffices.
  (or (constructor-index-desc? desc)
      (and (exported-index-desc*? desc)
           (let ([ht (exported-index-desc*-extras desc)])
             (or (hash-ref ht 'hidden? #f)
                 (hash-ref ht 'constructor? #f))))))

(define ((make-doc-index-thunk etag))
  (with-less-memory-pressure
    (with-parens
      (elisp-writeln etag)
      (define xref (load-collections-xref))
      (for* ([(entry uid) (in-indexed (xref-index xref))]
             [desc (in-value (entry-desc entry))]
             #:when desc
             #:unless (hide-desc? desc)
             [term (in-value (car (entry-words entry)))]
             [tag (in-value (entry-tag entry))])
        (define-values (path anchor) (xref-tag->path+anchor xref tag))
        (define (method-what)
          (cond
            [(method-tag? tag)
             (define-values (c/i _m) (get-class/interface-and-method tag))
             (format "method of ~a" c/i)]
            [else "method"]))
        (define (doc-from)
          (string-append
           "◊ "
           (match (path->main-doc-relative path)
             [(cons 'doc byte-strings)
              (define path-parts (map bytes->path byte-strings))
              (define rel-html (apply build-path path-parts))
              (path->string
               (path-replace-extension rel-html #""))]
             [_ (~a tag)])))
        (define-values (what from fams sort-order)
          (cond
            ;; New structs
            [(exported-index-desc*? desc)
             (define ht (exported-index-desc*-extras desc))
             (define kind (hash-ref ht 'kind))
             (define what (if (string=? kind "method")
                              (method-what)
                              kind))
             (define from
               (string-join (match (hash-ref ht 'display-from-libs #f)
                              [(? list? contents)
                               (map content->string contents)]
                              [#f
                               (map ~s (exported-index-desc-from-libs desc))])
                            ", "))
             (define fams (match (hash-ref ht 'language-family #f)
                            [(? list? fams) (string-join (map ~a fams) ", ")]
                            [#f "Racket"]))
             (define sort-order (hash-ref ht 'sort-order 0))
             (values what from fams sort-order)]
            [(index-desc? desc)
             (define ht (index-desc-extras desc))
             (define what (match (hash-ref ht 'module-kind #f)
                            ['lib    "module"]
                            ['lang   "language"]
                            ['reader "reader"]
                            [#f      "documentation"]
                            [v       (~a v)]))
             (define from
               (match (hash-ref ht 'display-from-libs #f)
                 [(? list? contents)
                  (string-join (map content->string contents) ", ")]
                 [#f (doc-from)]))
             (define fams (match (hash-ref ht 'language-family #f)
                            [(? list? fams) (string-join (map ~a fams) ", ")]
                            [#f "Racket"]))
             (define sort-order (hash-ref ht 'sort-order 0))
             (values what from fams sort-order)]
            ;; Older structs
            [(exported-index-desc? desc)
             (define what
               (match desc
                 [(? language-index-desc?)  "language"]
                 [(? reader-index-desc?)    "reader"]
                 [(? form-index-desc?)      "syntax"]
                 [(? procedure-index-desc?) "procedure"]
                 [(? thing-index-desc?)     "value"]
                 [(? struct-index-desc?)    "structure"]
                 [(? class-index-desc?)     "class"]
                 [(? interface-index-desc?) "interface"]
                 [(? mixin-index-desc?)     "mixin"]
                 [(? method-index-desc?)    (method-what)]
                 [_ ""]))
             (define from (string-join (map ~s (exported-index-desc-from-libs desc)) ", "))
             (values what from "" 0)]
            [(module-path-index-desc? desc)
             (values "module" "" "" 0)]
            [else
             (values "documentation" (doc-from) "" 0)]))
        (elisp-writeln (list uid term sort-order what from fams path anchor)))
      (newline))))

;;; This is for the requires/find command

;; Given some symbol as a string, return the modules providing it,
;; sorted by most likely to be desired.
(define (libs-exporting-documented sym-as-str)
  (with-less-memory-pressure
    (define xref (load-collections-xref))
    (define results
      (for*/set ([entry (in-list (xref-index xref))]
                 [desc (in-value (entry-desc entry))]
                 #:when (exported-index-desc? desc)
                 [name (in-value (symbol->string
                                  (exported-index-desc-name desc)))]
                 #:when (equal? name sym-as-str)
                 [libs (in-value (map symbol->string
                                      (exported-index-desc-from-libs desc)))]
                 #:when (not (null? libs)))
        ;; Take just the first lib. This usually seems to be the
        ;; most-specific, e.g. (racket/base racket).
        (car libs)))
    (sort (set->list results)
          string<?
          #:cache-keys? #t
          #:key
          (lambda (lib)
            (match lib
              [(and (pregexp "^racket/") v)
               (string-append "0_" v)]
              [(and (pregexp "^typed/racket/") v)
               (string-append "1_" v)]
              [v v])))))

;; This is for package-details

(define (build-module-doc-path-index)
  (delay/thread
   (define xref (load-collections-xref))
   (for*/hash ([entry (in-list (xref-index xref))]
               [desc (in-value (entry-desc entry))]
               [module? (in-value (module-path-index-desc? desc))]
               [lang?   (in-value (language-index-desc? desc))]
               #:when (or module? lang?))
     (define k (cons (car (entry-words entry))
                     lang?))
     (define v (let-values ([(p a) (xref-tag->path+anchor xref (entry-tag entry))])
                 (let ([p (path->string p)]
                       [a a])
                   (cons p a))))
     (values k v))))

(define module-doc-path-index (build-module-doc-path-index))

(define (refresh-module-doc-path-index!)
  (set! module-doc-path-index (build-module-doc-path-index)))

(define (module-doc-path mod-path-str lang?)
  (hash-ref (force module-doc-path-index)
            (cons mod-path-str lang?)
            #f))

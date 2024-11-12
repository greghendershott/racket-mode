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
         version/utils
         "define-fallbacks.rkt"
         "lib-pkg.rkt"
         "util.rkt"
         "xref.rkt")

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
         doc-search
         module-doc-path
         refresh-doc-index!)

(module+ test
  (require rackunit))

(define/contract (binding->path+anchor stx)
  (-> identifier? (or/c #f (cons/c path-string? (or/c #f string?))))
  (let* ([tag (xref-binding->definition-tag xref stx 0)]
         [p+a (and tag (tag->path+anchor xref tag))])
    p+a))

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
  (match (xref-binding->definition-tag xref stx 0)
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

;;; Documentation search

;; A trie where a node's children are represented as a hash-table from
;; char to node. Each node also has a set of zero or more values
;; (multiple, since we have so many duplicate keys e.g. various
;; flavors of "define" or "print"). The values are promises: Although
;; the trie is built for all search terms, the initial promise
;; contains just a `desc` and tag; the full doc-index-value is not
;; computed until retrieved.
(struct node (kids values))
;; kids:   (hash/c char? node?)
;; values: (set/c (promise/c doc-trie-value))
(define (empty-node) (node (make-hasheq) (set)))

(define doc-index-trie-root #f)

(define (refresh-doc-index!)
  (set! doc-index-trie-root
        (with-memory-use/log "build-doc-search-trie"
          (with-time/log "build-doc-search-trie"
            (build-doc-search-trie)))))

(define (doc-search prefix [limit 256])
  (unless doc-index-trie-root
    (refresh-doc-index!))
  (trie-find doc-index-trie-root prefix limit))

(define (trie-find root prefix limit)
  (match (string->list prefix)
    [(list)
     null]
    [chs
     (define results (mutable-set))
     (let find! ([n   root]
                 [chs chs])
       (match-define (cons ch more) chs)
       (define sub (hash-ref (node-kids n) ch (empty-node)))
       (cond
         [(null? more)
          (define (report! n)
            (for ([v (in-set (node-values n))])
              (set-add! results (force v)))
            (when (< (set-count results) limit)
              (for ([n (in-hash-values (node-kids n))])
                (report! n))))
          (report! sub)]
         [else
          (find! sub more)]))
     (set->list results)]))

;; Find values for all nodes for which `prefix` is an exact full or
;; prefix match -- for use producing completion candidates.
(define (trie-add! root str value)
  (let add! ([n   root]
             [chs (string->list str)])
    (match chs
      [(list ch)
       (hash-update! (node-kids n)
                     ch
                     (λ (n)
                       (node (node-kids n)
                             (set-add (node-values n) value)))
                     empty-node)]
      [(cons ch more)
       (add! (hash-ref! (node-kids n) ch (empty-node))
             more)])))

(define (build-doc-search-trie)
  (define root (empty-node))
  (define (hide-desc? desc)
    ;; Don't show doc for constructors; class doc suffices.
    (or (constructor-index-desc? desc)
        (and (exported-index-desc*? desc)
             (let ([ht (exported-index-desc*-extras desc)])
               (or (hash-ref ht 'hidden? #f)
                   (hash-ref ht 'constructor? #f))))))
  (for* ([entry (in-list (xref-index xref))]
         [desc (in-value (entry-desc entry))]
         #:when desc
         #:unless (hide-desc? desc)
         [term (in-value (car (entry-words entry)))]
         [tag (in-value (entry-tag entry))])
    (trie-add! root
               term
               (delay (doc-trie-value desc term tag))))
  root)

(define (doc-trie-value desc term tag)
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
  (define-values (what from fams pkg sort-order)
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
       (define pkg (lib-pkg
                    (match (exported-index-desc-from-libs desc)
                      [(cons lib _) lib]
                      [_            #f])))
       (define sort-order (hash-ref ht 'sort-order 0))
       (values what from fams pkg sort-order)]
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
       (define pkg (lib-pkg
                    (match (hash-ref ht 'module-kind #f)
                      ['lib (string->symbol term)]
                      [_    #f])))
       (define sort-order (hash-ref ht 'sort-order 0))
       (values what from fams pkg sort-order)]
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
       (define pkg (lib-pkg
                    (match (exported-index-desc-from-libs desc)
                      [(cons lib _) lib]
                      [_            #f])))
       (values what from "" pkg 0)]
      [(module-path-index-desc? desc)
       (define pkg (lib-pkg (string->symbol term)))
       (values "module" "" "" pkg 0)]
      [else
       (define pkg (lib-pkg #f))
       (values "documentation" (doc-from) "" pkg 0)]))
  (list term sort-order what from fams pkg path anchor))

;; This is for package-details
(define (module-doc-path mod-path-str lang?)
  (for/or ([v (in-list (doc-search mod-path-str 0))])
    (match-define (list term _sort what _from _fams _pkg path anchor) v)
    (and (equal? term mod-path-str)
         (equal? what (if lang? "language" "module"))
         (cons path anchor))))

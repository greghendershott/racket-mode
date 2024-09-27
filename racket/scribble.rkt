;; Copyright (c) 2013-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/promise
         racket/set
         racket/string
         (only-in scribble/core
                  tag?)
         scribble/blueboxes
         (only-in scribble/manual-struct
                  ;; i.e. Not the newer exported-index-desc*
                  ;; accessors, which we need to dynamic-require below
                  ;; when running on Racket <= 8.14.0, else provide
                  ;; stubs. We can't import them here, too. Note that
                  ;; except-in errors for items that aren't actually
                  ;; exported, so we can't use that here.
                  constructor-index-desc?
                  exported-index-desc?
                  exported-index-desc-name
                  module-path-index-desc?
                  language-index-desc?
                  reader-index-desc?
                  form-index-desc?
                  procedure-index-desc?
                  thing-index-desc?
                  struct-index-desc?
                  class-index-desc?
                  interface-index-desc?
                  mixin-index-desc?
                  method-index-desc?
                  exported-index-desc-from-libs)
         scribble/xref
         scribble/tag
         setup/xref
         syntax/parse/define
         version/utils
         "elisp.rkt")

(provide binding->path+anchor
         identifier->bluebox
         bluebox-command
         doc-index-names
         doc-index-lookup
         libs-exporting-documented
         module-doc-path
         refresh-module-doc-path-index!)

(module+ test
  (require rackunit))

(define xref-promise (delay/thread (load-collections-xref)))

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
    (let* ([xref (force xref-promise)]
           [tag  (xref-binding->definition-tag xref stx 0)]
           [p+a  (and tag (tag->path+anchor xref tag))])
      p+a)))

(define (tag->path+anchor xref tag)
  (define-values (path anchor) (xref-tag->path+anchor xref tag))
  (and path anchor (cons path anchor)))

;;; Blueboxes

(define racket-version->6.12? (version<? "6.12" (version)))

(define bluebox-cache (delay/thread (make-blueboxes-cache #t)))

(define (get-bluebox-string tag)
  (match (and racket-version->6.12?
              (fetch-blueboxes-strs tag
                                    #:blueboxes-cache (force bluebox-cache)))
    [(list* _kind strs)
     (string-replace (string-join strs "\n")
                     "\u00A0"
                     " ")]
    [_ #f]))

(define/contract (identifier->bluebox stx)
  (-> identifier? (or/c #f string?))
  (match (xref-binding->definition-tag (force xref-promise) stx 0)
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
  (when racket-version->6.12?
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

(define ((doc-index-names))
  (with-less-memory-pressure
    (with-parens
      (define xref (force xref-promise))
      (for* ([entry (in-list (xref-index xref))]
             [desc (in-value (entry-desc entry))]
             #:when (not (constructor-index-desc? desc))
             [term (in-value (car (entry-words entry)))])
        (elisp-write term))
      (newline))))

;; Newer exported-index-desc* struct introduced after Racket 8.14.
(define exported-index-desc*?
  (dynamic-require 'scribble/manual-struct 'exported-index-desc*?
                   (λ () (λ (_) #f))))
(define exported-index-desc*-extras
  (dynamic-require 'scribble/manual-struct 'exported-index-desc*-extras
                   (λ () (λ (_) (hasheq)))))

(define (doc-index-lookup str)
  (with-less-memory-pressure
    (define xref (force xref-promise))
    (define results
      (for*/set ([entry (in-list (xref-index xref))]
                 [desc (in-value (entry-desc entry))]
                 #:when (not
                         (or (constructor-index-desc? desc)
                             (and (exported-index-desc*? desc)
                                  (let ([ht (exported-index-desc*-extras desc)])
                                    (for/or ([key (in-list
                                                   (list 'hidden?
                                                         'constructor?))])
                                      (hash-ref ht key #f))))))
                 [term (in-value (car (entry-words entry)))]
                 #:when (string=? str term))
        (define tag (entry-tag entry))
        (define-values (path anchor) (xref-tag->path+anchor xref tag))
        (define-values (what from)
          (cond
            [(module-path-index-desc? desc)
             (values 'module null)]
            [(and (exported-index-desc*? desc)
                  (hash-ref (exported-index-desc*-extras desc) 'kind #f))
             =>
             (λ (kind)
               (values kind
                       (exported-index-desc-from-libs desc)))]
            [(exported-index-desc? desc)
             (define kind
               (match desc
                 [(? language-index-desc?)  'language]
                 [(? reader-index-desc?)    'reader]
                 [(? form-index-desc?)      'syntax]
                 [(? procedure-index-desc?) 'procedure]
                 [(? thing-index-desc?)     'value]
                 [(? struct-index-desc?)    'structure]
                 [(? class-index-desc?)     'class]
                 [(? interface-index-desc?) 'interface]
                 [(? mixin-index-desc?)     'mixin]
                 [(? method-index-desc?)
                  (cond
                    [(method-tag? tag)
                     (define-values (c/i _m) (get-class/interface-and-method tag))
                     (cons 'method c/i)]
                    [else 'method])]
                 [_ ""]))
             (define libs (exported-index-desc-from-libs desc))
             (values kind libs)]
            [else
             (values 'documentation
                     (list
                      (match (reverse (explode-path path))
                        [(list* _ v _) (path->string v)]
                        [_             (~a tag)])))]))
        (list term what from path anchor)))
    (sort (set->list results)
          string<?
          #:cache-keys? #t
          #:key
          (match-lambda
            [(list* _term _what (cons from _) _path _anchor)
             (match (~a from)
               [(and (pregexp "^racket/") v)
                (string-append "0_" v)]
               [(and (pregexp "^typed/racket/") v)
                (string-append "1_" v)]
               [v v])]
            [(cons term _) term]))))

;;; This is for the requires/find command

;; Given some symbol as a string, return the modules providing it,
;; sorted by most likely to be desired.
(define (libs-exporting-documented sym-as-str)
  (with-less-memory-pressure
    (define xref (force xref-promise))
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
   (define xref (force xref-promise))
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

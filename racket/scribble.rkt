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
         setup/xref
         syntax/parse/define
         version/utils
         "elisp.rkt"
         "util.rkt")

;; Fallbacks when new index structs aren't available, before c. Racket
;; 8.14.0.6.
(define-fallbacks scribble/manual-struct
  [(exported-index-desc*? _) #f]
  [(exported-index-desc*-extras _) #hasheq()]
  [(index-desc? _) #f]
  [(index-desc-extras _) #hasheq()])

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

(define racket-newer-than-6.12 (version<? "6.12" (version)))

(define bluebox-cache (delay/thread (make-blueboxes-cache #t)))

(define (get-bluebox-string tag)
  (match (and racket-newer-than-6.12
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

(define (hide-desc? desc)
  ;; Don't show doc for constructors; class doc suffices.
  (or (constructor-index-desc? desc)
      (and (exported-index-desc*? desc)
           (let ([ht (exported-index-desc*-extras desc)])
             (or (hash-ref ht 'hidden? #f)
                 (hash-ref ht 'constructor? #f))))))

(define ((doc-index-names))
  (with-less-memory-pressure
    (with-parens
      (define xref (force xref-promise))
      (for* ([entry (in-list (xref-index xref))]
             [desc (in-value (entry-desc entry))]
             #:unless (hide-desc? desc)
             [term (in-value (car (entry-words entry)))])
        (elisp-write term))
      (newline))))

(define (doc-index-lookup str)
  (with-less-memory-pressure
    (define xref (force xref-promise))
    (define results
      (for*/set ([entry (in-list (xref-index xref))]
                 [term (in-value (car (entry-words entry)))]
                 #:when (string=? str term)
                 [desc (in-value (entry-desc entry))]
                 #:when desc
                 ;;;[_ (in-value (println desc))] ;;; DEBUG
                 #:unless (hide-desc? desc))
        (define tag (entry-tag entry))
        (define (what/method tag)
          (cond
            [(method-tag? tag)
             (define-values (c/i _m) (get-class/interface-and-method tag))
             (format "method of ~a" c/i)]
            [else "method"]))
        (define-values (path anchor) (xref-tag->path+anchor xref tag))
        (define (doc-in)
          (match (reverse (explode-path path))
            [(list* html-file dir _)
             (format "in ~a ~a"
                     (path->string dir)
                     (path->string (path-replace-extension html-file
                                                           #"")))]
            [_
             (format "tag ~a") tag]))
        (define-values (what from fams)
          (cond
            ;; New structs
            [(exported-index-desc*? desc)
             (define ht (exported-index-desc*-extras desc))
             (define kind (hash-ref ht 'kind))
             (define what (if (string=? kind "method")
                              (what/method tag)
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
             (values what from fams)]
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
                 [#f (doc-in)]))
             (define fams (match (hash-ref ht 'language-family #f)
                            [(? list? fams) (string-join (map ~a fams) ", ")]
                            [#f "Racket"]))
             (values what from fams)]
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
                 [(? method-index-desc?)    (what/method tag)]
                 [_ ""]))
             (define from (string-join (map ~s (exported-index-desc-from-libs desc)) ", "))
             (values what from "")]
            [(module-path-index-desc? desc)
             (values "module" "" "")]
            [else
             (values "documentation" (doc-in) "")]))
        (list term what from fams path anchor)))
    (sort (set->list results)
          string<?
          #:cache-keys? #t
          #:key
          (match-lambda
            [(list* _term _what from _fams _path _anchor)
             (match from
               ;; sort things from some distinguished libs first
               [(and (pregexp "^racket/") v)
                (string-append " 0_" v)]
               [(and (pregexp "^typed/racket/") v)
                (string-append " 1_" v)]
               [(and (pregexp "^rhombus") v)
                (string-append " 2_" v)]
               ;; not from any libs: sort last
               [""
                (make-string 128 #\z)]
               [v v])]
            [(cons term _) term]))))

(module+ test
  (define older?
    (not (dynamic-require 'scribble/manual-struct 'index-desc? (λ () #f))))
  (let ([results (doc-index-lookup "match")])
    (check-true (for/or ([v (in-list results)])
                  (match v
                    [(list "match" "syntax" "racket/match, racket" family
                           _path _anchor)
                     (equal? family (if older? "" "Racket"))]
                    [_ #f]))
                (format "~v" results))
    (when (rhombus-installed?)
      (check-true (for/or ([v (in-list results)])
                    (match v
                      [(list "match" kind "rhombus" family
                             _path _anchor)
                       (and (equal? family (if older? "" "Rhombus"))
                            (equal? kind (if older? "value" "expression")))]
                      [_ #f]))
                  (format "~v" results))))
  (let ([results (doc-index-lookup "set-label")])
    (check-true (for/or ([v (in-list results)])
                  (match v
                    [(list "set-label" "method of message%" "racket/gui/base, racket/gui" family
                           _path _anchor)
                     (equal? family (if older? "" "Racket"))]
                    [_ #f]))
                (format "~v" results)))
  (let ([results (doc-index-lookup "print")])
    (check-true (for/or ([v (in-list results)])
                  (match v
                    [(list "print" "procedure" "racket/base, racket" family
                           _path _anchor)
                     (equal? family (if older? "" "Racket"))]
                    [_ #f]))
                (format "~v" results))
    (when (rhombus-installed?)
      (check-true (for/or ([v (in-list results)])
                    (match v
                      [(list "print" kind libs family
                             _path _anchor)
                       (and (equal? libs (if older? "(lib rhombus/rx.rhm)" "rhombus/rx"))
                            (equal? family (if older? "" "Rhombus"))
                            (equal? kind (if older? "value" "regexp charset operator")))]
                      [_ #f]))
                  (format "~v" results)))))

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

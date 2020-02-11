#lang at-exp racket/base

(require (only-in scribble/core tag?)
         scribble/xref
         setup/dirs
         setup/xref
         net/uri-codec
         racket/contract
         racket/format
         racket/match
        "../identifier.rkt")

(provide doc)

;; Once upon a time, you could enter commands in the REPL like ",doc".
;; It made sense to open the browser here -- despite needing junk to
;; convince macOS to open a file: URL using anchors a.k.a. fragments.
;;
;; But nowadays? Just return the URL. Let Emacs open the browser.
;; Especially because now check-syntax sometimes returns a help URL,
;; in which case the front end should just open the browser. Given
;; that case, let's always open the browser one consistent way -- in
;; Emacs using browse-url.

;; We are lazy-required so `delay`-ing would be N/A.
(define xref (load-collections-xref))

(define/contract (doc how str)
  (-> how/c string? string?)
  (or (->identifier how str stx->uri-string)
      (search str)))

(define (stx->uri-string stx)
  (~a "file://"
      (match (and xref (xref-binding->definition-tag xref stx 0))
        [(? tag? tag)
         (define-values (path anchor) (xref-tag->path+anchor xref tag))
         (~a path "#" anchor)]
        [_ (search (~a (syntax->datum stx)))])))

(define (search str)
  (~a (for/or ([f (in-list (list find-user-doc-dir find-doc-dir))])
        (search-dir f))
      "?q="
      (uri-encode str)))

(define (search-dir f)
  (match (f)
    [(? path? dir) (define path (build-path dir "search/index.html"))
                   (and (file-exists? path) (path->string path))]
    [_ #f]))

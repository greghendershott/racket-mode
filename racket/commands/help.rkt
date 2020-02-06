#lang at-exp racket/base

(require scribble/xref
         (only-in scribble/core tag?)
         setup/dirs
         setup/xref
         net/uri-codec
         racket/contract
         racket/format
         racket/match
         "../identifier.rkt")

(provide doc)

;; Once upon a time, you could enter commands in the REPL like ",doc"
;; and it used to make sense to open the browser, here, and have a lot
;; of crap to do so on macOS to handle anchors a.k.a. fragments.
;;
;; But nowadays just return the URL and let Emacs open the browser.
;; Especially because now check-syntax sometimes returns path + anchor
;; and we need to use that approach. Better to open the browser just
;; one way.

(define/contract (doc how str)
  (-> how/c string? string?)
  (->identifier how str stx->uri-string))

(define (stx->uri-string stx)
  (define xref (load-collections-xref))
  (~a "file://"
      (match (and xref (xref-binding->definition-tag xref stx 0))
        [(? tag? tag)
         (define-values (path anchor) (xref-tag->path+anchor xref tag))
         (~a path "#" anchor)]
        [_
         (~a (for/or ([f (in-list (list find-user-doc-dir find-doc-dir))])
               (search-dir f))
             "?q="
             (uri-encode (~a (syntax->datum stx))))])))

(define (search-dir f)
  (match (f)
    [#f #f]
    [dir (define path (build-path dir "search/index.html"))
         (and (file-exists? path) (path->string path))]))

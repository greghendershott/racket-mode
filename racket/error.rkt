;; Copyright (c) 2013-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (only-in pkg/db
                  get-catalogs)
         (only-in pkg/lib
                  pkg-catalog-suggestions-for-module
                  pkg-directory)
         racket/format
         racket/match
         setup/dirs
         "instrument.rkt"
         "stack-checkpoint.rkt"
         "util.rkt")

(provide racket-mode-error-display-handler
         prevent-path-elision-by-srcloc->string)

(module+ test
  (require rackunit))

(define ((racket-mode-error-display-handler output) msg v)
  (output
    (cond
      [(with-handlers ([values (λ _ #f)])
         ((dynamic-require 'rackunit 'exn:test:check?) v))
       (list 'exn:test:check msg)]
      [(exn? v)
       (define (fix-paths msg)
         (complete-paths
          (undo-path->relative-string/library msg)))
       (list 'exn
             (fix-paths msg)
             (if (member (exn-message v) (list "" msg))
                 ""
                 (fix-paths (exn-message v)))
             (list 'srclocs
                   (if (exn:srclocs? exn)
                       (map srcloc->elisp-value
                            ((exn:srclocs-accessor exn) exn))
                       null))
             (list 'context
                   (if (or (exn:fail:syntax? v)
                               (and (exn:fail:read? v) (not (exn:fail:read:eof? v)))
                               (exn:fail:user? v))
                       null
                       (context v))))]
      [else
       (list 'non-exn msg)])))

;;; srclocs

(define (srcloc->elisp-value x)
  (define src
    ;; Although I want to find/fix this properly upstream -- is
    ;; something a path-string? when it should be a path? -- for now
    ;; just catch here the case where the source is a string like
    ;; "\"/path/to/file.rkt\"" i.e. the string value has quotes.
    (match (srcloc-source x)
      [(pregexp "^\"(.+)\"$" (list _ unquoted)) unquoted]
      [(? path? v) (path->string v)]
      [v v]))
  (define line (or (srcloc-line x)   1))
  (define col  (or (srcloc-column x) 0))
  (list src line col (srcloc-position x) (srcloc-span x)))

;;; context

(define (context e)
  (define-values (kind pairs)
    (cond [(instrumenting-enabled)
           (values 'errortrace
                 (get-error-trace e))]
          [else
           (values 'plain
                   (for/list ([_ (error-print-context-length)]
                              [v (continuation-mark-set->trimmed-context
                                  (exn-continuation-marks e))])
                     v))]))
  (cons kind
        (for/list ([v (in-list pairs)])
          (match-define (cons expr src) v)
          (cons (~a expr)
                (and src (srcloc->elisp-value src))))))

;;; Complete pathnames for Emacs

;; The background here is that want source locations in error messages
;; to use complete pathnames ("complete" as in complete-path? a.k.a.
;; "absolute" plus drive letter on Windows). That way, Emacs features
;; like compilation-mode's next-error command will work nicely.
;;
;; - When we create strings from srclocs, ourselves: We create them
;;   that way. See source-location->string defined/used in this file.
;;
;; - When other things create strings from scrlocs: We try to prevent
;;   them from eliding in the first place. And since we can't always
;;   prevent, we try to undo any elision baked into the error message
;;   by the time we get it. As a sanity check, we don't transform
;;   things into complete pathnames unless the result actually exists.

;; srcloc->string from racket/base uses current-directory-for-user to
;; elide paths. Setting that to 'pref-dir -- where it is very unlikely
;; a user's source file will be -- should prevent it from eliding
;; anything.
(define (prevent-path-elision-by-srcloc->string)
  (current-directory-for-user (find-system-path 'pref-dir)))

;; The source-location->string function provided by syntax/srcloc uses
;; path->relative-string/library to elide paths with prefixes like
;; <pkgs>/ or <collects>/. We avoid using that function in this
;; module, for example in display-srclocs and in context-item->string
;; above. However things like racket/contract use syntax/srcloc and
;; those prefixes might be baked into exn-message. Here we try to undo
;; this for things that look like such source locations.
(define (undo-path->relative-string/library s)
  (regexp-replace*
   #px"(<(.+?)>/(.+?)):(\\d+[:.]\\d+)"
   s
   (λ (_ prefix+rel-path prefix rel-path line+col)
     (define (f dir [rel rel-path])
       (existing (simplify-path (build-path dir rel))))
     (~a (or (and (path-string? rel-path)
                  (match prefix
                    ["collects" (f (find-collects-dir))]
                    ["user"     (f (find-user-collects-dir))]
                    ["doc"      (f (find-doc-dir))]
                    ["user-doc" (f (find-user-doc-dir))]
                    ["pkgs"     (match rel-path
                                  [(pregexp "^(.+?)/(.+?)$" (list _ pkg-name more))
                                   (f (pkg-directory pkg-name) more)]
                                  [_ #f])]
                    [_          #f]))
             prefix+rel-path) ;keep as-is
         ":" line+col))))

(module+ test
  (check-equal? (undo-path->relative-string/library "<collects>/racket/file.rkt:1:0:")
                (~a (build-path (find-collects-dir) "racket" "file.rkt") ":1:0:"))
  (check-equal? (undo-path->relative-string/library "<doc>/2d/index.html:1:0:")
                (~a (build-path (find-doc-dir) "2d" "index.html") ":1:0:"))
  ;; Note: No test for <user-doc> because unlikely to work on Travis CI
  (let ([non-existing "<collects>/racket/does-not-exist.rkt:1:0 blah blah blah"])
   (check-equal? (undo-path->relative-string/library non-existing)
                 non-existing
                 "does not change to non-existing pathname")))

(module+ test
  (let ()
    (local-require racket/path
                   setup/path-to-relative)
    (define-polyfill (alternate-racket-clcl/clcp path box)
      #:module drracket/find-module-path-completions
      (values null null null))
    (define-values (_links _paths pkg-dirs)
      (alternate-racket-clcl/clcp (find-system-path 'exec-file) (box #f)))
    (printf "Checking .rkt files in ~v packages...\n" (length pkg-dirs))
    (define c (make-hash))
    (for ([item (in-list pkg-dirs)])
      (match item
        [(list (? string?) (? path? dir))
         (for ([p (in-directory dir)]
               #:when (equal? #".rkt" (path-get-extension p)))
           (define complete (~a p                                           ":1.0"))
           (define relative (~a (path->relative-string/library p #:cache c) ":1.0"))
           (define undone (undo-path->relative-string/library relative))
           (check-equal? undone complete))]
        [_ (void)]))))

;; If this looks like a source location where the pathname is
;; relative, prepend current-directory if that results in an actually
;; existing file.
(define (complete-paths s)
  (regexp-replace*
   #px"([^:]+):(\\d+[:.]\\d+)"
   s
   (λ (_ orig-path line+col)
     (~a (or (and (relative-path? orig-path)
                  (existing (build-path (current-directory) orig-path)))
             orig-path)
         ":" line+col))))

(define (existing p)
  (and (path? p) (file-exists? p) p))

(module+ test
  (let ()
    (local-require racket/file
                   racket/path)
    (define temp-dir (find-system-path 'temp-dir))
    (define example (make-temporary-file "racket-mode-test-~a" #f temp-dir))
    (define name (file-name-from-path example))
    (parameterize ([current-directory temp-dir])
      (let ([suffix ":3:0: f: unbound identifier\n   in: f"])
        (check-equal? (complete-paths (~a name suffix))
                      (~a (build-path temp-dir name) suffix)
                      "relative path: curdir prepended when that is an existing file"))
      (let ([msg (~a example ":3:0: f: unbound identifier\n   in: f")])
        (check-equal? (complete-paths msg)
                      msg
                      "already complete path: no change")))
    (delete-file example)))


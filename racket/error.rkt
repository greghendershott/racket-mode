#lang at-exp racket/base

(require (only-in pkg/db
                  get-catalogs)
         (only-in pkg/lib
                  pkg-catalog-suggestions-for-module
                  pkg-directory)
         racket/format
         racket/match
         racket/string
         setup/dirs
         "instrument.rkt"
         "stack-checkpoint.rkt"
         "util.rkt")

(provide display-exn
         racket-mode-error-display-handler
         prevent-path-elision-by-srcloc->string)

(module+ test
  (require rackunit))

(define (display-exn exn)
  (racket-mode-error-display-handler (exn-message exn) exn))

(define (racket-mode-error-display-handler str v)
  (cond [(exn? v)
         (unless (equal? "Check failure" (exn-message v)) ;rackunit check fails
           (display-commented (complete-paths
                               (undo-path->relative-string/library str)))
           (display-srclocs v)
           (unless (exn:fail:user? v)
             (display-context v))
           (maybe-suggest-packages v))]
        [else
         (display-commented str)]))

;;; srclocs

(define (display-srclocs exn)
  (when (exn:srclocs? exn)
    (define srclocs
      (match ((exn:srclocs-accessor exn) exn)
        ;; Some exceptions like exn:fail:read? include the first
        ;; srcloc in exn-message -- don't show it again.
        [(cons _ xs)
         #:when (or (exn:fail:read? exn)
                    (exn:fail:contract:variable? exn))
         xs]
        ;; Some exceptions like exn:fail:syntax? with Typed Racket
        ;; include _all_ in exn-message -- don't show _any_.
        [_
         #:when (exn:fail:syntax? exn)
         '()]
        [xs xs]))
    (for ([s (in-list srclocs)])
      (display-commented (source-location->string s)))))

(module+ test
  (let ([o (open-output-string)])
    (parameterize ([current-error-port o])
      (display-srclocs (make-exn:fail:read "..."
                                           (current-continuation-marks)
                                           '())))
    (check-equal? (get-output-string o) "")))

;; We don't use source-location->string from syntax/srcloc, because we
;; don't want the setup/path-to-relative behavior that elides complete
;; pathnames with prefixes like "<pkgs>/" etc. For strings we create
;; ourselves, we use our own such function, defined here.
(define (source-location->string x)
  (define src
    ;; Although I want to find/fix this properly upstream -- is
    ;; something a path-string? when it should be a path? -- for now
    ;; just catch here the case where the source is a string like
    ;; "\"/path/to/file.rkt\"" i.e. the string value has quotes.
    (match (srcloc-source x)
      [(pregexp "^\"(.+)\"$" (list _ unquoted)) unquoted]
      [v v]))
  (define line (or (srcloc-line x)   1))
  (define col  (or (srcloc-column x) 0))
  (format "~a:~a:~a" src line col))

;;; context

(define (display-context exn)
  (cond [(instrumenting-enabled)
         (define p (open-output-string))
         (print-error-trace p exn)
         (match (get-output-string p)
           ["" (void)]
           [s  (display-commented (~a "Context (errortrace):" s))])]
        [else
         (match (context->string
                 (continuation-mark-set->trimmed-context
                  (exn-continuation-marks exn)))
           ["" (void)]
           [s (display-commented
               (~a "Context (plain; to see better errortrace context, re-run with C-u prefix):\n"
                   s))])]))

(define (context->string xs)
  (string-join (for/list ([x xs]
                          [_ (error-print-context-length)])
                 (context-item->string x))
               "\n"))

(define (context-item->string ci)
  (match-define (cons id srcloc) ci)
  (~a (if (or srcloc id) "  " "")
      (if srcloc (source-location->string srcloc) "")
      (if (and srcloc id) " " "")
      (if id (format "~a" id) "")))

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
                (~a (build-path (find-collects-dir) "racket/file.rkt") ":1:0:"))
  (check-equal? (undo-path->relative-string/library "<doc>/2d/index.html:1:0:")
                (~a (build-path (find-doc-dir) "2d/index.html") ":1:0:"))
  ;; Note: No test for <user-doc> because unlikely to work on Travis CI
  (let ([non-existing "<collects>/racket/does-not-exist.rkt:1:0 blah blah blah"])
   (check-equal? (undo-path->relative-string/library non-existing)
                 non-existing
                 "does not change to non-existing pathname")))

(module+ test
  (let ()
    (local-require racket/path
                   setup/path-to-relative
                   drracket/find-module-path-completions)
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

;;; packages

(define (maybe-suggest-packages exn)
  (when (exn:missing-module? exn)
    (match (get-catalogs)
      [(list)
       (display-commented
        @~a{-----
            Can't suggest packages to install, because pkg/db get-catalogs is '().
            To configure:
            1. Start DrRacket.
            2. Choose "File | Package Manager".
            3. Click "Available from Catalog".
            4. When prompted, click "Update".
            -----})]
      [_
       (define mod ((exn:missing-module-accessor exn) exn))
       (match (pkg-catalog-suggestions-for-module mod)
         [(list) void]
         [(list p)
          (display-commented
           @~a{Try "raco pkg install @|p|" ?})]
         [(? list? ps)
          (display-commented
           @~a{Try "raco pkg install" one of @(string-join ps ", ") ?})]
         [_ void])])))

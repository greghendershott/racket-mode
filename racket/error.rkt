#lang at-exp racket/base

(require (only-in pkg/db get-catalogs)
         (only-in pkg/lib pkg-catalog-suggestions-for-module)
         racket/format
         racket/match
         racket/string
         "fresh-line.rkt"
         "instrument.rkt"
         "stack-checkpoint.rkt"
         "util.rkt")

(provide display-exn
         our-error-display-handler
         show-full-path-in-errors)

(module+ test
  (require rackunit))

(define (display-exn exn)
  (our-error-display-handler (exn-message exn) exn))

(define (our-error-display-handler str v)
  (cond [(exn? v)
         (unless (equal? "Check failure" (exn-message v)) ;rackunit check fails
           (fresh-line)
           (display-commented (fully-qualify-error-path str))
           (display-srclocs v)
           (unless (exn:fail:user? v)
             (display-context v))
           (maybe-suggest-packages v))]
        [else
         (fresh-line)
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

;; Don't use source-location->string from syntax/srcloc. Don't want
;; the setup/path-to-relative behavior that replaces full pathnames
;; with <collects>, <pkgs> etc. Instead want full pathnames for Emacs'
;; compilation-mode. HOWEVER note that <collects> or <pkgs> might be
;; baked into exn-message string already; we handle that in
;; `fully-qualify-error-path`. Here we handle only strings we create
;; ourselves, such as for the Context "stack trace".
(define (source-location->string x)
  (define src
    ;; Although I want to find/fix this properly upstream -- is
    ;; something a path-string? when it should be a path? -- for now
    ;; just catch here the case where the source is a string like
    ;; "\"/path/to/file.rkt\"" i.e. in quotes.
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
  ;; Limit the context in two ways:
  ;; 1. Don't go beyond error-print-context-length
  ;; 2. Don't go into "system" context that's just noisy.
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

;;; Fully-qualified pathnames for Emacs compilation-mode

;; srcloc->string uses current-directory-for-user to shorten error
;; messages. But we always want full pathnames. Setting it to
;; 'pref-dir -- very unlikely user .rkt file will be there -- is
;; least-worst way AFAIK.
(define (show-full-path-in-errors)
  (current-directory-for-user (find-system-path 'pref-dir)))

;; If this looks like a Racket error message, but the filename is
;; not fully-qualified, prepend curdir to the filename.
;;
;; This covers Racket 5.3.6 and earlier. In fact, this might be
;; sufficient for _all_ versions of Racket and we don't need the
;; `show-full-path-in-errors` thing above, at all. Not yet sure.
(define (fully-qualify-error-path s)
  (match s
    [(pregexp "^([^:]+):(\\d+)[:.](\\d+)(.*)$"
              (list _ path line col more))
     #:when (not (absolute-path? path))
     (~a (string-join (list (path->string
                             (build-path (current-directory) path))
                            line
                            col)
                      ":")
         more)]
    [s s]))

(module+ test
  (require rackunit)
  (case (system-type 'os)
    [(windows)
     (check-equal?
      (parameterize ([current-directory "c:\\tmp"])
        (fully-qualify-error-path "foo.rkt:3:0: f: unbound identifier\n   in: f"))
      "c:\\tmp\\foo.rkt:3:0: f: unbound identifier\n   in: f")
     (check-equal?
      (fully-qualify-error-path "c:\\tmp\\foo.rkt:3:0: f: unbound identifier\n   in: f")
      "c:\\tmp\\foo.rkt:3:0: f: unbound identifier\n   in: f")]
    [(macosx unix)
     (check-equal?
      (parameterize ([current-directory "/tmp/"])
        (fully-qualify-error-path "foo.rkt:3:0: f: unbound identifier\n   in: f"))
      "/tmp/foo.rkt:3:0: f: unbound identifier\n   in: f")
     (check-equal?
      (fully-qualify-error-path "/tmp/foo.rkt:3:0: f: unbound identifier\n   in: f")
      "/tmp/foo.rkt:3:0: f: unbound identifier\n   in: f")])
  (let ([o (open-output-string)])
    (parameterize ([current-error-port o])
      (display-srclocs (make-exn:fail:read "..."
                                           (current-continuation-marks)
                                           '())))
    (check-equal? (get-output-string o) "")))

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

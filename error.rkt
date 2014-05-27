#lang racket/base

(require racket/match
         racket/runtime-path
         racket/string
         racket/path
         racket/list
         "util.rkt")

(provide display-exn
         our-error-display-handler
         show-full-path-in-errors)

(module+ test
  (require rackunit))

(define (display-exn exn)
  (our-error-display-handler (exn-message exn) exn))

(define (our-error-display-handler str exn)
  (when (exn? exn)
    (unless (equal? "Check failure" (exn-message exn)) ;rackunit check fails
      (display-commented (fully-qualify-error-path str))
      (display-srclocs exn)
      (unless (exn:fail:user? exn)
        (display-context exn)))))

(define (display-srclocs exn)
  (when (exn:srclocs? exn)
    (let* ([srclocs ((exn:srclocs-accessor exn) exn)]
           [srclocs (cond [(or (exn:fail:read? exn)
                               (exn:fail:contract:variable? exn))
                           (cdr srclocs)] ;1st one already in exn-message
                          [(exn:fail:syntax? exn)
                           '()] ;all in exn-message, e.g. Typed Racket
                          [else srclocs])])
      (for ([srcloc srclocs])
        (display-commented (source-location->string srcloc))))))

(define (display-context exn)
  (match (context->string
          (continuation-mark-set->context (exn-continuation-marks exn)))
    ["" (void)]
    [s (display-commented "Context:")
       (display-commented s)]))

(define (context->string xs)
  ;; Limit the context in two ways:
  ;; 1. Don't go beyond error-print-context-length
  ;; 2. Don't go into "system" context that's just noisy.
  ;; Also, show the context in reverse, for Emacs compilation-mode.
  (string-join (reverse (for/list ([x xs]
                                   [_ (error-print-context-length)]
                                   #:unless (system-context? x))
                          (context-item->string x)))
               "\n"))

(define-runtime-path sandbox.rkt "sandbox.rkt")
(define (system-context? ci)
  (match-define (cons id src) ci)
  (or (not src)
      (let ([src (srcloc-source src)])
        (and (path? src)
             (or (equal? src sandbox.rkt)
                 (under-system-path? src))))))

(define excluded-collection-paths
  (for/list ([x '(["typed" "racket.rkt"]
                  ["typed-racket" "core.rkt"]
                  ["racket/contract" "base.rkt"]
                  ["racket/private" "base.rkt"])])
    (match-define (list coll file) x)
    (apply build-path
           (drop-right (explode-path (collection-file-path file coll))
                       1))))

(define (under-system-path? path)
  (define-values (dir base _) (split-path path))
  (for/or ([cp (in-list excluded-collection-paths)])
    (under? path cp)))

(define (under? path parent)
  (define as (explode-path (simplify-path path)))
  (define bs (explode-path (simplify-path parent)))
  (and (> (length as) (length bs))
       (for/and ([a as]
                 [b bs])
         (equal? a b))))

(module+ test
  (check-true  (under? "/a/b/c/d/e" "/a/b"))
  (check-false (under? "/a/b/c/d/e" "/x/y")))

(define (context-item->string ci)
  (match-define (cons id src) ci)
  (string-append (if (or src id) " " "")
                 (if src (source-location->string src) "")
                 (if (and src id) " " "")
                 (if id (format "~a" id) "")))

;; Don't use Racket's source-location->string. Don't want the
;; setup/path-to-relative behavior that replaces full pathnames with
;; <collects>, <pkgs> etc. Want full pathnames for Emacs'
;; compilation-mode.
(define (source-location->string x)
  (match-define (srcloc src line col pos span) x)
  (format "~a:~a:~a" src line col))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fully qualified pathnames in error messages, so that Emacs
;; compilation-mode can do its stuff.

;; srcloc->string uses current-directory-for-user to shorten error
;; messages. But we always want full pathnames. Setting it to
;; 'pref-dir -- very unlikely user .rkt file will be there -- is
;; least-worst way AFAIK.
;;
;; Wrinkle: current-directory-for-user was added after Racket 5.3.6,
;; so need to use dynamic-require here.
(define (show-full-path-in-errors)
  (with-handlers ([exn:fail? (lambda _ (void))])
    ((dynamic-require 'racket/base 'current-directory-for-user)
     (find-system-path 'pref-dir))))

;; If this looks like a Racket error message, but the filename is
;; not fully-qualified, prepend curdir to the filename.
;;
;; This covers Racket 5.3.6 and earlier. In fact, this might be
;; sufficient for _all_ versions of Racket and we don't need the
;; `show-full-path-in-errors` thing above, at all. Not yet sure.
(define (fully-qualify-error-path s)
  (match s
    [(pregexp "^([^/.]+)\\.([^.]+):(\\d+)[:.](\\d+):(.*)$"
              (list _ base ext line col more))
     (define curdir (path->string (current-directory)))
     (string-append curdir base "." ext ":" line ":" col ":" more)]
    [_ s]))

(module+ test
  (require rackunit)
  (check-equal?
   (parameterize ([current-directory "/tmp/"])
     (fully-qualify-error-path "foo.rkt:3:0: f: unbound identifier\n   in: f"))
   "/tmp/foo.rkt:3:0: f: unbound identifier\n   in: f")
  (check-equal?
   (fully-qualify-error-path "/tmp/foo.rkt:3:0: f: unbound identifier\n   in: f")
   "/tmp/foo.rkt:3:0: f: unbound identifier\n   in: f"))

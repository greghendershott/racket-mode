#lang racket/base

(require racket/match
         racket/runtime-path
         racket/string
         syntax/srcloc
         "util.rkt")

(provide our-error-display-handler
         display-exn)

(define (our-error-display-handler str exn)
  (when (exn? exn)
    (unless (equal? "Check failure" (exn-message exn)) ;rackunit check fails
      (display-commented str)
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
  (define ctx (continuation-mark-set->context (exn-continuation-marks exn)))
  (match (context->string ctx)
    ["" (void)]
    [s (display-commented "Context:")
       (display-commented s)]))

(define (context->string xs)
  (string-join (for/list ([x xs]
                          [n 10]
                          #:break (system-context? x))
                 (context-item->string x))
               "\n"))

(define-runtime-path sandbox.rkt "sandbox.rkt")
(define (system-context? ci)
  (match-define (cons id src) ci)
  (or (not src)
      (let ([src (srcloc-source src)])
        (and (path? src)
             (or (equal? src sandbox.rkt)
                 (under-system-path? src))))))

(define (under-system-path? path)
  (define excluded-collections
    '("typed/racket" "racket/contract" "racket/private"))
  (define-values (dir base _) (split-path path))
  (not (not (for/or ([collection (in-list excluded-collections)])
              (collection-file-path base collection #:fail (lambda _ #f))))))

(module+ test
  (require rackunit)
  (check-true (under-system-path?
   (string->path "/Applications/Racket_v5.93/share/pkgs/typed-racket-lib/typed-racket/tc-setup.rkt"))))

(define (context-item->string ci)
  (match-define (cons id src) ci)
  (string-append (if (or src id) " " "")
                 (if src (source-location->string src) "")
                 (if (and src id) " " "")
                 (if id (format "~a" id) "")))

(define (display-exn exn)
  (our-error-display-handler (exn-message exn) exn))

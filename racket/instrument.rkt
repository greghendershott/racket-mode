;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require (only-in errortrace/errortrace-key
                  errortrace-key)
         (only-in errortrace/errortrace-lib
                  error-context-display-depth)
         errortrace/stacktrace
         racket/match
         racket/unit)

(provide make-instrumented-eval-handler
         error-context-display-depth
         get-error-trace
         instrumenting-enabled
         test-coverage-enabled
         clear-test-coverage-info!
         get-uncovered
         profiling-enabled
         clear-profile-info!
         get-profile)

;;; Core instrumenting

(define instrumenting-enabled (make-parameter #f))

(define (make-instrumented-eval-handler [orig-eval (current-eval)])
  ;; This is modeled after the one in DrRacket.
  (define (racket-mode-instrumented-eval-handler orig-exp)
    (cond
      [(or (not (instrumenting-enabled))
           (compiled-expression? (if (syntax? orig-exp)
                                     (syntax-e orig-exp)
                                     orig-exp)))
       (orig-eval orig-exp)]
      [else
       (let loop ([exp (if (syntax? orig-exp)
                           orig-exp
                           (namespace-syntax-introduce
                            (datum->syntax #f orig-exp)))])
         (let ([top-e (expand-syntax-to-top-form exp)])
           (syntax-case top-e (begin)
             [(begin expr ...)
              ;; Found a `begin', so expand/eval each contained
              ;; expression one at a time
              (let i-loop ([exprs (syntax->list #'(expr ...))]
                           [last-one (list (void))])
                (cond
                  [(null? exprs)
                   (apply values last-one)]
                  [else
                   (i-loop (cdr exprs)
                           (call-with-values
                            (λ ()
                              (call-with-continuation-prompt
                               (λ () (loop (car exprs)))
                               (default-continuation-prompt-tag)
                               (λ args
                                 (apply
                                  abort-current-continuation
                                  (default-continuation-prompt-tag)
                                  args))))
                            list))]))]
             [_else
              ;; Not `begin', so proceed with normal expand and eval
              (orig-eval (errortrace-annotate top-e #f))])))]))
  racket-mode-instrumented-eval-handler)

;;; Better stack traces ("basic errortrace")

(define (should-annotate? stx phase) ;stacktrace-filter^
  (and (syntax-source stx)
       (syntax-property stx 'errortrace:annotate)))

(define key-module-name 'errortrace/errortrace-key) ;key-module-name^

(define (with-mark mark expr phase) ;stracktrace-imports^
  ;; This is modeled after the one in errortrace-lib. Specifically,
  ;; use `make-st-mark' for its capture of the original syntax to show
  ;; in the stack trace error message.
  (match (make-st-mark mark phase)
    [#f expr]
    [mark
     (with-syntax ([expr expr]
                   [mark mark]
                   [etk errortrace-key]
                   [wcm (syntax-shift-phase-level #'with-continuation-mark phase)])
       (syntax (wcm etk mark expr)))]))

;; Functional alternative to print-error-trace.
(define (get-error-trace e)
  (for/list ([_ (error-context-display-depth)]
             [stx (in-list
                   (map st-mark-source
                        (continuation-mark-set->list (exn-continuation-marks e)
                                                     errortrace-key)))]
             #:when stx)
    (cons (syntax->datum stx)
          (srcloc (syntax-source stx)
                  (syntax-line stx)
                  (syntax-column stx)
                  (syntax-position stx)
                  (syntax-span stx)))))

;;; Test coverage

(define test-coverage-enabled (make-parameter #f)) ;stacktrace/annotator-imports^

(define test-coverage-info (make-hasheq)) ;(hash/c syntax? mpair?).
;; This approach taken from DrR. Presumably set-mcar! is faster than a
;; box, which in turn is faster than hash-set!. The mcdr cell is
;; ignored.

(define (clear-test-coverage-info!)
  (hash-clear! test-coverage-info))

(define (test-coverage-point body expr phase) ;stacktrace/annotator-imports^
  (cond
    [(and (test-coverage-enabled)
          (zero? phase)
          (should-annotate? expr phase))
     ;; record as point that might get executed
     (define v (mcons #f #f))
     (hash-set! test-coverage-info expr v)
     (define update-coverage #`(#%plain-app set-mcar! #,v #t))
     (syntax-case expr (#%plain-module-begin)
       [(_mod _name _init-import (#%plain-module-begin . _body))
        (drop-in-sequence body '(tl tl tl hd tl) update-coverage)]
       [_else
        #`(begin #,update-coverage #,body)])]
    [else body]))

(define (drop-in-sequence stx path to-add)
  (let loop ([stx stx]
             [path path])
    (cond
      [(null? path)
       (cons to-add stx)]
      [(syntax? stx)
       (define dstx (syntax-disarm stx #f))
       (syntax-rearm (datum->syntax dstx
                                    (loop (syntax-e dstx) path)
                                    dstx
                                    dstx)
                     stx)]
      [(pair? stx)
       (case (car path)
         [(hd) (cons (loop (car stx) (cdr path)) (cdr stx))]
         [(tl) (cons (car stx) (loop (cdr stx) (cdr path)))])])))

(define (get-uncovered source)
  (parameterize ([error-print-width 65])
    (for/list ([stx (in-list (get-uncovered-expressions source))]
               [n (in-naturals 1)])
      (list (format "~a: ~.a"
                    n
                    (syntax->datum stx))
            source
            (syntax-line stx)
            (syntax-column stx)
            (syntax-position stx)
            (syntax-span stx)))))

;; Unlike the example in sandbox-lib, this eliminates subset syntaxes
;; (not just exact duplicates).
(define (get-uncovered-expressions source)
  (define (earlier/wider? stx1 stx2)
    (let ([p1 (syntax-position stx1)]
          [p2 (syntax-position stx2)])
      (or (< p1 p2) ; earlier first
          (and (= p1 p2)
               (> (syntax-span stx1) ; wider first
                  (syntax-span stx2))))))
  (define unsorted
    (for*/list ([(stx mpair) (in-hash test-coverage-info)]
                #:when (and (not (mcar mpair)) ;not covered
                            (syntax-position stx)
                            (equal? (syntax-source stx) source)))
      stx))
  (define sorted (sort unsorted earlier/wider?))
  (let loop ([xs sorted])
    (match xs
      [(list* this next more)
       (define this-beg (syntax-position this))
       (define next-beg (syntax-position next))
       (define this-end (+ this-beg (syntax-span this)))
       (define next-end (+ next-beg (syntax-span next)))
       (cond
         ;; Omit `next` if only a subset of `this`
         [(and (<= this-beg next-beg)
               (<= next-end this-end))
          (loop (cons this more))]
         [else
          (cons this (loop (cons next more)))])]
      [(list this) (list this)]
      [(list) null])))

;;; Profiling

(define profile-key (gensym)) ;stacktrace/annotator-imports^

(define profiling-enabled (make-parameter #f)) ;stacktrace/annotator-imports^

(define profile-info (make-hasheq)) ;(hash/c any/c prof?)

(define (clear-profile-info!)
  (hash-clear! profile-info))

(struct prof
  (nest? ;guard nested calls
   num   ;exact-nonnegative-integer?
   time  ;exact-nonnegative-integer?
   name  ;(or/c #f symbol?)
   expr) ;syntax?
  #:mutable
  #:transparent)

(define (initialize-profile-point key name expr) ;stacktrace/annotator-imports^
  (hash-set! profile-info
             key
             (prof #f 0 0 (and (syntax? name) (syntax-e name)) expr)))

(define (register-profile-start key) ;stacktrace/annotator-imports^
  (define p (hash-ref profile-info key))
  (set-prof-num! p (add1 (prof-num p)))
  (cond [(prof-nest? p) #f]
        [else (set-prof-nest?! p #t)
              (current-process-milliseconds)]))

(define (register-profile-done key start) ;stacktrace-imports^
  (void
   (when start
     (define p (hash-ref profile-info key))
     (set-prof-nest?! p #f)
     (set-prof-time! p (+ (- (current-process-milliseconds) start)
                          (prof-time p))))))

(define (get-profile)
  (for/list ([x (in-list (hash-values profile-info))])
    (match-define (prof _nest? count msec name stx) x)
    (define src (syntax-source stx))
    (define beg (syntax-position stx))
    (define end (and beg (+ beg (syntax-span stx))))
    (list count
          msec
          (and name (symbol->string name))
          (and src (path? src) (path->string src))
          beg
          end)))


;;; Finally, invoke the unit

(define-values/invoke-unit/infer stacktrace/errortrace-annotate@)

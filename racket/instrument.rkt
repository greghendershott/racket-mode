;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require data/interval-map
         (only-in errortrace/errortrace-key
                  errortrace-key)
         (only-in errortrace/errortrace-lib
                  print-error-trace
                  error-context-display-depth)
         (only-in errortrace/stacktrace
                  stacktrace^
                  stacktrace@
                  stacktrace-imports^
                  original-stx
                  expanded-stx)
         racket/format
         racket/match
         racket/set
         racket/unit
         syntax/parse
         "repl-output.rkt"
         "repl-session.rkt"
         "util.rkt")

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

(define ((make-instrumented-eval-handler [orig-eval (current-eval)]) orig-exp)
  ;; This is modeled after the one in DrRacket.
  (cond
    [(or (not (instrumenting-enabled))
         (compiled-expression? (syntax-or-sexpr->sexpr orig-exp)))
     (orig-eval orig-exp)]
    [else
     (let loop ([exp (syntax-or-sexpr->syntax orig-exp)])
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
            (let* ([expanded-e (expand-syntax top-e)]
                   ;; For make-st-mark to work correctly we need to
                   ;; parameterize original-stx and expanded-stx.
                   [annotated (parameterize ([original-stx top-e]
                                             [expanded-stx expanded-e])
                                (annotate-top expanded-e
                                              (namespace-base-phase)))])
              (warn-about-time-apply expanded-e)
              (orig-eval annotated))])))]))

(define warned-sessions (mutable-set))
(define (warn-about-time-apply stx)
  (syntax-parse stx
    #:datum-literals (#%app time-apply)
    [(#%app time-apply . _)
     (unless (set-member? warned-sessions (current-session-id))
       (set-add! warned-sessions (current-session-id))
       (repl-output-message
        @~a{Warning: time or time-apply used in errortrace annotated code.
            Instead use command-line racket for more-accurate measurements.
            (Will not warn again for this REPL session.)}))
       #t]
    [(ss ...) (for/or ([stx (in-list (syntax->list #'(ss ...)))])
                  (warn-about-time-apply stx))]
    [_ #f]))


;;; Better stack traces ("basic errortrace")

(define base-phase
  (variable-reference->module-base-phase (#%variable-reference)))

(define (with-mark mark expr phase)
  ;; This is modeled after the one in errortrace-lib. Specifically,
  ;; use `make-st-mark' for its capture of the original syntax to show
  ;; in the stack trace error message.
  (match (make-st-mark mark phase)
    [#f  expr]
    [loc (define phase-shift (- phase base-phase))
         (with-syntax ([expr expr]
                       [loc loc]
                       [errortrace-key errortrace-key]
                       [qte (syntax-shift-phase-level #'quote phase-shift)]
                       [wcm (syntax-shift-phase-level #'with-continuation-mark
                                                      phase-shift)])
           (syntax (wcm (qte errortrace-key)
                        loc
                        expr)))]))

;; Functional alternative to print-error-trace.
(define (get-error-trace e)
  (for/list ([_ (error-context-display-depth)]
             [stx (in-list
                   (map st-mark-source
                        (continuation-mark-set->list (exn-continuation-marks e)
                                                     errortrace-key)))])
    (cons (syntax->datum stx)
          (srcloc (syntax-source stx)
                  (syntax-line stx)
                  (syntax-column stx)
                  (syntax-position stx)
                  (syntax-span stx)))))

;;; Test coverage

(define test-coverage-enabled (make-parameter #f)) ;stacktrace-imports^

(define test-coverage-info (make-hasheq)) ;(hash/c syntax? mpair?).
;; This approach taken from DrR. Presumably set-mcar! is faster than a
;; box, which in turn is faster than hash-set!. The cdr cell is
;; ignored.

(define (clear-test-coverage-info!)
  (hash-clear! test-coverage-info))

(define (initialize-test-coverage-point expr) ;stacktrace-imports^
  (hash-set! test-coverage-info expr (mcons #f #f)))

(define (test-covered expr) ;stacktrace-imports^
  (define v (hash-ref test-coverage-info expr #f))
  (and v (with-syntax ([v v])
           #'(#%plain-app set-mcar! v #t))))

(define (get-uncovered source)
  (for/set ([stx (in-list (get-uncovered-expressions source))])
    (define beg (syntax-position stx))
    (define end (+ beg (syntax-span stx)))
    (cons beg end)))

;; from sandbox-lib
(define (get-uncovered-expressions source)
  (let* ([xs (hash-map test-coverage-info
                       (lambda (k v) (cons k (mcar v))))]
         [xs (filter (lambda (x) (and (syntax-position (car x))
                                      (equal? (syntax-source (car x)) source)))
                     xs)]
         [xs (sort xs (lambda (x1 x2)
                        (let ([p1 (syntax-position (car x1))]
                              [p2 (syntax-position (car x2))])
                          (or (< p1 p2) ; earlier first
                              (and (= p1 p2)
                                   (> (syntax-span (car x1)) ; wider first
                                      (syntax-span (car x2))))))))]
         [xs (reverse xs)])
    (if (null? xs)
      xs
      (let loop ([xs (cdr xs)] [r (list (car xs))])
        (if (null? xs)
          (map car (filter (lambda (x) (not (cdr x))) r))
          (loop (cdr xs)
                (cond [(not (and (= (syntax-position (caar xs))
                                    (syntax-position (caar r)))
                                 (= (syntax-span (caar xs))
                                    (syntax-span (caar r)))))
                       (cons (car xs) r)]
                      [(cdar r) r]
                      [else (cons (car xs) (cdr r))])))))))

;;; Profiling

(define profile-key (gensym)) ;stacktrace-imports^

(define profiling-enabled (make-parameter #f)) ;stacktrace-imports^

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

(define (initialize-profile-point key name expr) ;stacktrace-imports^
  (hash-set! profile-info
             key
             (prof #f 0 0 (and (syntax? name) (syntax-e name)) expr)))

(define (register-profile-start key) ;stacktrace-imports^
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
(define-values/invoke-unit/infer stacktrace@)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example

;; (parameterize ([instrumenting-enabled #t]
;;                [test-coverage-enabled #t]
;;                [profiling-enabled #f]
;;                [current-eval (make-instrumented-eval-handler (current-eval))])
;;   (namespace-require (string->path "/tmp/simple.rkt")))
;; (get-test-coverage-info)
;; (get-profile-info)

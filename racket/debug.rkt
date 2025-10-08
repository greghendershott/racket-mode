;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/format
         racket/list
         racket/match
         (only-in racket/path
                  path-only
                  file-name-from-path)
         racket/set
         syntax/modread
         "debug-annotator.rkt"
         "elisp.rkt"
         "repl-output.rkt"
         "util.rkt")

(module+ test
  (require rackunit))

(provide (rename-out [on-break-channel debug-notify-channel])
         debug-resume
         debug-set-local
         debug-disable
         debug-enable-step
         make-debug-eval-handler)

(define-logger racket-mode-debugger)

(define breakable-positions/c (hash/c path? (set/c #:cmp 'eq pos/c)))
(define/contract breakable-positions breakable-positions/c (make-hash))
(define/contract (breakable-position? src pos)
  (-> path? pos/c boolean?)
  (set-member? (hash-ref breakable-positions src (seteq)) pos))

(define/contract (annotate stx)
  (-> syntax? syntax?)
  (define source (syntax-source stx))
  (repl-output-message (format "Debug annotate ~v" source))
  (define-values (annotated breakables)
    (annotate-for-single-stepping stx pause? pause-before pause-after))
  (hash-update! breakable-positions
                source
                (λ (s) (set-union s (list->seteq breakables)))
                (seteq))
  annotated)

;; A struct hierarchy related to the "next" variable.
;;
;; - The "resume" structs are more general, including "over" and "out"
;;   types, which must be reduced to "next" structs.
;;
;; - The "next" structs are more specific, and represent values
;;   actually permitted in the `next` variable.
;;
;; The `caps` field is conditional action points set by the user,
;; represented as (hash/c (cons/c path? nat/c) any/c).

(struct Resume (caps))
(struct Resume:Over Resume ()) ;step over
(struct Resume:Out  Resume ()) ;step out

(struct Next Resume ())
(struct Next:All  Next ()) ;single step, and also `caps`
(struct Next:Some Next ()) ;only `caps`
(struct Next:None Next ()) ;nothing

(define/contract next
  (case-> (-> Next?)
          (-> Next? void))
  (let ([v (Next:None (hash))])
    (case-lambda [() v]
                 [(v!) (set! v v!)])))

;; Following are the functions we give `annotate-for-single-stepping`,
;; calls to which it "weaves into" the annotated code. When it calls
;; `pause?` and we return true, next it calls either `pause-before` or
;; `pause-after`.

(define ((pause? src) pos)
  (match (next)
    [(? Next:None?) #f]
    [(? Next:All?) #t]
    [(Next points) (hash-ref points (cons src pos) #f)]))

(define (pause-before top-mark ccm)
  #;(-> mark/c continuation-mark-set? (or/c #f (listof any/c)))
  (pause 'before top-mark ccm #f))

(define (pause-after top-mark ccm . vals)
  #;(->* (mark/c continuation-mark-set?) #:rest (listof any/c)
         any)
  (apply values (pause 'after top-mark ccm vals)))

(define (pause before/after top-mark ccm vals)
  #;(-> (or/c 'before 'after) mark/c continuation-mark-set? (or/c #f (listof any/c))
        (or/c #f (listof any/c)))
  (define stx (mark-source top-mark))
  (define src (syntax-source stx))
  (define beg (syntax-position stx))
  (define end (+ (syntax-position stx) (syntax-span stx) -1))
  (define pos (case before/after
                [(before) beg]
                [(after)  end]))
  (define break?
    (match (next)
      [(and (Next points) v)
       (or (eval-point-expression before/after top-mark vals stx
                                  (hash-ref points (cons src pos) #f))
           (Next:All? v))]))
  (cond
    [break?
     ;; If it is not possible to round-trip serialize/deserialize the
     ;; values, use the original values when stepping (don't attempt to
     ;; substitute user-supplied values).
     (define (maybe-serialized-vals)
       (let ([str (~s vals)])
         (if (and (serializable? vals)
                  (<= (string-length str) max-width))
             (cons #t str)
             (cons #f (~s #:max-width    max-width
                          #:limit-marker limit-marker
                          vals)))))
     ;; The on-break-channel is how we notify the Emacs front-end. This
     ;; is a synchronous channel-put but it should return fairly quickly,
     ;; as soon as the command server gets and writes it. In other words,
     ;; this is sent as a notification, unlike a command response as a
     ;; result of a request.
     (define this-break-id (new-break-id))
     (define max-width 128)
     (define limit-marker "⋯")
     (define locals
       (for*/list ([binding  (in-list (mark-bindings top-mark))]
                   [stx      (in-value (first binding))]
                   [get/set! (in-value (second binding))]
                   #:when (and (syntax-original? stx) (syntax-source stx)))
         (list (syntax-source stx)
               (syntax-position stx)
               (syntax-span stx)
               (syntax->datum stx)
               (~v #:max-width    max-width
                   #:limit-marker limit-marker
                   (get/set!)))))
     (channel-put on-break-channel
                  (list 'debug-break
                        (list src pos beg (add1 end))
                        breakable-positions
                        locals
                        (cons this-break-id
                              (case before/after
                                [(before) (list 'before)]
                                [(after)  (list 'after (maybe-serialized-vals))]))))
     (let wait ()
       (sync
        (handle-evt
         ;; Wait for any debug-set-local command to put something to
         ;; on-set-local-channel, then wait again.
         on-set-local-channel
         (match-lambda
           [(list (== this-break-id)
                  (app string->path source)
                  pos
                  new-value-str)
            (for*/or ([binding (in-list (mark-bindings top-mark))]
                      [stx (in-value (first binding))]
                      #:when (and (syntax-original? stx) (syntax-source stx))
                      #:when (and (equal? source (syntax-source stx))
                                  (= pos (syntax-position stx)))
                      [get/set! (in-value (second binding))])
              (get/set! (read-str/default new-value-str (get/set!)))
              #t)
            (wait)]
           [_ (wait)]))
        ;; Wait for debug-resume command to put to on-resume-channel.
        ;; If wrong break ID, ignore and wait again.
        (handle-evt
         on-resume-channel
         (match-lambda
           [(list (app from-elisp-resume-next new-next)
                  (list* (== this-break-id) before/after more))
            (next (calc-next new-next before/after top-mark ccm))
            (begin0
                ;; The step annotator needs us to return the values to
                ;; be used when resuming from before or after step --
                ;; either the original values, or those the user asked
                ;; to be substituted.
                (match* [before/after more]
                  [['before (list)]
                   #f]
                  [['before (list new-vals-str)]
                   (read-str/default new-vals-str vals)]
                  [['after (list new-vals-pair)]
                   (match new-vals-pair
                     [(cons #t  new-vals-str) (read-str/default new-vals-str vals)]
                     [(cons '() _)            vals]) ]))]
           [_ (wait)]))))]
    ;; Otherwise, if we didn't break, we simply need to (a) calculate
    ;; the next point and (b) tell the annotator to use the original
    ;; values (no user substitution).
    [else
     (next (calc-next (next) before/after top-mark ccm))
     (case before/after
       [(before) #f]
       [(after)  vals])]))

;; Note: This works using Racket language expressions, regardless of
;; the user program #lang.
(define (eval-point-expression before/after top-mark vals stx expr)
  (cond
    [(boolean? expr) expr]
    [else
     (define where (format "~a ~a::~a:~a"
                           before/after
                           (file-name-from-path (syntax-source stx))
                           (syntax-position stx)
                           (syntax-span stx)))
     (define bindings-outer-to-inner (reverse (mark-bindings top-mark)))
     (define (#%dump)
       (let* ([s (string-append "#%dump " where)]
              [s (for*/fold ([s s])
                            ([binding  (in-list bindings-outer-to-inner)]
                             [stx      (in-value (first binding))]
                             [get/set! (in-value (second binding))]
                             #:when (and (syntax-original? stx)
                                         (syntax-source stx)))
                   (string-append s (format "\n  ~a = ~v"
                                            (syntax-e stx)
                                            (get/set!))))]
              [s (if (eq? before/after 'after)
                     (string-append s (format "\n => ~s" vals))
                     s)])
         (repl-output-message s)
         (log-racket-mode-debugger-info s)
         (void)))
     (define ht
       (for*/hasheq ([binding  (in-list bindings-outer-to-inner)]
                     [stx      (in-value (first binding))]
                     #:when    (and (syntax-original? stx)
                                    (syntax-source stx))
                     [sym      (in-value (syntax->datum stx))]
                     [get/set! (in-value (second binding))])
         (values sym get/set!)))
     (define new-expr
       #`(let-values (#,@(for/list ([(sym get/set!) (in-hash ht)])
                           #`[(#,(datum->syntax #f sym)) #,(get/set!)])
                      [(#%dump) #,#%dump])
           #,expr))
     (define (on-fail e)
       (repl-output-message
        (format "~a\n  in debugger expression: ~s\n  at: ~a"
                (exn-message e)
                expr
                where))
       #f)
     (define result (with-handlers ([exn:fail? on-fail])
                      (eval new-expr)))
     (cond
       [(void? result) #f]
       [else result])]))

(define (serializable? v)
  (with-handlers ([exn:fail:read? (λ _ #f)])
    (equal? v (write/read v))))

(module+ test
  (check-true (serializable? 42))
  (check-true (serializable? 'foo))
  (check-false (serializable? (open-output-string))))

(define (write/read v)
  (define out (open-output-string))
  (write v out)
  (define in (open-input-string (get-output-string out)))
  (read in))

(module+ test
  (check-equal? (write/read 42) 42)
  (check-equal? (write/read 'foo) 'foo))

(define (read-str/default str default)
  (with-handlers ([exn:fail:read? (λ _ default)])
    (read (open-input-string str))))

(define/contract (calc-next next before/after top-mark ccm)
  (-> Resume? (or/c 'before 'after) mark/c continuation-mark-set?
      Next?)
  (define (big-step frames)
    (define num-marks (length (debug-marks (current-continuation-marks))))
    (or (for/or ([frame  (in-list frames)]
                  [depth (in-range (length frames) -1 -1)]
                  #:when (<= num-marks depth))
          (let* ([stx   (mark-source frame)]
                 [src   (syntax-source stx)]
                 [left  (syntax-position stx)]
                 [right (and left (+ left (syntax-span stx) -1))])
            (and right
                 (breakable-position? src right)
                 (Next:Some
                       (cons (list src right #t '(break))
                             (Resume-caps next))))))
        (Next:All (Resume-caps next))))
  (match next
    [(? Resume:Out?)  (big-step (debug-marks ccm))]
    [(? Resume:Over?) (case before/after
                        [(before) (big-step (cons top-mark (debug-marks ccm)))]
                        [(after)  (Next:All (Resume-caps next))])]
    [_ next]))

(define break-id/c nat/c)
(define/contract new-break-id
  (-> break-id/c)
  (let ([n 0]) (λ () (begin0 n (set! n (add1 n))))))

(define/contract (debug-marks ccm)
  (-> continuation-mark-set? (listof mark/c))
  (continuation-mark-set->list ccm debug-key))

;;; Command interface

;; These contracts are suitable for "edge" with ELisp.

(define elisp-action-point/c (list/c path-string? pos/c string?))
(define elisp-resume-next/c (cons/c (or/c 'over 'out 'all 'some 'none)
                                    (listof elisp-action-point/c)))

(define/contract (from-elisp-resume-next v)
  (-> elisp-resume-next/c Resume?)
  (define ctor (case (car v)
                 [(over) Resume:Over]
                 [(out)  Resume:Out]
                 [(all)  Next:All]
                 [(some) Next:Some]
                 [(none) Next:None]))
  (define caps (for/hash ([v (in-list (cdr v))])
                 (match-define (list path-str pos condition) v)
                 (values (cons (string->path path-str)
                               pos)
                         (with-handlers ([exn:fail?
                                          (λ (e)
                                            `(begin
                                              (println ,(exn-message e))
                                              #f))])
                           (read (open-input-string condition))))))
  (ctor caps))

(define locals/c (listof (list/c path-string? pos/c pos/c symbol? string?)))
(define break-vals/c (cons/c break-id/c
                             (or/c (list/c 'before)
                                   (list/c 'after (cons/c boolean? string?)))))
(define on-break/c (list/c 'debug-break
                           (list/c path? pos/c pos/c pos/c)
                           breakable-positions/c
                           locals/c
                           break-vals/c))
(define/contract on-break-channel (channel/c on-break/c) (make-channel))

(define resume-vals/c (cons/c break-id/c
                              (or/c (list/c 'before)
                                    (list/c 'before string?)
                                    (list/c 'after (cons/c elisp-bool/c string?)))))

(define on-resume/c (list/c elisp-resume-next/c resume-vals/c))
(define/contract on-resume-channel
  (channel/c on-resume/c) (make-channel))

(define/contract (debug-resume resume-info)
  (-> on-resume/c #t)
  (channel-put on-resume-channel resume-info)
  #t)

(define on-set-local/c (list/c break-id/c string? pos/c string?))
(define/contract on-set-local-channel
  (channel/c on-set-local/c) (make-channel))

(define/contract (debug-set-local set-local-info)
  (-> on-set-local/c #t)
  (channel-put on-set-local-channel set-local-info)
  #t)

(define (debug-disable)
  (next (Next:None (hash)))
  (hash-clear! breakable-positions))

(define (debug-enable-step)
   (next (Next:All (Resume-caps (next)))))

;;; Make eval handler to instrument entire files

(define eval-handler/c (-> any/c any))

(define/contract ((make-debug-eval-handler files [orig-eval (current-eval)]) v)
  (->* ((set/c path?)) (eval-handler/c) eval-handler/c)
  (cond [(compiled-expression? (syntax-or-sexpr->sexpr v))
         (orig-eval v)]
        [else
         (define stx (syntax-or-sexpr->syntax v))
         (define top-stx (expand-syntax-to-top-form stx))
         (cond [(set-member? files (syntax-source stx))
                (next (Next:All (hash)))
                (parameterize* ([current-eval orig-eval]
                                [current-load/use-compiled
                                 (let ([orig (current-load/use-compiled)])
                                   (λ (file mod)
                                     (cond [(set-member? files file)
                                            (unless (and (pair? mod)
                                                         (pair? (cdr mod))
                                                         (module-declared? file #f))
                                              (load-module/annotate file mod))]
                                           [else
                                            (orig file mod)])))])
                  (eval-syntax (annotate (expand-syntax top-stx))))]
               [else (orig-eval top-stx)])]))

(define (load-module/annotate file m)
  (repl-output-message (format "~v" `(load-module/annotate ,file ,m)))
  (call-with-input-file* file
    (λ (in)
      (port-count-lines! in)
      (parameterize ([read-accept-compiled #f]
                     [current-load-relative-directory (path-only file)])
        (with-module-reading-parameterization
          (λ ()
            (define e (parameterize ([current-namespace (make-base-namespace)])
                        (expand (read-syntax file in))))
            (eval (annotate (check-module-form e m file)))))))))

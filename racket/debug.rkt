;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/format
         racket/list
         racket/match
         (only-in racket/path path-only)
         racket/set
         syntax/modread
         "debug-annotator.rkt"
         "elisp.rkt"
         "interaction.rkt"
         "repl-output.rkt"
         "repl-session.rkt"
         "util.rkt")

(module+ test
  (require rackunit))

(provide (rename-out [on-break-channel debug-notify-channel])
         debug-resume
         debug-disable
         debug-enable-step
         make-debug-eval-handler)

(define-logger racket-mode-debugger)

(define breakable-positions/c (hash/c path? (set/c #:cmp 'eq pos/c)))
(define/contract breakable-positions breakable-positions/c (make-hash))
(define/contract (breakable-position? src pos)
  (-> path? pos/c boolean?)
  (set-member? (hash-ref breakable-positions src (seteq)) pos))

(define/contract (annotate stx #:source [source (syntax-source stx)])
  (->* (syntax?) (#:source path?) syntax?)
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
;; represented as (hash/c (cons/c path? nat/c) (cons/c cond action)).

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
    [(and (Next points) v)
     ;; Note: Although it would be nice to evaluate `conditions`,
     ;; here, we don't have access to (mark-bindings top-mark), here
     ;; -- only when the annotator calls our `pause-before` or
     ;; `pause-after` functions.
     ;;
     ;; Note: Regardless of whether it's Next:Some or Next:All, when
     ;; specific debug point matches this position, prefer it for its
     ;; `actions`. Motivation: A user has a "watchpoint" (action is
     ;; print and/or log) for some point, and we want that to happen
     ;; also when they single step with Next:All.
     (or (hash-ref points (cons src pos) #f)
         (Next:All? v))]))

(define/contract (pause-before top-mark ccm)
  (-> mark/c continuation-mark-set? (or/c #f (listof any/c)))
  (pause 'before top-mark ccm #f))

(define/contract (pause-after top-mark ccm . vals)
  (->* (mark/c continuation-mark-set?) #:rest (listof any/c)
       any)
  (apply values (pause 'after top-mark ccm vals)))

(define/contract (pause before/after top-mark ccm vals)
  (-> (or/c 'before 'after) mark/c continuation-mark-set? (or/c #f (listof any/c))
      (or/c #f (listof any/c)))
  (define stx (mark-source top-mark))
  (define src (syntax-source stx))
  (define beg (syntax-position stx))
  (define end (+ (syntax-position stx) (syntax-span stx) -1))
  (define pos (case before/after
                [(before) beg]
                [(after)  end]))

  ;; What to do depends on whether the pause is due to a user debug
  ;; point, and if so, its condition and actions. Regardless, when
  ;; single-stepping always do 'break action.
  (define actions
    (append
     (cond [(Next:All? (next)) '(break)]
           [else                null])
     (match ((pause? src) pos)
       [(cons condition actions)
        #:when
        (or (equal? condition #t) ;short-cut
            (with-handlers ([values
                             (λ (e)
                               (repl-output-message
                                (format "~a\nin debugger condition expression:\n  ~v"
                                        (exn-message e)
                                        condition))
                               #t)]) ;take the actions anyway
              (eval
               (call-with-session-context (current-session-id)
                                          with-locals
                                          condition
                                          (mark-bindings top-mark)))))
        actions]
       [_ null])))

  (when (memq 'print actions)
    (unless (null? (mark-bindings top-mark))
      (repl-output-message "Debugger watchpoint; locals:")
      (for* ([binding  (in-list (reverse (mark-bindings top-mark)))]
             [stx      (in-value (first binding))]
             [get/set! (in-value (second binding))]
             #:when (and (syntax-original? stx) (syntax-source stx)))
        (repl-output-message (format " ~a = ~a" stx (~v (get/set!)))))))

  (when (memq 'log actions)
    (log-racket-mode-debugger-info
     "watch ~a ~v~a"
     before/after
     stx
     (for*/fold ([str ""])
                ([binding  (in-list (reverse (mark-bindings top-mark)))]
                 [stx      (in-value (first binding))]
                 [get/set! (in-value (second binding))]
                 #:when (and (syntax-original? stx) (syntax-source stx)))
       (string-append str (format "\n ~a = ~a" stx (~v (get/set!)))))))

  (cond
    [(memq 'break actions)
     ;; Start a debug repl on its own thread, because below we're going to
     ;; block indefinitely with (channel-get on-resume-channel), waiting for
     ;; the Emacs front end to issue a debug-resume command.
     (define repl-thread (thread (repl src pos top-mark)))
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
     ;; Wait for debug-resume command to put to on-resume-channel. If
     ;; wrong break ID, ignore and wait again.
     (let wait ()
       (match (channel-get on-resume-channel)
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
                   [(cons '() _)            vals]) ])
            (kill-thread repl-thread))]
         [_ (wait)]))]
    ;; Otherwise, if we didn't break, we simply need to (a) calculate
    ;; the next point and (b) tell the annotator to use the original
    ;; values (no user substitution).
    [else
     (next (calc-next (next) before/after top-mark ccm))
     (case before/after
       [(before) #f]
       [(after)  vals])]))

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

;;; Debug REPL

(define ((repl src pos top-mark))
  (parameterize ([current-prompt-read (make-prompt-read src pos top-mark)])
    (read-eval-print-loop)))

(define (make-prompt-read src pos top-mark)
  (define (racket-mode-debug-prompt-read)
    (define-values (_base name _dir) (split-path src))
    (define prompt (format "[~a:~a]" name pos))
    (define stx (get-interaction prompt))
    (call-with-session-context (current-session-id)
                               with-locals stx (mark-bindings top-mark)))
  racket-mode-debug-prompt-read)

(define (with-locals stx bindings)
  ;; Before or during module->namespace -- i.e. during a racket-run --
  ;; current-namespace won't (can't) yet be a namespace with module
  ;; body bindings. Indeed it might be from make-base-empty-namespace,
  ;; and not even include racket/base bindings such as #%app. In that
  ;; case make them available. That way the debug REPL at least can
  ;; handle expressions involving local bindings.
  (unless (member '#%app (namespace-mapped-symbols))
    (log-racket-mode-debug "debug prompt-read namespace-require racket/base")
    (namespace-require 'racket/base))
  ;; Note that mark-bindings is ordered from inner to outer scopes --
  ;; and can include outer variables shadowed by inner ones. So use
  ;; only the first occurence of each identifier symbol we encounter.
  ;; e.g. in (let ([x _]) (let ([x _]) ___)) we want only the inner x.
  (define ht (make-hasheq))
  (for* ([binding  (in-list bindings)]
         [sym      (in-value (syntax->datum (first binding)))]
         #:unless (hash-has-key? ht sym)
         [get/set! (in-value (second binding))])
    (hash-set! ht sym get/set!))
  (syntax-case stx ()
    ;; I couldn't figure out how to get a set! transformer to work for
    ;; Typed Racket -- how to annotate or cast a get/set! as (-> Any
    ;; Void). So instead, just intercept (set! id e) as a datum and
    ;; effectively (get/set! (eval e debug-repl-ns)) here. In other
    ;; words treat the stx like a REPL "command". Of course this
    ;; totally bypasses type-checking, but this is a debugger. YOLO!
    [(set! id e)
     (and (module-declared? 'typed/racket/base)
          (eq? 'set! (syntax->datum #'set!))
          (identifier? #'id)
          (hash-has-key? ht (syntax->datum #'id)))
     (let ([set (hash-ref ht (syntax->datum #'id))]
           [v   (eval #'e)])
       (set v)
       #`(void))]
    ;; Wrap stx in a let-syntax form with a make-set!-transformer for
    ;; every local variable in the mark-bindings results.
    [_
     (let ([syntax-bindings
            (for/list ([(sym get/set!) (in-hash ht)])
              (define id (datum->syntax #f sym))
              (define xform
                (make-set!-transformer
                 (λ (stx)
                   (syntax-case stx (set!)
                     [(set! id v) (identifier? #'id) #`(#%plain-app #,get/set! v)]
                     [id          (identifier? #'id) #`'#,(get/set!)]))))
              #`(#,id #,xform))])
       #`(let-syntax #,syntax-bindings
           #,stx))]))

;;; Command interface

;; These contracts are suitable for "edge" with ELisp.

(define elisp-action-point/c (list/c path-string? pos/c string? string?))
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
                 (match-define (list path-str pos condition actions) v)
                 (values (cons (string->path path-str)
                               pos)
                         (cons (read (open-input-string condition))
                               (read (open-input-string actions))))))
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
(define/contract on-resume-channel (channel/c on-resume/c) (make-channel))

(define/contract (debug-resume resume-info)
  (-> on-resume/c #t)
  (channel-put on-resume-channel resume-info)
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

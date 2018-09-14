#lang racket/base

(require (for-syntax racket/base)
         gui-debugger/marks
         racket/contract
         racket/format
         racket/lazy-require
         racket/list
         racket/match
         racket/set
         racket/string
         syntax/modread
         "interactions.rkt"
         "util.rkt")

(lazy-require ["debug-annotator.rkt" (annotate-for-single-stepping)])

(provide (rename-out [on-break-channel debug-notify-channel])
         debug-eval
         debug-resume
         debug-disable
         make-debug-eval-handler
         next-break
         set-debug-repl-namespace!)

(define debug-repl-ns (make-base-namespace))
(define (set-debug-repl-namespace! ns)
  (set! debug-repl-ns ns))

;; A gui-debugger/marks "mark" is a thunk that returns a
;; full-mark-struct -- although gui-debugger/marks doesn't provide
;; that struct. Instead the thunk can be passed to various accessor
;; functions.
(define mark/c (-> any/c))

;; A "mark-binding" is a list whose first element is syntax of the
;; identifier, and whose second element is a get/set! procedure.
(define get/set!/c (case-> (-> any/c)
                           (-> any/c void)))

(define breakable-positions/c (hash/c path? (set/c #:cmp 'eq pos/c)))
(define/contract breakable-positions breakable-positions/c (make-hash))
(define/contract (breakable-position? src pos)
  (-> path? pos/c boolean?)
  (set-member? (hash-ref breakable-positions src (seteq)) pos))

(define/contract (annotate stx)
  (-> syntax? syntax?)
  (define source (syntax-source stx))
  (display-commented (format "Debug annotate ~v" source))
  (define-values (annotated breakables)
    (annotate-for-single-stepping stx break? break-before break-after))
  (hash-update! breakable-positions
                source
                (λ (s) (set-union s (list->seteq breakables)))
                (seteq))
  annotated)

(define break-when/c (or/c 'all 'none (cons/c path-string? pos/c)))
(define/contract next-break
  (case-> (-> break-when/c)
          (-> break-when/c void))
  (let ([v 'none])
    (case-lambda [() v]
                 [(v!) (set! v v!)])))

;; If this returns #t, either break-before or break-after will be
;; called next.
(define ((break? src) pos)
  (match (next-break)
    ['none                    #f]
    ['all                     #t]
    [(cons (== src) (== pos)) #t]
    [_                        #f]))

(define/contract (break-before top-mark ccm)
  (-> mark/c continuation-mark-set? (or/c #f (listof any/c)))
  (break 'before top-mark ccm #f))

(define/contract (break-after top-mark ccm . vals)
  (->* (mark/c continuation-mark-set?) #:rest (listof any/c)
       any)
  (apply values (break 'after top-mark ccm vals)))

(define/contract (break before/after top-mark ccm vals)
  (-> (or/c 'before 'after) mark/c continuation-mark-set? (or/c #f (listof any/c))
      (or/c #f (listof any/c)))
  (define stx (mark-source top-mark))
  (define src (syntax-source stx))
  (define pos (case before/after
                [(before)    (syntax-position stx)]
                [(after)  (+ (syntax-position stx) (syntax-span stx) -1)]))
  (define locals
    (for*/list ([binding  (in-list (mark-bindings top-mark))]
                [stx      (in-value (first binding))]
                [get/set! (in-value (second binding))]
                #:when (and (syntax-original? stx) (syntax-source stx)))
      (list (syntax-source stx)
            (syntax-position stx)
            (syntax-span stx)
            (syntax->datum stx)
            (~v (get/set!)))))
  ;; Start a debug repl on its own thread, because below we're going to
  ;; block indefinitely with (channel-get on-resume-channel), waiting for
  ;; the Emacs front end to issue a debug-resume command.
  (define repl-thread (parameterize ([current-namespace debug-repl-ns])
                        (thread (repl src pos top-mark))))
  ;; The on-break-channel is how we notify the Emacs front-end. This
  ;; is a synchronous channel-put but it should return fairly quickly,
  ;; as soon as the TCP command server gets and writes it. In other
  ;; words, this is sent as a notification, unlike a command response
  ;; as a result of a request.
  (define this-break-id (new-break-id))
  (channel-put on-break-channel
               (list 'debug-break
                     (cons src pos)
                     breakable-positions
                     locals
                     (cons this-break-id
                           (case before/after
                             [(before) (list 'before)]
                             [(after)  (list 'after (~s vals))]))))
  ;; Wait for debug-resume command to put to on-resume-channel. If
  ;; wrong break ID, ignore and wait again. Note that some Racket
  ;; values are non-serializable -- e.g. #<output-port> -- in which
  ;; case just eat the exn:fail:read and use the original `vals`.
  (let wait ()
    (begin0
        (match (channel-get on-resume-channel)
          [(list break-when (list (== this-break-id) 'before))
           (next-break (calc-next-break before/after break-when top-mark ccm))
           #f]
          [(list break-when (list (== this-break-id) (or 'before 'after) vals-str))
           (next-break (calc-next-break before/after break-when top-mark ccm))
           (with-handlers ([exn:fail:read? (λ _ vals)])
             (read (open-input-string vals-str)))]
          [_ (wait)])
      (kill-thread repl-thread)
      (newline))))

(define/contract (calc-next-break before/after break-when top-mark ccm)
  (-> (or/c 'before 'after) (or/c break-when/c 'over 'out) mark/c continuation-mark-set?
      any)
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
                 (cons src right))))
        'all))
  (match* [break-when before/after]
    [['out  _]       (big-step                (debug-marks ccm))]
    [['over 'before] (big-step (cons top-mark (debug-marks ccm)))]
    [['over 'after]  'all]
    [[v     _]       v]))

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

(define ((make-prompt-read src pos top-mark))
  (define-values (_base name _dir) (split-path src))
  (define stx (get-interaction (format "[~a:~a]" name pos)))
  (with-locals stx (mark-bindings top-mark)))

(define (with-locals stx bindings)
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
           [v   (eval #'e debug-repl-ns)])
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

;; Intended use is for `code` to be a function definition form. It
;; will be re-defined annotated for single stepping: When executed it
;; will call our break?, break-before, and break-after functions.
(define/contract (debug-eval source-str line col pos code)
  (-> path-string? pos/c nat/c pos/c string? #t)
  (define source (string->path source-str))
  (define in (open-input-string code))
  (port-count-lines! in)
  (set-port-next-location! in line col pos)
  (eval (annotate (expand (read-syntax source in))))
  (next-break 'all)
  #t)

(define locals/c (listof (list/c path-string? pos/c pos/c symbol? string?)))
(define break-vals/c (cons/c break-id/c
                             (or/c (list/c 'before)
                                   (list/c 'after string?))))
(define on-break/c (list/c 'debug-break
                           break-when/c
                           breakable-positions/c
                           locals/c
                           break-vals/c))
(define/contract on-break-channel (channel/c on-break/c) (make-channel))

(define resume-vals/c (cons/c break-id/c
                              (or/c (list/c 'before)
                                    (list/c 'before string?)
                                    (list/c 'after string?))))
(define on-resume/c (list/c (or/c break-when/c 'out 'over) resume-vals/c))
(define/contract on-resume-channel (channel/c on-resume/c) (make-channel))

(define/contract (debug-resume resume-info)
  (-> on-resume/c #t)
  (channel-put on-resume-channel resume-info)
  #t)

(define (debug-disable)
  (next-break 'none)
  (for ([k (in-hash-keys breakable-positions)])
    (hash-remove! breakable-positions k)))


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
                (next-break 'all)
                (parameterize* ([current-eval orig-eval]
                                [current-load/use-compiled
                                 (let ([orig (current-load/use-compiled)])
                                   (λ (file mod)
                                     (cond [(set-member? files file)
                                            (load-module/annotate file mod)]
                                           [else
                                            (orig file mod)])))])
                  (eval-syntax (annotate (expand-syntax top-stx))))]
               [else (orig-eval top-stx)])]))

;; This never seems to be called ???
(define (load-module/annotate file m)
  (display-commented (format "~v" `(load-module/annotate ,file ,m)))
  (define-values (base _ __) (split-path file))
  (call-with-input-file* file
    (λ (in)
      (port-count-lines! in)
      (parameterize ([read-accept-compiled #f]
                     [current-load-relative-directory base])
        (with-module-reading-parameterization
          (λ ()
            (define e (parameterize ([current-namespace (make-base-namespace)])
                        (expand (read-syntax file in))))
            (eval (annotate (check-module-form e m file)))))))))

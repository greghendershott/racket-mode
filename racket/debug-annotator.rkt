;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         (only-in racket/base [apply plain-apply])
         (prefix-in kernel: syntax/kerncase)
         (only-in racket/contract -> any/c))

;; This is like gui-debugger/annotate except:
;;
;; 0. Our annotate-stx does NOT add breaks to syntax sources not
;;    matching the syntax it is given. See
;;    https://github.com/racket/drracket/issues/230 and below.
;;
;; 1. "Modernize": Use racket/base not racket/scheme. Don't need
;;    opt-lambda.
;;
;; 2. We remove the record-bound-id and record-top-level-id callbacks
;;    that we don't use, from annotate-for-single-stepping (but leave
;;    them for now in annotate-stx).
;;
;; 3. We remove the source arg that is completely unused (I'm guessing
;;    historical).

(provide annotate-for-single-stepping
         mark-source
         mark-bindings
         debug-key
         mark/c)

(define (annotate-for-single-stepping stx break? break-before break-after)
  (define (break-wrap debug-info annotated raw is-tail?)
    (let* ([start  (syntax-position raw)]
           [end    (+ start (syntax-span raw) -1)]
           [break? (break? (syntax-source raw))])
      (if is-tail?
          #`(let-values ([(value-list) #f])
              (if (#%plain-app #,break? #,start)
                  (set! value-list (#%plain-app
                                    #,break-before
                                    #,debug-info
                                    (#%plain-app current-continuation-marks)))
                  (#%plain-app void))
              (if (#%plain-app not value-list)
                  #,annotated
                  (#%plain-app plain-apply values value-list)))
          #`(let-values ([(value-list) #f])
              (if (#%plain-app #,break? #,start)
                  (set! value-list (#%plain-app
                                    #,break-before
                                    #,debug-info
                                    (#%plain-app current-continuation-marks)))
                  (#%plain-app void))
              (if (#%plain-app not value-list)
                  (#%plain-app
                   call-with-values
                   (#%plain-lambda () #,annotated)
                   (case-lambda
                     [(val) (if (#%plain-app #,break? #,end)
                                (#%plain-app
                                 #,break-after
                                 #,debug-info
                                 (#%plain-app current-continuation-marks)
                                 val)
                                val)]
                     [vals (if (#%plain-app
                                #,break? #,end)
                               (#%plain-app
                                plain-apply
                                #,break-after
                                #,debug-info
                                (#%plain-app current-continuation-marks)
                                vals)
                               (#%plain-app plain-apply values vals))]))
                  (if (#%plain-app #,break? #,end)
                      (#%plain-app
                       plain-apply #,break-after
                       #,debug-info
                       (#%plain-app current-continuation-marks)
                       value-list)
                      (#%plain-app plain-apply values value-list)))))))
  (annotate-stx stx break-wrap))

(define (annotate-stx stx break-wrap [record-bound-id void] [record-top-level-id void])
  (define breakpoints (make-hasheq))

  (define (previous-bindings bound-vars)
    (if (null? bound-vars)
        #'null
        #'(#%plain-app debugger-local-bindings)))

  (define (top-level-annotate stx)
    (kernel:kernel-syntax-case/phase
     stx (namespace-base-phase)
     [(module identifier name mb)
      (module-annotate stx)]
     [else-stx
      (general-top-level-expr-iterator stx  #f)]))

  (define (module-annotate stx)
    (syntax-case (disarm stx) ()
      [(_ identifier name mb)
       (syntax-case (disarm #'mb) ()
         [(plain-module-begin . module-level-exprs)
          (with-syntax ([(module . _) stx])
            (rearm
             stx
             (quasisyntax/loc stx
               (module identifier name
                 #,(rearm
                    #'mb
                    #`(plain-module-begin
                       #,@(map (lambda (e) (module-level-expr-iterator
                                            e (list (syntax-e #'identifier)
                                                    (syntax-source #'identifier))))
                               (syntax->list #'module-level-exprs))))))))])]))

  (define (module-level-expr-iterator stx module-name)
    (kernel:kernel-syntax-case
     stx #f
     [(#%provide . provide-specs)
      stx]
     [(#%declare . declare-specs)
      stx]
     [else-stx
      (general-top-level-expr-iterator stx module-name)]))

  (define (general-top-level-expr-iterator stx module-name)
    (kernel:kernel-syntax-case
     stx #f
     [(define-values (var ...) expr)
      (begin
        (for-each (lambda (v) (record-bound-id 'bind v v))
                  (syntax->list #'(var ...)))
        (quasisyntax/loc stx
          (begin
            (define-values (var ...) #,(annotate #`expr '() #t module-name))
            #,(if (syntax-source stx)
                  #`(begin (#%plain-app
                            #,record-top-level-id
                            '#,module-name
                            (quote-syntax var)
                            (case-lambda
                              [() var]
                              [(v) (set! var v)])) ...)
                  #'(#%plain-app void))
            (#%plain-app void))))]
     [(define-syntaxes (var ...) expr)
      stx]
     [(begin-for-syntax . exprs)
      ;; compile time, so treat it like define-syntaxes
      stx]
     [(begin . top-level-exprs)
      (quasisyntax/loc stx
        (begin #,@(map (lambda (expr)
                         (module-level-expr-iterator expr module-name))
                       (syntax->list #'top-level-exprs))))]
     [(#%require . require-specs)
      stx]
     [(module . _)
      ;; a submodule:
      (module-annotate stx)]
     [(module* . _)
      ;; a submodule:
      (module-annotate stx)]
     [else
      (annotate stx '() #f module-name)]))

  (define (annotate expr bound-vars is-tail? module-name)

    (define annotate-break?
      (let ([pos (syntax-position expr)]
            [src (syntax-source expr)])
        (and src pos
             ;; https://github.com/racket/drracket/issues/230
             (equal? src (syntax-source stx))
             (hash-ref breakpoints pos (lambda () #t))
             (kernel:kernel-syntax-case
              expr #f
              [(if test then else) #t]
              [(begin . bodies) #t]
              [(begin0 . bodies) #t]
              [(let-values . clause) #t]
              [(letrec-values . clause) #t]
              [(set! var val) #t]
              [(with-continuation-mark key mark body) #t]
              [(#%plain-app . exprs) #t]
              [_ #f])
             (begin
               (hash-set! breakpoints pos #f)
               (when (not is-tail?)
                 (hash-set! breakpoints (+ pos (syntax-span expr) -1) #f))
               #t))))

    (define (let/rec-values-annotator letrec?)
      (kernel:kernel-syntax-case
       (disarm expr) #f
       [(label (((var ...) rhs) ...) . bodies)
        (let* ([new-bindings (apply append
                                    (map syntax->list
                                         (syntax->list #`((var ...) ...))))]
               [all-bindings (append new-bindings bound-vars)]
               [new-rhs (map (lambda (expr)
                               (annotate expr
                                         (if letrec? all-bindings bound-vars)
                                         #f module-name))
                             (syntax->list #'(rhs ...)))]
               [last-body (car (reverse (syntax->list #'bodies)))]
               [all-but-last-body (reverse (cdr (reverse (syntax->list #'bodies))))]
               [bodies (append (map (lambda (expr)
                                      (annotate expr all-bindings #f module-name))
                                    all-but-last-body)
                               (list (annotate
                                      last-body
                                      all-bindings
                                      is-tail? module-name)))]
               [local-debug-info (assemble-debug-info new-bindings new-bindings 'normal #f)]
               [previous-bindings (previous-bindings bound-vars)])
          (for-each (lambda (id) (record-bound-id 'bind id id)) new-bindings)
          (with-syntax ([(new-rhs/trans ...) new-rhs]
                        [previous-bindings previous-bindings])
            (if letrec?
                (quasisyntax/loc expr
                  (let ([old-bindings previous-bindings])
                    (label (((debugger-local-bindings)
                             (#%plain-lambda ()
                                             (#%plain-app
                                              list*
                                              #,@local-debug-info
                                              old-bindings)))
                            ((var ...) new-rhs/trans) ...)
                           #,@bodies)))
                (quasisyntax/loc expr
                  (label (((var ...) new-rhs/trans) ...)
                         (let ([debugger-local-bindings
                                (#%plain-lambda ()
                                                (#%plain-app
                                                 list*
                                                 #,@local-debug-info
                                                 previous-bindings))])
                           #,@bodies))))))]))

    (define (lambda-clause-annotator clause)
      (kernel:kernel-syntax-case
       clause #f
       [(arg-list . bodies)
        (let* ([new-bound-vars (arglist-bindings #'arg-list)]
               [all-bound-vars (append new-bound-vars bound-vars)]
               [new-bodies (let loop ([bodies (syntax->list #'bodies)])
                             (if (equal? '() (cdr bodies))
                                 (list (annotate (car bodies) all-bound-vars #t module-name))
                                 (cons (annotate (car bodies) all-bound-vars #f module-name)
                                       (loop (cdr bodies)))))])
          (for-each (lambda (id) (record-bound-id 'bind id id)) new-bound-vars)
          (quasisyntax/loc clause
            (arg-list
             (let ([debugger-local-bindings
                    (#%plain-lambda ()
                                    (#%plain-app
                                     list*
                                     #,@(assemble-debug-info new-bound-vars new-bound-vars 'normal #f)
                                     #,(previous-bindings bound-vars)))])
               #,@new-bodies))))]))

    (define annotated
      (rearm
       expr
       (kernel:kernel-syntax-case
        (disarm expr) #f
        [var-stx (identifier? (syntax var-stx))
                 (let ([binder (and (syntax-original? expr)
                                    (member expr bound-vars free-identifier=?))])
                   (if binder
                       (record-bound-id 'ref expr (car binder))
                       (record-bound-id 'top-level expr expr))
                   expr)]

        [(#%plain-lambda . clause)
         (quasisyntax/loc expr
           (#%plain-lambda #,@(lambda-clause-annotator #'clause)))]

        [(case-lambda . clauses)
         (quasisyntax/loc expr
           (case-lambda #,@(map lambda-clause-annotator (syntax->list #'clauses))))]

        [(if test then else)
         (quasisyntax/loc expr
           (if #,(annotate #'test bound-vars #f module-name)
               #,(annotate #'then bound-vars is-tail? module-name)
               #,(annotate #'else bound-vars is-tail? module-name)))]

        [(begin . bodies)
         (letrec ([traverse
                   (lambda (lst)
                     (if (and (pair? lst) (equal? '() (cdr lst)))
                         `(,(annotate (car lst) bound-vars is-tail? module-name))
                         (cons (annotate (car lst) bound-vars #f module-name)
                               (traverse (cdr lst)))))])
           (quasisyntax/loc expr
             (begin #,@(traverse (syntax->list #'bodies)))))]


        [(begin0 body)
         (quasisyntax/loc expr
           (begin0 #,(annotate #'body bound-vars #t module-name)))]

        [(begin0 . bodies)
         (quasisyntax/loc expr
           (begin0 #,@(map (lambda (expr)
                             (annotate expr bound-vars #f module-name))
                           (syntax->list #'bodies))))]

        [(let-values . clause)
         (let/rec-values-annotator #f)]

        [(letrec-values . clause)
         (let/rec-values-annotator #t)]

        [(set! var val)
         (let ([binder (and (syntax-original? #'var)
                            (member #'var bound-vars free-identifier=?))])
           (when binder
             (record-bound-id 'set expr (car binder)))
           (quasisyntax/loc expr
             (set! var #,(annotate #`val bound-vars #f module-name))))]

        [(quote _) expr]

        [(quote-syntax _) expr]

        [(quote-syntax _ #:local) expr]

        [(with-continuation-mark key mark body)
         (quasisyntax/loc expr
           (with-continuation-mark key
             #,(annotate #'mark bound-vars #f module-name)
             #,(annotate #'body bound-vars is-tail? module-name)))]

        [(#%plain-app . exprs)
         (let ([subexprs (map (lambda (expr)
                                (annotate expr bound-vars #f module-name))
                              (syntax->list #'exprs))])
           (if (or is-tail? (not (syntax-source expr)))
               (quasisyntax/loc expr (#%plain-app . #,subexprs))
               (wcm-wrap (make-debug-info module-name expr
                                          bound-vars bound-vars
                                          'normal #f (previous-bindings bound-vars))
                         (quasisyntax/loc expr
                           (#%plain-app . #,subexprs)))))]

        [(#%top . var) expr]
        [(#%variable-reference . _) expr]

        [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                     (syntax->datum expr))])))

    (if annotate-break?
        (break-wrap
         (make-debug-info module-name expr bound-vars bound-vars
                          'at-break #f (previous-bindings bound-vars))
         annotated
         expr
         is-tail?)
        annotated))

  (values (top-level-annotate stx) (hash-map breakpoints (lambda (k v) k))))

(define (arglist-bindings arglist-stx)
  (syntax-case arglist-stx ()
    [var
     (identifier? arglist-stx)
     (list arglist-stx)]
    [(var ...)
     (syntax->list arglist-stx)]
    [(var . others)
     (cons #'var (arglist-bindings #'others))]))

(define (disarm stx) (syntax-disarm stx code-insp))
(define (rearm old new) (syntax-rearm new old))

(define code-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

;;; marks

;; This is the equivalent of gui-debugger/marks that we actually use.
;; We want to avoid dependency on gui-debugger-lib because it depends
;; on racket/gui.

(define-struct full-mark-struct (module-name source label bindings values))

;; A gui-debugger/marks "mark" is a thunk that returns a
;; full-mark-struct -- although, like gui-debugger/marks, we don't
;; provide that struct. Instead the thunk can be passed to various
;; accessor functions.
(define mark/c (-> any/c))

;; debug-key: this key will be used as a key for the continuation marks.
(define-struct debug-key-struct ())
(define debug-key (make-debug-key-struct))

(define (assemble-debug-info tail-bound free-vars label lifting?)
  (map make-mark-binding-stx free-vars))

(define (wcm-wrap debug-info expr)
  (quasisyntax/loc expr (with-continuation-mark #,debug-key #,debug-info #,expr)))

(define (make-debug-info module-name source tail-bound free-vars label lifting? assembled-info-stx)
  (make-full-mark module-name source label free-vars assembled-info-stx))

(define (make-mark-binding-stx id)
  #`(case-lambda
      [() #,id] ; Note: `id` might be undefined; caller must catch exceptions
      [(v) (set! #,id v)]))

;; the 'varargs' creator is used to avoid an extra cons cell in every mark
(define (make-make-full-mark-varargs module-name source label bindings)
  (lambda (values)
    (make-full-mark-struct module-name source label bindings values)))

(define (make-full-mark module-name source label bindings assembled-info-stx)
  (datum->syntax #'here
                 `(#%plain-lambda ()
                   (#%plain-app
                    ,(make-make-full-mark-varargs module-name source label bindings)
                    ,assembled-info-stx))))

(define (mark-source mark)
  (full-mark-struct-source (mark)))

(define (mark-bindings mark)
  (map list
       (full-mark-struct-bindings (mark))
       (full-mark-struct-values (mark))))

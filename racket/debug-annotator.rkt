#lang racket/base

(require (for-syntax racket/base)
         (only-in mzscheme [apply plain-apply])
         (prefix-in kernel: syntax/kerncase))

(provide annotate-for-single-stepping)

;; This is like gui-debugger/annotate except:
;;
;; 0. Our annotate-stx does NOT add breaks to syntax sources not
;;    matching the syntax it is given. See
;;    https://github.com/racket/drracket/issues/230 and below.
;;
;; 1. Our module-annotate disarms/rearms module level expressions. See
;;    https://github.com/racket/drracket/issues/231 and below.
;;
;; 2. "Modernize": Use racket/base not racket/scheme. Don't need
;;    opt-lambda.
;;
;; 3. We remove the record-bound-id and record-top-level-id callbacks
;;    that we don't use, from annotate-for-single-stepping (but leave
;;    them for now in annotate-stx).
;;
;; 4. We remove the source arg that is completely unused (I'm guessing
;;    historical).

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
    (syntax-case stx ()
      [(_ identifier name mb)
       (syntax-case (disarm #'mb) ()
         [(plain-module-begin . module-level-exprs)
          (with-syntax ([(module . _) stx])
            (quasisyntax/loc stx
              (module identifier name
                #,(rearm
                   #'mb
                   #`(plain-module-begin
                      #,@(map (lambda (e)
                                ;; https://github.com/racket/drracket/issues/231
                                (rearm
                                 e
                                 (module-level-expr-iterator
                                  (disarm e)
                                  (list (syntax-e #'identifier)
                                        (syntax-source #'identifier)))))
                              (syntax->list #'module-level-exprs)))))))])]))

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
                            #,record-top-level-id '#,module-name #'var
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Like gui-debugger/marks. Although we could use that as-is, it would
;; mean a dependency on the entire `drracket` package. Since we're
;; already replacing gui-debugger/annotate, "vendor" this little bit
;; of code to avoid the huge dependency.

(require racket/undefined)

(provide debug-key
         mark-source
         mark-bindings)

(define-struct full-mark-struct (module-name source label bindings values))

;; BREAKPOINT STRUCTURES

(define-struct normal-breakpoint-info (mark-list kind))
(define-struct error-breakpoint-info (message))
(define-struct breakpoint-halt ())
(define-struct expression-finished (returned-value-list))

(define-struct skipto-mark-struct ())
(define skipto-mark? skipto-mark-struct?)
(define skipto-mark (make-skipto-mark-struct))
(define (strip-skiptos mark-list)
  (filter (lambda (x) (#%plain-app not (skipto-mark? x))) mark-list))


;; debug-key: this key will be used as a key for the continuation marks.

(define-struct debug-key-struct ())
(define debug-key (make-debug-key-struct))

(define (extract-mark-list mark-set)
  (strip-skiptos (continuation-mark-set->list mark-set debug-key)))


;; the 'varargs' creator is used to avoid an extra cons cell in every mark:

(define (make-make-full-mark-varargs module-name source label bindings)
  (lambda (values)
    (make-full-mark-struct module-name source label bindings values)))

;; see module top for type

(define (make-full-mark module-name source label bindings assembled-info-stx)
  (datum->syntax #'here
                 `(#%plain-lambda ()
                   (#%plain-app
                    ,(make-make-full-mark-varargs module-name source label bindings)
                    ,assembled-info-stx))))

(define (mark-module-name mark)
  (full-mark-struct-module-name (mark)))

(define (mark-source mark)
  (full-mark-struct-source (mark)))

;; identifier -> identifier
(define (make-mark-binding-stx id)
  #`(case-lambda
      [() #,id] ; Note: `id` might be undefined; caller must catch exceptions
      [(v) (set! #,id v)]))

(define (mark-bindings mark)
  (map list
       (full-mark-struct-bindings (mark))
       (full-mark-struct-values (mark))))

(define (mark-label mark)
  (full-mark-struct-label (mark)))

(define (mark-binding-value mark-binding)
  (with-handlers ([exn:fail:contract:variable? (λ (x) undefined)])
    ((cadr mark-binding))))

(define (mark-binding-binding mark-binding)
  (car mark-binding))

(define (mark-binding-set! mark-binding v)
  ((cadr mark-binding) v))

(define (expose-mark mark)
  (let ([source (mark-source mark)]
        [label (mark-label mark)]
        [bindings (mark-bindings mark)])
    (list source
          label
          (map (lambda (binding)
                 (list (mark-binding-binding binding)
                       (mark-binding-value binding)))
               bindings))))

(define (display-mark mark)
  (apply
   string-append
   (format "source: ~a\n" (syntax->datum (mark-source mark)))
   (format "label: ~a\n" (mark-label mark))
   (format "bindings:\n")
   (map (lambda (binding)
          (format " ~a : ~a\n" (syntax-e (mark-binding-binding binding))
                  (mark-binding-value binding)))
        (mark-bindings mark))))


;; possible optimization: rig the mark-maker to guarantee statically that a
;; variable can occur at most once in a mark.

(define (binding-matches matcher mark)
  (filter (lambda (binding-pair) (matcher (mark-binding-binding binding-pair)))
          (mark-bindings mark)))

(define (lookup-all-bindings matcher mark-list)
  (apply append (map (lambda (m) (binding-matches matcher m)) mark-list)))

(define (lookup-first-binding matcher mark-list fail-thunk)
  (let ([all-bindings (lookup-all-bindings matcher mark-list)])
    (if (null? all-bindings)
        (fail-thunk)
        (car all-bindings))))

(define (lookup-binding mark-list id)
  (mark-binding-value
   (lookup-first-binding (lambda (id2) (free-identifier=? id id2))
                         mark-list
                         (lambda ()
                           (error 'lookup-binding "variable not found in environment: ~a\n"
                                  (if (syntax? id)
                                      (syntax->datum id)
                                      id))))))

(define (all-bindings mark)
  (map mark-binding-binding (mark-bindings mark)))

(define (wcm-wrap debug-info expr)
  (quasisyntax/loc expr (with-continuation-mark #,debug-key #,debug-info #,expr)))


;; DEBUG-INFO STRUCTURES

;;;;;;;;;;
;;
;; make-debug-info builds the thunk which will be the mark at runtime.  It contains
;; a source expression and a set of binding/value pairs.
;; (syntax-object BINDING-SET VARREF-SET any boolean) -> debug-info)
;;
;;;;;;;;;;

(define (make-debug-info module-name source tail-bound free-vars label lifting? assembled-info-stx)
  (make-full-mark module-name source label free-vars assembled-info-stx))

(define (assemble-debug-info tail-bound free-vars label lifting?)
  (map make-mark-binding-stx free-vars))

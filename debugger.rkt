#lang racket/base

(require (for-syntax racket/base)
         data/interval-map
         gui-debugger/annotator
         gui-debugger/marks
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         syntax/id-table
         "cmds.rkt"
         "command-output.rkt"
         "debugger-load.rkt" ;not gui-debugger/load-sandbox b/c gui
         "elisp.rkt"
         "util.rkt")

;;; TODO: A `(debug)` form to put in source, to cause a break?
;;;
;;; TODO: How about handling exn:break -- can we make it work when
;;; user breaks? (Probably of no practical use except in seemingly
;;; "hung" programs.)

(provide make-debug-eval-handler)

;;; Protocol/Flow

;; Our client should set current-eval to the result of
;; `(make-debug-eval-handler (current-eval) file-to-be-debugged)`.
;; Then namespace-require the primary file to be debugged. It will get
;; annotated in the usual way by gui-debugger (plus some extra
;; 'DEBUG-DONE annotation by us).
;;
;; When evaluating the original file and its requires, we respond by
;; outputting zero or more `(also-file? ,path) sexprs. We expect a
;; sexpr answer to each. If non-#f, that file is also annotated for
;; debugging. (This is like the DrR popup asking whether to debug each
;; additional file).
;;
;; Thereafter excecution, followed by one or more breaks. Upon a
;; break, we output a (break ...) sexpr. Then we enter a
;; read-eval-print-loop, whose prompt-read handler prints a
;; "DEBUG:path>" prompt. It is a full-fledged REPL (not limited to
;; "debugging commands"). It supports the usual, non-debuggging
;; commands. And it support certain debugging commands. Some debug
;; commands -- like (break) (set) (get) -- cause us to remain in our
;; extended debug REPL, with the program still at its breakpoint. Some
;; of the debug commands -- (step) or (go) -- exit the extended REPL
;; and resume execution. This resumed execution may result in more
;; breaks.
;;
;; Eventually a 'DEBUG-DONE output from us says we are done with the
;; evaluation of the module. However the annotated code is still live.
;; If it is evaluated (e.g. function call) in the REPL, we may get
;; more breaks. If so, we will again emit a (debug) sexpr and enter
;; our debugger REPL.
;;
;; As a result, it's best to think of the (break) sexpr as a kind of
;; "notification" or "synchronous exception". The Elisp code should
;; expect it at any time. On receipt, it should find-file and enable a
;; debugger minor-mode. Likewise, upon a (go) or (step) it should
;; disable the debugger minor-mode. In other words, the minor mode
;; isn't about debugging, it's about being in a break state, and the
;; things that can be shown or done during that state, including
;; resuming.


;;; Breakpoints

(define step? #t)

;; Annotation populates this with an entry for every breakable
;; position.
(define breakpoints (make-hash)) ;(hash src (hash pos (U #f #t 'one-shot))))

(define (list-breaks)
  (elisp-println breakpoints))

(define (clear-breakable-positions!)
  (hash-clear! breakpoints))

(define (set-breakable-positions! source breakable-positions)
  (for ([pos (in-list breakable-positions)])
    (hash-update! breakpoints
                  source
                  (λ (ht)
                    (hash-set! ht pos #f)
                    ht)
                  (λ () (make-hash)))))

(define (should-break?! src pos)
  (define ht (hash-ref breakpoints src (hash)))
  (match (hash-ref ht pos #f)
    [#f #f]
    [#t #t]
    ['one-shot (hash-set! ht pos #f) #t]))

;; If fuzzy-pos is close to a following actually breakable position,
;; set the breakpoint status and return the actual breakable position
;; (so the client may update its UI). Else return #f (so the client
;; can complain to the user).
(define (set-breakpoint! src fuzzy-pos v)
  (match (hash-ref breakpoints src #f)
    [#f #f]
    [ht
     ;; FIXME: Smarter way to find next breakable position?
     (match (for/or ([i (in-range fuzzy-pos (+ fuzzy-pos 2048))])
              (and (hash-has-key? ht i) i))
       [#f #f]
       [actual-pos (hash-set! ht actual-pos v) actual-pos])]))

;;; Bound identifiers

;; Annotation populates this with an entry for every identifer. We use
;; this to match source positions with identifier stxs. (DrR uses a
;; vector, but I think an interval-map is a better fit.)
(define bound-id-locs (make-hash)) ;(hash/c src interval-map)
(define local-uses '()) ;(listof stx)
(define top-uses '()) ;(listof stx)

(define-syntax-rule (push! v xs)
  (set! xs (cons v xs)))

(define (clear-bound-identifiers!)
  (hash-clear! bound-id-locs)
  (set! local-uses '())
  (set! top-uses '()))

(define (add-bound-identifier! type bound binding)
  (define src (syntax-source bound))
  (define pos (syntax-position bound))
  (define span (syntax-span bound))
  (when (and src pos span)
    (unless (eq? type 'top-level)
      (hash-update! bound-id-locs
                    src
                    (λ (im)
                      (interval-map-set! im pos (+ pos span) binding)
                      im)
                    make-interval-map))
    (when (eq? type 'ref)
      (push! bound local-uses))
    (when (eq? type 'top-level)
      (push! bound top-uses))))

(define ((id=? a) b)
  (or (bound-identifier=? a b)
      (free-identifier=? a b)))

(define (position->identifier src pos)
  (interval-map-ref (hash-ref bound-id-locs src (make-interval-map))
                    pos
                    #f))

(define (list-bindings) ;just for dev
  (local-require racket/pretty)
  (pretty-print (for/list ([(src im) (in-hash bound-id-locs)])
                  (list src
                        (for/list ([(k v) (in-dict im)])
                          (list k (syntax->datum v)))))
                (current-error-port)))

;;; Top-level bindings

(define top-level-bindings '()) ;(listof (cons/c stx procedure?))

(define (clear-top-level-bindings!)
  (set! top-level-bindings '()))

(define (add-top-level-binding! var get/set!)
  (set! top-level-bindings
        (cons (cons var get/set!) top-level-bindings)))

(define (lookup-top-level-var var)
  (for/or ([b (in-list top-level-bindings)])
    (match-define (cons v get/set!) b)
    (and (or (bound-identifier=? v var)
             (free-identifier=? v var))
         get/set!)))

;;; Get/set vars (either bound or top-level)

;; The success continuation is called with the value and the setter proc.
(define (lookup-var id frames sk fk)
  (cond [(and id frames (lookup-first-binding (λ (id2) (free-identifier=? id id2))
                                              frames
                                              (λ () #f)))
         => (λ (binding)
              (sk (mark-binding-value binding)
                  (λ (v) (mark-binding-set! binding v))))]
        [(and id (lookup-top-level-var id))
         => (λ (tlb)
              (sk (tlb) tlb))]
        [else (fk)]))

(define (get-var frames src pos) ;; -> string?
  (lookup-var (position->identifier src pos)
              frames
              (λ (val get/set!) (~s val)) ;~s for write so we can read later
              (λ () 'undefined)))

(define (set-var frames src pos new-val) ;; -> string?
  (with-handlers ([exn:fail? (λ _ #f)])
    (lookup-var (position->identifier src pos)
                frames
                (λ (val get/set!) (get/set! new-val) #t)
                (λ () #f))))

;;; Annotation callbacks

(define (record-bound-identifier type bound binding)
  (add-bound-identifier! type bound binding)
  (void))

(define (record-top-level-identifier mod var get/set!)
  (add-top-level-binding! var get/set!)
  (void))

;; When break? returns #t, either break-before or break-after will be
;; called next.
(define ((break? src) pos)
  (or (should-break?! src pos) ;do first so can clear one-shot breaks
      step?))

(define (break-before top-mark ccm)
  (break 'before top-mark ccm #f))

(define (break-after top-mark ccm . vals)
  (apply values (break 'after top-mark ccm vals)))

(define (break which top-mark ccm vals)
  (define other-marks (continuation-mark-set->list ccm debug-key))
  (define all-marks (cons top-mark other-marks))
  (define stx (mark-source top-mark))
  (define src (syntax-source stx))
  (define pos (case which
                [(before) (syntax-position stx)]
                [(after)  (+ (syntax-position stx) (syntax-span stx) -1)]))
  (define locals (for*/list ([b (in-list (mark-bindings top-mark))]
                             [stx (in-value (mark-binding-binding b))]
                             #:when (syntax-original? stx)
                             [val (in-value (mark-binding-value b))])
                   (list (filter (id=? stx) local-uses) val)))
  (define tops (for*/list ([b (in-list top-level-bindings)]
                           [stx (in-value (car b))]
                           [val (in-value ((cdr b)))])
                 (list (filter (id=? stx) top-uses) val)))
  (define bindings (append locals tops))
  (with-output-to-debug-break-output-file
    (λ ()
      (elisp-println
       `(break
         ,which
         (pos    ,pos) ;also in stx, but extract for Elisp convenience
         (src    ,src) ;also in stx, but extract for Elisp convenience
         (stx    ,stx)
         (module ,(mark-module-name top-mark))
         (frames ,(for/list ([m (in-list all-marks)])
                    (mark-source m)))
         (bindings ,bindings)
         (vals ,(and vals (~s vals))))))) ;~s for write so we can read later

  (define (enrich-with-locals stx)
    ;; Using module->namespace gives read/write access to top-level
    ;; identifiers. But for locals, we need to wrap the REPL input stx
    ;; in a let-syntax that has a make-set!-transformer for each
    ;; local.
    (syntax-case stx ()
      [stx
       #`(let #,(for*/list ([b (in-list (mark-bindings top-mark))]
                            [stx (in-value (mark-binding-binding b))]
                            #:when (syntax-original? stx)
                            [id (in-value (syntax->datum stx))]
                            [val (in-value (mark-binding-value b))])
                  #`[#,id #,val])
       ;; #`(let ()
       ;;     (local-require gui-debugger/marks)
       ;;     (let-syntax #,(for*/list ([b (in-list (mark-bindings top-mark))]
       ;;                               [stx (in-value (mark-binding-binding b))]
       ;;                               #:when (syntax-original? stx))
       ;;                     (define get/set! (cadr b))
       ;;                     #`[#,(syntax->datum stx)
       ;;                        (make-set!-transformer
       ;;                         (λ (stx)
       ;;                           (syntax-case stx (set!)
       ;;                             [(set! id v)
       ;;                              #'(mark-binding-set! #,b v)]
       ;;                             [id
       ;;                              (identifier? #'id)
       ;;                              #`#,(get/set!)])))])
              stx)]))

  (define ((debug-prompt-read resume))
    (printf "DEBUG:~a:~a> " (path->string src) pos)
    (define in ((current-get-interaction-input-port)))
    (define stx ((current-read-interaction) (object-name in) in))
    (newline) ;cosmetic
    (syntax-case stx ()
      ;; #,command redirect
      [(uq cmd)
       (eq? 'unsyntax (syntax-e #'uq))
       (with-output-to-command-output-file
         (λ () (handle-debug-command #'cmd resume)))]
      ;; ,command normal
      [(uq cmd)
       (eq? 'unquote (syntax-e #'uq))
       (handle-debug-command #'cmd resume)]
      [_ (println (syntax->datum (enrich-with-locals stx)))
         (enrich-with-locals stx)]))

  (define (handle-debug-command cmd-stx resume)
    (match (syntax->datum cmd-stx)
      [`(step)          (set! step? #t) (resume vals)]
      [`(step ,vs)      (set! step? #t) (resume vs)]
      [`(go)            (set! step? #f) (resume vals)]
      [`(go ,vs)        (set! step? #f) (resume vs)]
      [`(break ,pos ,v) (elisp-println (set-breakpoint! src pos v))]
      [`(get ,pos)      (elisp-println (get-var all-marks src pos))]
      [`(set ,pos ,v)   (elisp-println (set-var all-marks src pos v))]
      [_                (handle-command cmd-stx src)]))

  (let/ec resume
    (parameterize ([current-prompt-read (debug-prompt-read resume)]
                   [current-namespace (module->namespace src)])
      (read-eval-print-loop))))

(define (debug-done)
  ;; Evaluation of the annotated module has completed. The annotated
  ;; code is still "live" and can still be evaluated from the REPL --
  ;; e.g. calling a function. The annotation will still call our
  ;; `break?`, which will continue to break due to existing
  ;; breakpoints or if `step?` is #t. Set `step?` to #t here (it may
  ;; have been set #f by a `go` command) so user gets at least the
  ;; initial break.
  (set! step? #t)
  (with-output-to-debug-break-output-file
    (λ ()
      (elisp-println 'DEBUG-DONE))))

;;; Annotation

(define (make-debug-eval-handler orig-eval file-to-debug)
  (set! step? #t)
  (clear-breakable-positions!)
  (clear-bound-identifiers!)
  (clear-top-level-bindings!)

  (define (annotate-module? path [module 'n/a])
    (or (equal? path file-to-debug)
        (and (path? path)
             (equal? (path-only path) (path-only file-to-debug)) ;FIXME
             (begin
               (with-output-to-debug-break-output-file
                 (λ () (elisp-println `(also-file? ,path))))
               (read)))))

  (λ (orig-exp)
    (cond [(compiled-expression? (syntax-or-sexpr->sexpr orig-exp))
           (orig-eval orig-exp)]
          [else
           (define exp (syntax-or-sexpr->syntax orig-exp))
           (define top-e (expand-syntax-to-top-form exp))
           (define path (and (syntax? orig-exp)
                             (let ([src (syntax-source orig-exp)])
                               (and (path? src)
                                    src))))
           (cond [(annotate-module? path)
                  (parameterize ([current-eval orig-eval])
                    (eval/annotations top-e
                                      annotate-module?
                                      (annotator file-to-debug)))]
                 [else (orig-eval top-e)])])))

(define ((annotator add-done-path) stx)
  (define source (syntax-source stx))
  (define-values (annotated breakable-positions)
    (annotate-for-single-stepping (expand-syntax stx)
                                  break?
                                  break-before
                                  break-after
                                  record-bound-identifier
                                  record-top-level-identifier
                                  source))
  (set-breakable-positions! source breakable-positions)
  (cond [(equal? add-done-path source) (annotate-add-debug-done annotated)]
        [else annotated]))

(define (annotate-add-debug-done stx)
  (syntax-case stx (module)
    [(module id lang mb)
     (syntax-case (disarm #'mb) ()
       [(plain-module-begin module-level-exprs ...)
        (quasisyntax/loc stx
          (module id lang
            #,(rearm #'mb
                     #`(plain-module-begin
                        module-level-exprs ...
                        (#%plain-app #,debug-done)))))])]))

(define (disarm stx) (syntax-disarm stx code-insp))
(define (rearm old new) (syntax-rearm new old))

(define code-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example usage

;; (define file-to-debug (string->path "/tmp/a.rkt"))
;; (parameterize ([current-eval (make-debug-eval-handler (current-eval)
;;                                                       file-to-debug)])
;;   (namespace-require file-to-debug))

;; Local Variables:
;; coding: utf-8
;; comment-column: 40
;; indent-tabs-mode: nil
;; require-final-newline: t
;; show-trailing-whitespace: t
;; End:

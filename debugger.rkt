#lang racket/base

(require data/interval-map
         gui-debugger/annotator
         gui-debugger/marks
         racket/match
         racket/port
         racket/dict
         "command-output.rkt"
         "debugger-load.rkt" ;not gui-debugger/load-sandbox b/c gui
         "elisp.rkt"
         "util.rkt")

(provide make-debug-eval-handler)

;; Active breakpoints: Presumably these will be Emacs overlays?

;; Frames and Vars: Presumably these will be in some separate
;; racket-debug-watch-mode window?

;; Multi-file debugging. DrR prompts as eval-handler is given each
;; file. I guess that will require a command prompt?

;; How about something like Elisp `(debug id ...)`, or Ruby `pry`? Is
;; that orthogonal -- could be done with errortrace instrumentation?
;; Maybe the only connection to this is that IF full debugger is
;; running it could be an automatic breakpoint.


;;; Printing
(define (pr v)
  (begin
    (local-require racket/pretty)
    (pretty-print v (current-error-port))) ;just to debug the debugger
  (with-output-to-command-output-file (λ () (elisp-println v))))
;; (require racket/pretty)  ;for interactice dev...
;; (define pr pretty-print) ;...easier to read


;;; Breakpoints

(define step? #t)

;; Annotation populates this with an entry for every breakable
;; position. Subsequently, you can only update entries to be #t or
;; #f. IOW you can't set a breakpoint for a position that is not
;; breakable.
(define breakpoints (make-hash)) ;(hash/c (cons src pos) boolean)

(define (clear-breakable-positions!)
  (hash-clear! breakpoints))

(define (list-breaks)
  (elisp-println breakpoints))

(define (should-break? src pos)
  (hash-ref breakpoints (cons src pos) #f))

(define (set-breakable-positions! source breakable-positions)
  (for ([pos (in-list breakable-positions)])
    (hash-set! breakpoints (cons source pos) #f)))

(define (set-breakpoint! src pos on?)
  (define key (cons src pos))
  (cond [(hash-has-key? breakpoints key)
         (hash-set! breakpoints key on?)
         #t]
        [else #f]))

;;; Bound identifiers

;; Annotation populates this with an entry for every identifer. We use
;; this to match source positions with identifier stxs. (DrR uses a
;; vector, but I think an interval-map is a better fit.)
(define bound-identifiers (make-hash)) ;(hash/c src interval-map)

(define (clear-bound-identifiers!)
  (hash-clear! bound-identifiers))

(define (add-bound-identifier! bound binding)
  (define src (syntax-source bound))
  (define pos (syntax-position bound))
  (define span (syntax-span bound))
  (when (and src pos span)
    (hash-update! bound-identifiers
                  src
                  (λ (im)
                    (interval-map-set! im pos (+ pos span) binding)
                    im)
                  (λ () (make-interval-map)))))

(define (position->identifier src pos)
  (interval-map-ref (hash-ref bound-identifiers src (make-interval-map))
                    pos
                    #f))

(define (list-bindings) ;just for dev
  (local-require racket/pretty)
  (pretty-print (for/list ([(src im) (in-hash bound-identifiers)])
                  (list src
                        (for/list ([(k v) (in-dict im)])
                          (list k (syntax->datum v)))))
                (current-error-port)))

;;; Top-level bindings

(define top-level-bindings '()) ;(list/c (cons/c stx procedure?))

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

(define (get-var frames src pos)
  (lookup-var (position->identifier src pos)
              frames
              (λ (val get/set!) (pr (format "~a" val)))
              (λ () (pr 'undefined))))

(define (set-var frames src pos new-val-str)
  (with-handlers ([exn:fail? void])
    (define new-val (with-input-from-string new-val-str read))
    (lookup-var (position->identifier src pos)
                frames
                (λ (val get/set!) (get/set! new-val) #t)
                (λ () #f))))

;;; Annotation callbacks

(define (record-bound-identifier type bound binding)
  ;;(pr (list 'record-bound type bound binding))
  (add-bound-identifier! bound binding)
  (void))

(define (record-top-level-identifier mod var get/set!)
  ;;(pr (list 'record-top-level mod var get/set!))
  (add-top-level-binding! var get/set!)
  (void))

;; When break? returns #t, either break-before or break-after will be
;; called next.
(define ((break? src) pos)
  (or step?
      (should-break? src pos)))

(define (break-before top-mark ccm)
  (break-prompt 'break-before top-mark ccm #f))

(define (break-after top-mark ccm . vals)
  (apply values (break-prompt 'break-after top-mark ccm vals)))

(define (break-prompt which top-mark ccm vals)
  (define other-marks (continuation-mark-set->list ccm debug-key))
  (define all-marks (cons top-mark other-marks))
  (define stx (mark-source top-mark))
  (define src (syntax-source stx))
  (define pos (case which
                [(break-before) (syntax-position stx)]
                [(break-after)  (+ (syntax-position stx) (syntax-span stx) -1)]))
  (pr
   `(,which
     (pos    ,pos)
     (stx    ,stx)
     (module ,(mark-module-name top-mark))
     (frames ,(for/list ([m (in-list all-marks)])
                (mark-source m)))
     (bindings ,(for*/list ([b (in-list (mark-bindings top-mark))]
                            [stx (in-value (mark-binding-binding b))]
                            #:when (syntax-original? stx)
                            [val (in-value (mark-binding-value b))])
                  (list stx val)))
     (vals ,vals)))
  ;; Could this be a read-eval-print-loop much like the main REPL?
  ;; Allowing arbitrary evaluations?
  (let loop ()
    (eprintf "DEBUG> ") ;;just to keep racket-repl happy for input during dev
    (match (read)
      ;; Commands to resume, optionally modifying `vals` (whose
      ;; meaning varies for break-before and break-after):
      [`(step)        (set! step? #t) vals]
      [`(step ,vs)    (set! step? #t) vs]
      [`(go)          (set! step? #f) vals]
      [`(go ,vs)      (set! step? #f) vs]
      ;; Commands to tweak state but not yet resume (ergo the `loop`):
      [`(break ,pos)  (pr (set-breakpoint! src pos #t)) (loop)]
      [`(clear ,pos)  (pr (set-breakpoint! src pos #f)) (loop)]
      ;; [`(breaks)      (list-breaks)                     (loop)]
      [`(bindings)    (list-bindings)                   (loop)]
      [`(get ,pos)    (pr (get-var all-marks src pos))   (loop)]
      [`(set ,pos ,v) (pr (set-var all-marks src pos v)) (loop)]
      [_              (pr "unknown command")             (loop)])))

(define (debug-done)
  (pr 'DEBUG-DONE))

;;; Annotation

(define (make-debug-eval-handler orig-eval files-to-debug)
  (set! step? #t)
  (clear-breakable-positions!)
  (clear-bound-identifiers!)
  (clear-top-level-bindings!)

  (define (annotate-module? path [module 'n/a])
    ;; (pr `(debug-file? ,filename))
    ;; (display "DEBUG> ")
    ;; (read)
    (member path files-to-debug))

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
                                      (annotator (car files-to-debug))))]
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

;; (define files-to-debug (map string->path '("/tmp/simple.rkt" "/tmp/foo.rkt")))

;; (parameterize ([current-eval (make-debug-eval-handler (current-eval)
;;                                                       files-to-debug)])
;;   (namespace-require (car files-to-debug)))

;; Local Variables:
;; coding: utf-8
;; comment-column: 40
;; indent-tabs-mode: nil
;; require-final-newline: t
;; show-trailing-whitespace: t
;; End:

#lang racket/base

(require data/interval-map
         gui-debugger/annotator
         gui-debugger/marks
         racket/match
         racket/dict
         "debugger-load.rkt" ;not gui-debugger/load-sandbox b/c gui
         "elisp.rkt")

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
;;(define pr elisp-println) ;for real, use by Elisp
(require racket/pretty)  ;for interactice dev...
(define pr pretty-print) ;...easier to read


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

(define (list-bindings)
  (pr (for/list ([(src im) (in-hash bound-identifiers)])
        (list src
              (for/list ([(k v) (in-dict im)])
                (list k (syntax->datum v)))))))

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
              (λ (val get/set!) (pr val))
              (λ () (pr 'undefined))))

(define (set-var frames src pos new-val)
  (lookup-var (position->identifier src pos)
              frames
              (λ (val get/set!) (get/set! new-val) #t)
              (λ () #f)))

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
    (display "DEBUG> ") ;;just to keep racket-repl happy for input during dev
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
      [`(breaks)      (list-breaks)                     (loop)]
      [`(bindings)    (list-bindings)                   (loop)]
      [`(get ,pos)    (get-var all-marks src pos)       (loop)]
      [`(set ,pos ,v) (set-var all-marks src pos v)     (loop)]
      [_              (pr "unknown command")            (loop)])))

;;; Annotation

(define ((make-debug-eval-handler orig-eval) orig-exp)
  (cond [(compiled-expression? (if (syntax? orig-exp)
                                   (syntax-e orig-exp)
                                   orig-exp))
         (orig-eval orig-exp)]
        [else
         (define exp (if (syntax? orig-exp)
                         orig-exp
                         (namespace-syntax-introduce
                          (datum->syntax #f orig-exp))))
         (define top-e (expand-syntax-to-top-form exp))
         (define fn (and (syntax? orig-exp)
                         (let ([src (syntax-source orig-exp)])
                           (and (path? src)
                                src))))
         (cond [(annotate-this-module? fn)
                (parameterize ([current-eval orig-eval])
                  (eval/annotations top-e
                                    annotate-module?
                                    annotator))]
               [else (orig-eval top-e)])]))

(define (annotator stx)
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
  annotated)

(define (annotate-module? filename m)
  (annotate-this-module? filename))

(define (annotate-this-module? filename)
  ;; (pr `(debug-file? ,filename))
  ;; (display "DEBUG> ")
  ;; (read)
  (member filename files-to-debug))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define files-to-debug (list (string->path "/tmp/simple.rkt")
                             (string->path "/tmp/foo.rkt")))

(parameterize ([current-eval (make-debug-eval-handler (current-eval))])
  (clear-breakable-positions!)
  (clear-bound-identifiers!)
  (clear-top-level-bindings!)
  (namespace-require (car files-to-debug)))

;; Local Variables:
;; coding: utf-8
;; comment-column: 40
;; indent-tabs-mode: nil
;; require-final-newline: t
;; show-trailing-whitespace: t
;; End:

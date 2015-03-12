#lang racket/base

(require data/interval-map
         gui-debugger/annotator
         gui-debugger/marks
         racket/dict
         racket/format
         racket/match
         racket/path
         racket/port
         "command-output.rkt"
         "debugger-load.rkt" ;not gui-debugger/load-sandbox b/c gui
         "elisp.rkt"
         "util.rkt")

(provide make-debug-eval-handler)

;;; Protocol

;; Our client should set current-eval to the result of
;; `(make-debug-eval-handler (current-eval) file-to-be-debugged)`.
;; Then namespace-require the primary file to be debugged. It will get
;; annotated in the usual way by gui-debugger (plus some extra
;; 'DEBUG-DONE annotation by us).
;;
;; When evaluating the original file and it's requires, we respond by
;; outputting zero or more `(also-file? ,path) sexprs. We expect a
;; sexpr answer to each. If non-#f, that file is also annotated for
;; debugging. (This is like the DrR popup asking whether to debug each
;; additional file).
;;
;; Then we enter what you can think of as a "debugger REPL". The main
;; "prompt" we output is a `(break ...) sexpr. This says we've hit a
;; breakpoint, and where it is. Our client should then issue a sexpr
;; "command" like `(step) or `(go). This leads to another break
;; "prompt" from us, and so on. A final 'DEBUG-DONE "prompt" from us
;; lets the client know it can exit its debugger UI mode.
;;
;; A few commands -- such a `(break) `(set) `(get) -- cause us to
;; enter a "sub-loop": We expect zero or more other such commands, and
;; eventually a command like `(step) or `(go) which will result in
;; another `(break) or 'DEBUG-DONE prompt. In other words, while the
;; program is at a breakpoint, zero or more special commands can be
;; issue before resuming execution.
;;
;; This command and sub-command REPL is in the `break-prompt`
;; function.

;;; Printing
(define (pr v)
  ;; (begin
  ;;   (local-require racket/pretty)
  ;;   (pretty-print v (current-error-port))) ;just to debug the debugger
  (with-output-to-command-output-file (λ () (elisp-println v))))

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
  (or (should-break?! src pos) ;do first so can clear one-shot breaks
      step?))

(define (break-before top-mark ccm)
  (break-prompt 'before top-mark ccm #f))

(define (break-after top-mark ccm . vals)
  (apply values (break-prompt 'after top-mark ccm vals)))

(define (break-prompt which top-mark ccm vals)
  (define other-marks (continuation-mark-set->list ccm debug-key))
  (define all-marks (cons top-mark other-marks))
  (define stx (mark-source top-mark))
  (define src (syntax-source stx))
  (define pos (case which
                [(before) (syntax-position stx)]
                [(after)  (+ (syntax-position stx) (syntax-span stx) -1)]))
  (pr
   `(break
     ,which
     (pos    ,pos)
     (src    ,src)
     (stx    ,stx)
     (module ,(mark-module-name top-mark))
     (frames ,(for/list ([m (in-list all-marks)])
                (mark-source m)))
     (bindings ,(for*/list ([b (in-list (mark-bindings top-mark))]
                            [stx (in-value (mark-binding-binding b))]
                            #:when (syntax-original? stx)
                            [val (in-value (mark-binding-value b))])
                  (list stx val)))
     (vals ,(and vals (~s vals))))) ;~s for write so we can read later
  ;; Could this be a read-eval-print-loop much like the main REPL?
  ;; Allowing arbitrary evaluations?
  (let loop ()
    ;;(eprintf "DEBUG> ") ;just to keep racket-repl happy for input during dev
    (match (read)
      [`(quit) (eprintf "Quitting debugger\n") (exit)]
      ;; Commands to resume, optionally modifying `vals` (whose
      ;; meaning varies for 'before and 'after):
      [`(step)     (set! step? #t) vals]
      [`(step ,vs) (set! step? #t) vs]
      [`(go)       (set! step? #f) vals]
      [`(go ,vs)   (set! step? #f) vs]
      ;; Commands to tweak state but not yet resume (ergo the `loop`):
      [`(break ,pos ,v) (pr (set-breakpoint! src pos v))      (loop)]
      [`(get ,pos)      (pr (get-var all-marks src pos))      (loop)]
      [`(set ,pos ,v)   (pr (set-var all-marks src pos v))    (loop)]
      [x                (pr (format "unknown command: ~a" x)) (loop)])))

(define (debug-done)
  (pr 'DEBUG-DONE))

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
               (pr `(also-file? ,path))
               ;;(eprintf "DEBUG> ") ;just to keep racket-repl happy during dev
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

;; (define file-to-debug (string->path "/tmp/simple.rkt"))

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

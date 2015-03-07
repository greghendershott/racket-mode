#lang racket/base

(require gui-debugger/annotator
         gui-debugger/marks
         racket/match
         racket/format
         racket/pretty
         "debugger-load.rkt") ;not gui-debugger/load-sandbox b/c gui

;; Active breakpoints: Presumably these will be Emacs overlays?

;; Frames and Vars: Presumably these will be in some separate
;; racket-debug-watch-mode window?

;; How about something like Elisp `(debug id ...)`, or Ruby `pry`? Is
;; that orthogonal -- could be done with errortrace instrumentation?
;; Maybe the only connection to this is that IF full debugger is
;; running it could be an automatic breakpoint.

(define step? #t)

;; Annotation populates this with an entry for every breakable
;; position. Subsequently, you can only update entries to be #t or #f.
;; IOW you can't set a breakpoint for a position that is not
;; breakable.
(define breakpoints (make-hash)) ;(hash/c (cons src pos) boolean)

(define (list-breaks)
  ;; TODO: Make Elisp friendly.
  (pretty-print breakpoints))

(define (should-break? src pos)
  (hash-ref breakpoints (cons src pos) #f))

(define (reset-breakable-positions! source breakable-positions)
  (clear-breakpoints-for-source! source)
  (set-breakable-positions! source breakable-positions))

(define (clear-breakpoints-for-source! source)
  (for ([key (in-list (for*/list ([key (in-hash-keys breakpoints)]
                                  [src (in-value (car key))]
                                  #:when (equal? source src))
                        key))])
    (hash-remove! breakpoints key)))

(define (set-breakable-positions! source breakable-positions)
  (for ([pos (in-list breakable-positions)])
    (hash-set! breakpoints (cons source pos) #f)))

(define (set-breakpoint! src pos on?)
  (define key (cons src pos))
  (cond [(hash-has-key? breakpoints key)
         (hash-set! breakpoints key on?)
         #t]
        [else #f]))

;;;

(define (record-bound-identifier type bound binding)
  ;;(pretty-print (list ''record-bound type bound binding))
  (void))

(define (record-top-level-identifier mod var get/set!)
  ;;(pretty-print (list ''record-top-level mod var get/set!))
  (void))

(define ((break? src) pos)
  ;; (pretty-print (list ''break? src pos))
  (or step?
      (should-break? src pos)))

(define (break-before top-mark ccm)
  (command 'break-before top-mark ccm '())
  #f)

(define (break-after top-mark ccm . vals)
  (define other-marks (continuation-mark-set->list ccm debug-key))
  (command 'break-after top-mark ccm vals)
  (apply values vals))

(define (command which top-mark ccm vals)
  (define other-marks (continuation-mark-set->list ccm debug-key))
  (define stx (mark-source top-mark))
  (define src (match (syntax-source stx) [(? path? p) (path->string p)] [x x]))
  (define pos (case which
                [(break-before) (syntax-position stx)]
                [(break-after)  (+ (syntax-position stx) (syntax-span stx) -1)]))
  (pretty-print
   `(,which
     (module ,(mark-module-name top-mark))
     (source ,src)
     (pos    ,pos)
     (line   ,(syntax-line stx))
     (col    ,(syntax-column stx))
     (frames ,(for/list ([m (in-list (cons top-mark other-marks))])
                (define stx (mark-source m))
                `(,(syntax-position stx)
                  ,(+ (syntax-position stx) (syntax-span stx)))))
     (bindings ,(for/list ([b (in-list (mark-bindings top-mark))])
                  `(,(syntax-e (mark-binding-binding b))
                    ,(~a (mark-binding-value b)))))
     (vals ,vals)))
  (let loop ()
    (display "DEBUG> ") ;;just to keep racket-repl happy for input
    (match (read)
      [`(step)       (set! step? #t)]
      [`(go)         (set! step? #f)]
      [`(break ,pos) (displayln (set-breakpoint! (syntax-source stx) pos #t)) (loop)]
      [`(clear ,pos) (displayln (set-breakpoint! (syntax-source stx) pos #f)) (loop)]
      [`(list)       (list-breaks) (loop)]
      [_             (displayln #f)])))

(define ((make-debug-eval-handler orig-eval) orig-exp)
  (cond
    [(compiled-expression? (if (syntax? orig-exp)
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

(define (annotate-module? filename m)
  ;; For now, only annotate the original file.
  (annotate-this-module? filename))

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
  (reset-breakable-positions! source breakable-positions)
  annotated)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define file-to-debug (string->path "/tmp/simple.rkt"))

(define (annotate-this-module? filename)
  (equal? filename file-to-debug))

(parameterize ([current-eval (make-debug-eval-handler (current-eval))])
  (namespace-require file-to-debug))

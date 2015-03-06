#lang racket/base

(require gui-debugger/annotator
         gui-debugger/marks
         racket/match
         racket/pretty
         "debugger-load.rkt") ;not gui-debugger/load-sandbox b/c gui

;; Breakpoints: Presumably these will be Emacs overlays?

;; Frames and Vars: Presumably these will be in some separate
;; racket-debug-watch-mode window?

(define step? #t)
(define breakpoints (make-hash)) ;(hash/c pos boolean)

(define (record-bound-identifier type bound binding)
  ;;(pretty-print (list ''record-bound type bound binding))
  (void))

(define (record-top-level-identifier mod var get/set!)
  ;;(pretty-print (list ''record-top-level mod var get/set!))
  (void))

(define ((break? src) pos)
  ;; (pretty-print (list ''break? src pos))
  (or step?
      (hash-ref breakpoints pos #f)))

(define (break-before top-mark ccm)
  (define other-marks (continuation-mark-set->list ccm debug-key))
  (pretty-print (list 'break-before
                      (map (λ (b)
                             (cons (syntax-e (mark-binding-binding b))
                                   (mark-binding-value b)))
                           (mark-bindings top-mark))))
  (command top-mark)
  #f)

(define (break-after top-mark ccm . vals)
  (define other-marks (continuation-mark-set->list ccm debug-key))
  (pretty-print (list 'break-after
                      (map (λ (b)
                             (cons (syntax-e (mark-binding-binding b))
                                   (mark-binding-value b)))
                           (mark-bindings top-mark))
                      vals))
  (command top-mark)
  (apply values vals))

;; prompt for debugger command
(define (command top-mark)
  (define stx (car (expose-mark top-mark)))
  (printf "DEBUG@~a:~a:~a>> "
          (syntax-source stx) (syntax-line stx) (syntax-column stx))
  (match (read-line)
    ["s" (set! step? #t)]
    ["g" (set! step? #f)]
    ["b" (hash-set! breakpoints (syntax-position stx) #t) (command top-mark)]
    ["u" (hash-set! breakpoints (syntax-position stx) #f) (command top-mark)]
    [_   (void)]))

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
     (cond
       [(or (annotate-this-module? fn))
        (parameterize ([current-eval orig-eval])
          (eval/annotations
           top-e
           ; annotate-module?
           (lambda (fn m)
             (annotate-this-module? fn))
           ; annotator
           (lambda (stx)
             (define-values (annotated break-posns)
               (annotate-for-single-stepping
                (expand-syntax stx)
                break? break-before break-after
                record-bound-identifier
                record-top-level-identifier
                (syntax-source stx)))
             (hash-for-each
              breakpoints
              (lambda (pos status)
                ; possible efficiency problem for large files with many breakpoints
                (when (and (syntax-position stx)
                           (>= pos (syntax-position stx))
                           (< pos (+ (syntax-position stx) (syntax-span stx)))
                           (not (memq pos break-posns)))
                  (hash-remove! breakpoints pos))))
             (for ([posn (in-list break-posns)])
               (hash-set!
                breakpoints posn
                (hash-ref breakpoints posn (lambda () #f))))
             annotated)))]
       [else (orig-eval top-e)])]))

(define file-to-debug (string->path "/tmp/simple.rkt"))

(define (annotate-this-module? filename)
  (equal? filename file-to-debug))

(parameterize ([current-eval (make-debug-eval-handler (current-eval))])
  (namespace-require file-to-debug))

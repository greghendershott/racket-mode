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
         "debugger-load.rkt"
         "debugger-step.rkt" ;not gui-debugger/load-sandbox b/c gui
         "elisp.rkt"
         "older-racket.rkt" ;for hash-clear! and hash-empty?
         "util.rkt"
         "with-output.rkt")

;;; TODO: A `(debug)` form to put in source, to cause a break?

;;; TODO: Conditional breakpoints. Break when a user-supplied
;;; expression becomes #t. e.g. `(>= x 5)`, where `x` is the
;;; identifier during the break in which the break-on-expression is
;;; set.

(provide make-debug-eval-handler
         debug-exn:break-handler)

(module+ test
  (require rackunit))

;;; Protocol/Flow

;; Our client should set current-eval to the result of
;; `(make-debug-eval-handler (current-eval) file-to-be-debugged)`.
;; Then namespace-require the primary file to be debugged. Its code
;; will get annotated in the usual way by gui-debugger.
;;
;; When evaluating the original file and its requires, we respond by
;; outputting zero or more `(also-file? ,path) sexprs. We expect a
;; sexpr answer to each. If non-#f, that file is also annotated for
;; debugging. (This is like the DrR popup asking whether to debug each
;; additional file).
;;
;; Thereafter: Execcution, with a breakpoint automatically before the
;; first expression. Upon a break, we output a (break ...) sexpr. Then
;; we enter a read-eval-print-loop, whose prompt-read handler prints a
;; "DEBUG:path:pos>" prompt. It is a full-fledged REPL:
;;
;; 1. Its prompt-read handler wraps the input expression in a
;;    let-syntax with a set!-transformer for every local variable. As
;;    a result the user can eval and even set! local as well as
;;    top-level bindings.
;;
;; 2. It supports the usual racket-mode commands, plus some debugging
;;    commands.
;;
;;    Some of these debug commands -- like (break) (set) (get) --
;;    cause us to remain in our extended debug REPL, with the program
;;    still at its breakpoint.
;;
;;    Other debug commands -- (step) or (go) -- exit the extended REPL
;;    and resume execution. This resumed execution may result in more
;;    breaks.
;;
;; After evaluation of the module completes, its code is still
;; annotated. When any of it is evaluated again in the REPL, it will
;; again break before the first expression -- see all of the above.
;;
;; As a result, it's best to think of the (break) sexpr as a kind of
;; "synchronous notification". The Elisp code should expect it at any
;; time. On receipt, it should find-file and enable a debugger break
;; state minor-mode. Likewise, upon a (go) or (step) it should disable
;; the minor-mode. In other words, the minor mode isn't about
;; debugging per se -- it's about being in a break state, and the
;; things that can be shown or done during that state, including that
;; state by resuming.


(define (debug-marks ccm)
  (continuation-mark-set->list ccm debug-key))

;;; Breakpoints

;; Annotation calls `set-breakable-positions!`, which builds
;; `breakpoints`, a mutable hash-table of hash-tables. (I'd use a
;; single ht with (cons src pos) key, except that set-breakpoint!
;; needs per-source.)
;;
;; (Hash Path ;source
;;       (Hash Natural ;position
;;             (U #f               ;never break
;;                #t               ;always break
;;                'one-shot        ;break first time, then becomes #f
;;                Positive-Integer ;skip N times, then always break
;;                )))
(define breakpoints (make-hash))

(define (clear-breakable-positions!)
  (hash-clear! breakpoints))

(define (set-breakable-positions! source breakable-positions)
  (for ([pos (in-list breakable-positions)])
    (hash-update! breakpoints
                  source
                  (λ (ht)
                    (hash-set! ht pos #f)
                    ht)
                  make-hash)))

(define (break-here?! src pos)
  (define ht (hash-ref breakpoints src (hash)))
  (match (hash-ref ht pos #f)
    [#f #f]
    [#t #t]
    ['one-shot (hash-set! ht pos #f) #t]
    [(? exact-positive-integer? skip)
     (define n (sub1 skip))
     (hash-set! ht pos (if (zero? n) #t n))
     (zero? n)]))

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

(define-syntax-rule (push! xs v)
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
      (push! local-uses bound))
    (when (eq? type 'top-level)
      (push! top-uses bound))))

(define ((id=? a) b)
  (or (bound-identifier=? a b)
      (free-identifier=? a b)))

(define (position->identifier src pos)
  (interval-map-ref (hash-ref bound-id-locs src (make-interval-map))
                    pos
                    #f))


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
              (sk (cadr binding)))]
        [(and id (lookup-top-level-var id))
         => (λ (get/set!)
              (sk get/set!))]
        [else (fk)]))

(define (get-var frames src pos) ;; -> string?
  (lookup-var (position->identifier src pos)
              frames
              (λ (get/set!) (~s (get/set!))) ;~s for write so we can read later
              (λ () 'undefined)))

(define (set-var frames src pos new-val) ;; -> string?
  (with-handlers ([exn:fail? (λ _ #f)])
    (lookup-var (position->identifier src pos)
                frames
                (λ (get/set!) (get/set! new-val) #t)
                (λ () #f))))


;;; Watchpoints

(define watchpoints (make-hash)) ;(hash/c identifier? any/c)

(define (any-watches?)
  (not (hash-empty? watchpoints)))

(define (set-watchpoint! src pos v)
  (match (position->identifier src pos)
    [#f #f]
    [id (hash-set! watchpoints id v) #t]))

(define (clear-watchpoint! src pos)
  (match (position->identifier src pos)
    [#f #f]
    [id (hash-remove! watchpoints id) #t]))

(define (watch-break? frames)
  (for/or ([(id v) (in-hash watchpoints)])
    (lookup-var id
                frames
                (λ (get/set!) (equal? (get/set!) v))
                (λ () (eprintf "not found\n") #f))))

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
  (or (break-here?! src pos) ;test first, has side-effect
      (debug-step?)
      (cond [(any-watches?) (set! break-only-for-watch? #t) #t]
            [else #f])))

;; This bit of awkwardness is because `break?` is not called with
;; `top-mark` -- which we need to detect watch conditions right away.
;; As a result we'll have break? return #t if there are any watches
;; (regardless of their condition) and do the actual test in
;; break-before.
(define break-only-for-watch? #f)

;; Mark (Listof Mark) -> (U #f (Listof Any))
(define (break-before top-mark ccm)
  (define all-marks (cons top-mark (debug-marks ccm)))
  (and (or (not break-only-for-watch?)
           (begin (set! break-only-for-watch? #f)
                  (watch-break? all-marks)))
       (break 'before all-marks #f)))

;; Mark (Listof Mark) (Listof Any) -> (U #f (Listof Any))
(define (break-after top-mark ccm . vals)
  (apply values
         (cond [break-only-for-watch? (set! break-only-for-watch? #f) vals]
               [else (break 'after (cons top-mark (debug-marks ccm)) vals)])))

;; (U 'before 'after) (Listof Mark) (U #f (Listof Any)) -> (U #f (Listof Any))
(define (break which all-marks vals)
  (define top-mark (car all-marks))
  (define stx (mark-source top-mark))
  (define src (syntax-source stx))
  (define pos (match which
                ['after (+ (syntax-position stx) (syntax-span stx) -1)]
                [_      (syntax-position stx)]))
  (define locals (for*/list ([b (in-list (mark-bindings top-mark))]
                             [stx (in-value (mark-binding-binding b))]
                             #:when (syntax-original? stx)
                             [val (in-value (mark-binding-value b))])
                   (list (filter (id=? stx) local-uses)
                         (~s val)))) ;~s write so we can read later
  (define tops (for*/list ([b (in-list top-level-bindings)]
                           [stx (in-value (car b))]
                           [val (in-value ((cdr b)))])
                 (list (filter (id=? stx) top-uses)
                       (~s val)))) ;~s write so we can read later
  (define bindings (append locals tops))
  (with-output-to-debug-break-output-file
    (elisp-println
     `(break
       ,which
       (pos ,pos) ;also in stx, but extract for Elisp convenience
       (src ,src) ;also in stx, but extract for Elisp convenience
       (stx ,stx)
       (module ,(mark-module-name top-mark))
       (frames ,(for/list ([m (in-list all-marks)])
                  (mark-source m)))
       (bindings ,bindings)
       (vals ,(and vals (~s vals)))))) ;~s write so we can read later

  (define (add-locals stx)
    ;; Using module->namespace gives read/write access to top-level
    ;; identifiers. But for locals, we need to wrap the REPL input stx
    ;; in a let-syntax that has a make-set!-transformer for each
    ;; local. Ergo the user can use set! to change the actual value.
    (syntax-case stx ()
      [stx
       ;; Assumption: Given two identifiers with eq symbol, we can
       ;; find the innermost one because it will be first in the list
       ;; returned by mark-bindings. That's my impression from
       ;; gui-debugger/marks.rkt.
       (let ([inner-bindings (make-hasheq)]) ;symbol? => mark-binding
         (for* ([b (in-list (mark-bindings top-mark))]
                [stx (in-value (mark-binding-binding b))]
                #:when (syntax-original? stx)
                [id (in-value (syntax->datum stx))]
                #:when (not (hash-has-key? inner-bindings id)))
           (hash-set! inner-bindings id b))
         #`(let-syntax #,(for*/list ([b (in-hash-values inner-bindings)]
                                     [stx (in-value (mark-binding-binding b))]
                                     [id (in-value (syntax->datum stx))]
                                     [get/set! (in-value (cadr b))])
                           (with-syntax ([id id]
                                         [get/set! get/set!])
                             #'[id
                                (make-set!-transformer
                                 (λ (stx)
                                   (syntax-case stx (set!)
                                     [(set! id v)
                                      (identifier? #'id)
                                      #'(#%plain-app get/set! v)]
                                     [id
                                      (identifier? #'id)
                                      #'(#%plain-app get/set!)])))]))
             stx))]))

  (define ((debug-prompt-read resume))
    (display-prompt (format "DEBUG:~a:~a" (name-only src) pos))
    (define in ((current-get-interaction-input-port)))
    (define stx ((current-read-interaction) (object-name in) in))
    (newline) ;cosmetic
    (syntax-case stx ()
      ;; #,command redirect
      [(uq cmd)
       (eq? 'unsyntax (syntax-e #'uq))
       (with-output-to-command-output-file
         (handle-debug-command #'cmd resume))]
      ;; ,command normal
      [(uq cmd)
       (eq? 'unquote (syntax-e #'uq))
       (handle-debug-command #'cmd resume)]
      [_ (add-locals stx)]))

  (define (handle-debug-command cmd-stx resume)
    (match (syntax->datum cmd-stx)
      [`(step)          (debug-step? #t) (resume vals)]
      [`(step ,vs)      (debug-step? #t) (resume vs)]
      [`(go)            (debug-step? #f) (resume vals)]
      [`(go ,vs)        (debug-step? #f) (resume vs)]
      [`(break ,pos ,v) (elisp-println (set-breakpoint! src pos v))]
      [`(watch ,pos ,v) (elisp-println (set-watchpoint! src pos v))]
      [`(unwatch ,pos)  (elisp-println (clear-watchpoint! src pos))]
      [`(get ,pos)      (elisp-println (get-var all-marks src pos))]
      [`(set ,pos ,v)   (elisp-println (set-var all-marks src pos v))]
      [_                (handle-command cmd-stx src)]))

  (let/ec resume
    (parameterize ([current-prompt-read (debug-prompt-read resume)]
                   [current-namespace (module->namespace src)])
      (read-eval-print-loop))))


;;; Annotation

(define (make-debug-eval-handler orig-eval file-to-debug)
  (cond [file-to-debug
         (debug-step? #t)
         (clear-breakable-positions!)
         (clear-bound-identifiers!)
         (clear-top-level-bindings!)

         (define (annotate-module? path [module 'n/a])
           (or (equal? path file-to-debug)
               (and (path? path)
                    (equal? (path-only path) (path-only file-to-debug)) ;FIXME
                    (begin
                      (with-output-to-debug-break-output-file
                       (elisp-println `(also-file? ,path)))
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
                                             annotator))]
                        [else (orig-eval top-e)])]))]
        [else orig-eval]))

(define (debug-exn:break-handler exn)
  (eprintf "debug exn:break handler, thread ~v\n" (current-thread))
  (break 'exn:break
         (continuation-mark-set->list (exn-continuation-marks exn) debug-key)
         #f)
  ((exn:break-continuation exn)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example usage

;; (let ([file-to-debug (string->path "/tmp/a.rkt")])
;;   (parameterize ([current-eval (make-debug-eval-handler (current-eval)
;;                                                         file-to-debug)])
;;     (namespace-require file-to-debug)))

;; Local Variables:
;; coding: utf-8
;; comment-column: 40
;; indent-tabs-mode: nil
;; require-final-newline: t
;; show-trailing-whitespace: t
;; End:

#lang racket/base
;; Do NOT use `at-exp` in this file! See issue #290.

(require racket/contract
         racket/match
         racket/set
         racket/tcp
         "channel.rkt"
         (only-in "debug.rkt" make-debug-eval-handler next-break)
         "elisp.rkt"
         "error.rkt"
         "gui.rkt"
         "instrument.rkt"
         "interactions.rkt"
         "md5.rkt"
         "mod.rkt"
         "namespace.rkt"
         "print.rkt"
         (only-in "syntax.rkt" with-expanded-syntax-caching-evaluator)
         "util.rkt"
         "welcome.rkt")

(provide start-repl-session-server
         run
         call-with-session-context
         current-session-id
         current-session-maybe-mod
         current-session-md5
         current-session-submit-pred)

(define drracket:submit-predicate/c (-> input-port? boolean? boolean?))

(define-struct/contract session
  ([thread      thread?]
   [ns          namespace?]
   [maybe-mod   (or/c #f mod?)]
   [md5         string?]
   [submit-pred (or/c #f drracket:submit-predicate/c)]))

(define sessions (make-hash))

(define current-session-id (make-parameter #f))
(define current-session-maybe-mod (make-parameter #f))
(define current-session-md5 (make-parameter #f))
(define current-session-submit-pred (make-parameter #f))

(define (call-with-session-context sid proc . args)
  (match (and sid (hash-ref sessions sid #f))
    [(session thread ns maybe-mod md5 submit-pred)
     (parameterize ([current-namespace ns]
                    [current-session-id sid]
                    [current-session-md5 md5]
                    [current-session-maybe-mod maybe-mod]
                    [current-session-submit-pred submit-pred])
       (apply proc args))]
    [_ (apply proc args)]))

(define (start-repl-session-server port launch-token)
  (thread
   (λ ()
     (define listener (tcp-listen port 4 #t "127.0.0.1"))
     (let accept-a-connection ()
       (with-handlers ([exn:fail? void]) ;just disconnect; see #327
         (define-values (in out) (tcp-accept listener))
         (parameterize ([current-input-port  in]
                        [current-output-port out]
                        [current-error-port out])
           (file-stream-buffer-mode in  'none) ;???
           (file-stream-buffer-mode out 'none) ;???
           ;; Immediately after connecting, the client must send us
           ;; exactly the same launch token value that it gave us as a
           ;; command line argument when it started us. Else we close
           ;; the connection. See issue #327.
           (display-commented "Enter launch token")
           (cond [(and launch-token
                       (equal? launch-token (elisp-read in)))
                  (define session-id (gensym 'repl-session-))
                  (elisp-writeln `(ok ,session-id) out)
                  (parameterize ([current-session-id session-id])
                    (thread repl-manager-thread-thunk))]
                 [else
                  (display-commented "Authorization failed")
                  (close-input-port in)
                  (close-output-port out)])))
       (accept-a-connection)))))

(define (repl-manager-thread-thunk)
  (welcome #f)
  (parameterize ([error-display-handler our-error-display-handler])
    (do-run rerun-default)))

(define (do-run rr) ;rerun? -> void?
  (match-define (rerun maybe-mod
                       mem-limit
                       pretty-print?
                       context-level
                       cmd-line-args
                       debug-files
                       retry-as-skeleton?
                       ready-thunk) rr)
  (define-values (dir file mod-path) (maybe-mod->dir/file/rmp maybe-mod))
  ;; Always set current-directory and current-load-relative-directory
  ;; to match the source file.
  (current-directory dir)
  (current-load-relative-directory dir)
  ;; Make src-loc->string provide full pathnames
  (show-full-path-in-errors)
  ;; Custodian for the REPL.
  (define repl-cust (make-custodian))
  (when (< 0 mem-limit)
    (custodian-limit-memory repl-cust
                            (inexact->exact (round (* 1024 1024 mem-limit)))
                            repl-cust))
  ;; If racket/gui/base isn't loaded, the current-eventspace parameter
  ;; doesn't exist, so make a "dummy" parameter of that name.
  (define current-eventspace (txt/gui (make-parameter #f) current-eventspace))

  ;; Create REPL thread
  (define repl-thread
    (parameterize* ;; Use `parameterize*` because the order matters.
        (;; FIRST: current-custodian and current-namespace, so in
         ;; effect for later parameterizations.
         [current-custodian repl-cust]
         [current-namespace (if mod-path
                                ((txt/gui make-base-empty-namespace
                                          make-gui-empty-namespace))
                                ((txt/gui make-base-namespace
                                          make-gui-namespace)))]
         ;; OTHERS:
         [compile-enforce-module-constants #f]
         [compile-context-preservation-enabled (not (eq? context-level 'low))]
         [current-eval
          (cond [(debug-level? context-level) (make-debug-eval-handler debug-files)]
                [(instrument-level? context-level)(make-instrumented-eval-handler)]
                [else (current-eval)])]
         [instrumenting-enabled (instrument-level? context-level)]
         [profiling-enabled (eq? context-level 'profile)]
         [test-coverage-enabled (eq? context-level 'coverage)]
         [current-sync/yield (txt/gui sync yield)]
         ;; LAST: `current-eventspace` because `make-eventspace`
         ;; creates an event handler thread -- now. We want that
         ;; thread to inherit the parameterizations above. (Otherwise
         ;; in the non-gui case, we call `thread` below in the body of
         ;; the parameterize* form, so that's fine.)
         [current-eventspace ((txt/gui void make-eventspace))])
      ;; repl-thunk will either be used as the thunk for a thread we
      ;; make directly -- or, when racket/gui/base is instantiated,
      ;; installed as the current eventspace's event queue via
      ;; queue-callback, running under (eventspace-handler-thread
      ;; (current-eventspace)).
      (define (repl-thunk)
        ;; 0. Command line arguments
        (current-command-line-arguments cmd-line-args)
        ;; 1. Set print hooks and output handlers
        (set-print-parameters pretty-print?)
        (set-output-handlers)
        ;; 2. If module, require and enter its namespace, etc.
        (with-expanded-syntax-caching-evaluator maybe-mod
          (when (and maybe-mod mod-path)
            (parameterize ([current-module-name-resolver module-name-resolver-for-run])
              ;; When exn:fail? during module load, re-run with "empty"
              ;; module. Note: Unlikely now that we're using
              ;; dynamic-require/some-namespace.
              (define (load-exn-handler exn)
                (display-exn exn)
                (channel-put message-to-main-thread-channel
                             (struct-copy rerun rr [maybe-mod #f]))
                (sync never-evt))
              (with-handlers ([exn? load-exn-handler])
                (maybe-configure-runtime mod-path) ;FIRST: see #281
                (current-namespace
                 (dynamic-require/some-namespace maybe-mod retry-as-skeleton?))
                (maybe-warn-about-submodules mod-path context-level)
                (check-#%top-interaction)))))
        ;; 3. Record information about our session
        (hash-set! sessions
                   (current-session-id)
                   (session (current-thread)
                            (current-namespace)
                            maybe-mod
                            (maybe-mod->md5 maybe-mod)
                            (get-repl-submit-predicate maybe-mod)))
        ;; 3b. And call the ready-thunk command-server gave us from a
        ;; run command, so that it can send a response for the run
        ;; command. Because the command server runs on a different
        ;; thread, it is probably waiting with (sync some-channel) and
        ;; the thunk will simply channel-put.
        (ready-thunk)
        ;; 4. read-eval-print-loop
        (parameterize ([current-prompt-read (make-prompt-read maybe-mod)]
                       [current-module-name-resolver module-name-resolver-for-repl])
          (make-get-interaction)
          ;; Note that read-eval-print-loop catches all non-break
          ;; exceptions.
          (read-eval-print-loop)))

      ;; Main thread: Run repl-thunk on a plain thread, or, on the
      ;; eventspace thread via queue-callback. Return the thread.
      (define t/v ((txt/gui thread    queue-callback           ) repl-thunk))
      (define thd ((txt/gui (λ _ t/v) eventspace-handler-thread) (current-eventspace)))
      thd))

  ;; Main thread: Wait for message from REPL thread on channel. Also
  ;; catch breaks, in which case we (a) break the REPL thread so
  ;; display-exn runs there, and (b) continue from the break instead
  ;; of re-running so that the REPL environment is maintained.
  (define message
    (call-with-exception-handler
     (match-lambda
       [(and (or (? exn:break:terminate?) (? exn:break:hang-up?)) e) e]
       [(exn:break msg marks continue) (break-thread repl-thread) (continue)]
       [e e])
     (λ () (sync message-to-main-thread-channel))))
  (match context-level
    ['profile  (clear-profile-info!)]
    ['coverage (clear-test-coverage-info!)]
    [_         (void)])
  (custodian-shutdown-all repl-cust)
  (newline) ;; FIXME: Move this to racket-mode.el instead?
  (match message
    [(? rerun? new-rr) (do-run new-rr)]
    [(load-gui repl?)  (require-gui repl?) (do-run rr)]))

;; command, called from command-server thread
(define/contract (run what mem pp ctx args dbgs skel)
  (-> list? number? elisp-bool/c context-level? list? (listof path-string?) elisp-bool/c
      list?)
  (define ready-channel (make-channel))
  (channel-put message-to-main-thread-channel
               (rerun (->mod/existing what)
                      mem
                      (as-racket-bool pp)
                      ctx
                      (list->vector args)
                      (list->set (map string->path dbgs))
                      (as-racket-bool skel)
                      (λ () (channel-put ready-channel what))))
  ;; Waiting for this allows the command response to be used as the
  ;; all-clear for additional commands that need the module load to be
  ;; done and entering a REPL for that module. For example, to compose
  ;; run with get-profile or get-uncovered.
  (sync ready-channel))

(define/contract ((make-prompt-read m))
  (-> (or/c #f mod?) (-> any))
  (begin0 (get-interaction (maybe-mod->prompt-string m))
    ;; let debug-instrumented code break again
    (next-break 'all)))

(define (maybe-mod->md5 m)
  (define-values (dir file _) (maybe-mod->dir/file/rmp m))
  (if (and dir file)
      (file->md5 (build-path dir file))
      ""))

;; <https://docs.racket-lang.org/tools/lang-languages-customization.html#(part._.R.E.P.L_.Submit_.Predicate)>
(define/contract (get-repl-submit-predicate m)
  (-> (or/c #f mod?) (or/c #f drracket:submit-predicate/c))
  (define-values (dir file rmp) (maybe-mod->dir/file/rmp m))
  (define path (and dir file (build-path dir file)))
  (and path rmp
       (or (with-handlers ([exn:fail? (λ _ #f)])
            (match (with-input-from-file (build-path dir file) read-language)
              [(? procedure? get-info)
               (match (get-info 'drracket:submit-predicate #f)
                 [#f #f]
                 [v  v])]
              [_ #f]))
           (with-handlers ([exn:fail? (λ _ #f)])
             (match (module->language-info rmp #t)
               [(vector mp name val)
                (define get-info ((dynamic-require mp name) val))
                (get-info 'drracket:submit-predicate #f)]
               [_ #f])))))

(define (maybe-configure-runtime mod-path)
  ;; Do configure-runtime when available.
  ;; Important for langs like Typed Racket.
  (with-handlers ([exn:fail? void])
    (match (module->language-info mod-path #t)
      [(vector mp name val)
       (define get-info ((dynamic-require mp name) val))
       (define configs (get-info 'configure-runtime '()))
       (for ([config (in-list configs)])
         (match-let ([(vector mp name val) config])
           ((dynamic-require mp name) val)))]
      [_ (void)])
    (define cr-submod `(submod
                        ,@(match mod-path
                            [(list 'submod sub-paths ...) sub-paths]
                            [_ (list mod-path)])
                        configure-runtime))
    (when (module-declared? cr-submod)
      (dynamic-require cr-submod #f))))

(define (check-#%top-interaction)
  ;; Check that the lang defines #%top-interaction
  (unless (memq '#%top-interaction (namespace-mapped-symbols))
    (display-commented
     "Because the language used by this module provides no #%top-interaction\n you will be unable to evaluate expressions here in the REPL.")))

;; Catch attempt to load racket/gui/base for the first time.
(define (make-module-name-resolver repl?)
  (let ([orig-resolver (current-module-name-resolver)])
    (define (resolve mp rmp stx load?)
      (when (and load? (memq mp '(racket/gui/base
                                  racket/gui/dynamic
                                  scheme/gui/base)))
        (unless (gui-required?)
          (channel-put message-to-main-thread-channel
                       (load-gui repl?))
          (sync never-evt)))
      (orig-resolver mp rmp stx load?))
    (case-lambda
      [(rmp ns)           (orig-resolver rmp ns)]
      [(mp rmp stx)       (resolve mp rmp stx #t)]
      [(mp rmp stx load?) (resolve mp rmp stx load?)])))
(define module-name-resolver-for-run  (make-module-name-resolver #f))
(define module-name-resolver-for-repl (make-module-name-resolver #t))

;;; Output handlers; see issues #381 #397

(define the-default-output-handlers
  (for/hash ([get/set (in-list (list port-write-handler
                                     port-display-handler
                                     port-print-handler))])
    (values get/set (get/set (current-output-port)))))

(define (set-output-handlers)
  (for ([(get/set v) (in-hash the-default-output-handlers)])
    (get/set (current-output-port) v)))

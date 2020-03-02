#lang racket/base
;; Do NOT use `at-exp` in this file! See issue #290.

(require racket/contract
         racket/match
         racket/set
         racket/tcp
         (only-in "debug.rkt" make-debug-eval-handler next-break)
         "elisp.rkt"
         "error.rkt"
         "gui.rkt"
         "instrument.rkt"
         "interactions.rkt"
         "mod.rkt"
         "print.rkt"
         (only-in "syntax.rkt" with-expanded-syntax-caching-evaluator)
         "util.rkt")

(provide start-repl-session-server
         run
         call-with-session-context
         exit-repl-session
         current-session-id
         current-session-maybe-mod
         current-session-submit-pred)

;;; REPL session "housekeeping"

(define next-session-number 0)

(define drracket:submit-predicate/c (-> input-port? boolean? boolean?))

(define-struct/contract session
  ([thread           thread?]  ;the repl manager thread
   [repl-msg-chan    channel?] ;see repl-message structs
   [interaction-chan channel?]
   [ns               namespace?]
   [maybe-mod        (or/c #f mod?)]
   [submit-pred      (or/c #f drracket:submit-predicate/c)])
  #:transparent)

(define sessions (make-hash))

(define current-session-id (make-parameter #f))
(define current-repl-msg-chan (make-parameter #f))
;current-interaction-chan defined in "interactions.rkt"
(define current-session-maybe-mod (make-parameter #f))
(define current-session-submit-pred (make-parameter #f))

;;; Messages to each repl manager thread

;; Definitions for context-level member of run-config struct

(define profile/coverage-levels
  ;; "sibling" levels that need instrument plus...
  '(profile    ;profiling-enabled
    coverage)) ;execute-counts-enabled

(define instrument-levels
  `(high     ;compile-context-preservation-enabled #t + instrument
    ,@profile/coverage-levels))

(define context-levels
  `(low      ;compile-context-preservation-enabled #f
    medium   ;compile-context-preservation-enabled #t
    ,@instrument-levels
    debug))

(define (context-level? v)          (memq? v context-levels))
(define (instrument-level? v)       (memq? v instrument-levels))
(define (profile/coverage-level? v) (memq? v profile/coverage-levels))
(define (debug-level? v)            (eq? v 'debug))

;; The message structs

(define-struct/contract repl-manager-thread-message ())

(define-struct/contract (load-gui repl-manager-thread-message)
  ([in-repl? boolean?]))

(define-struct/contract (run-config repl-manager-thread-message)
  ([maybe-mod       (or/c #f mod?)]
   [memory-limit    exact-nonnegative-integer?] ;0 = no limit
   [pretty-print?   boolean?]
   [context-level   context-level?]
   [cmd-line-args   (vectorof string?)]
   [debug-files     (set/c path?)]
   [ready-thunk     (-> any/c)]))

(define (initial-run-config ready-thunk)
  (run-config #f    ;maybe-mod
              0     ;memory-limit
              #f    ;pretty-print?
              'low  ;context-level
              #()   ;cmd-line-args
              (set) ;debug-files
              ready-thunk))

;;; Functionality provided for commands

;; A way to parameterize commands that need to work with a specific
;; REPL session. Called from command-server thread.
(define (call-with-session-context sid proc . args)
  (log-racket-mode-debug "~v" sessions)
  (match (hash-ref sessions sid #f)
    [(and (session _thd msg-ch int-ch ns maybe-mod submit-pred) s)
     (log-racket-mode-debug "call-with-session-context ~v => ~v" sid s)
     (parameterize ([current-repl-msg-chan       msg-ch]
                    [current-interaction-chan    int-ch]
                    [current-namespace           ns]
                    [current-session-id          sid]
                    [current-session-maybe-mod   maybe-mod]
                    [current-session-submit-pred submit-pred])
       (apply proc args))]
    [_
     (log-racket-mode-debug "call-with-session-context ~v -- no session found" sid)
     (apply proc args)]))

;; Command. Called from command-server thread
(define (exit-repl-session sid)
  (match (hash-ref sessions sid #f)
    [(struct* session ([thread t]))
     (log-racket-mode-debug "exit-repl: break-thread for ~v" sid)
     (break-thread t 'terminate)]
    [_ (log-racket-mode-error "exit-repl: ~v not in `sessions`" sid)]))

;; Command. Called from command-server thread
(define/contract (run what mem pp ctx args dbgs)
  (-> list? number? elisp-bool/c context-level? list? (listof path-string?)
      list?)
  (define ready-channel (make-channel))
  (channel-put (current-repl-msg-chan)
               (run-config (->mod/existing what)
                           mem
                           (as-racket-bool pp)
                           ctx
                           (list->vector args)
                           (list->set (map string->path dbgs))
                           (λ () (channel-put ready-channel what))))
  ;; Waiting for this allows the command response to be used as the
  ;; all-clear for additional commands that need the module load to be
  ;; done and entering a REPL for that module. For example, to compose
  ;; run with get-profile or get-uncovered.
  (sync ready-channel))

;;; REPL session server

(define (start-repl-session-server port launch-token)
  (thread (listener-thread-thunk port launch-token)))

(define ((listener-thread-thunk port launch-token))
  (define listener (tcp-listen port 4 #t "127.0.0.1"))
  (let accept-a-connection ()
    (define custodian (make-custodian))
    (parameterize ([current-custodian custodian])
      ;; `exit` in a REPL should terminate that REPL session -- not
      ;; the entire back end server. Also, this is opportunity to
      ;; remove the session from `sessions` hash table.
      (define (our-exit-handler code)
        (log-racket-mode-info "(our-exit-handler ~v) ~v"
                              code (current-session-id))
        (when (current-session-id) ;might exit before session created
          (hash-remove! sessions (current-session-id))
          (log-racket-mode-debug "sessions: ~v" sessions))
        (custodian-shutdown-all custodian))
      (parameterize ([exit-handler our-exit-handler])
        (define-values (in out) (tcp-accept listener))
        (parameterize ([current-input-port  in]
                       [current-output-port out]
                       [current-error-port  out])
          (for ([p (in-list (list in out))])
            (file-stream-buffer-mode p 'none)) ;would 'line be sufficient?
          ;; Immediately after connecting, the client must send us
          ;; exactly the same launch token value that it gave us as a
          ;; command line argument when it started us. Else we close
          ;; the connection. See issue #327.
          (define supplied-token (elisp-read in))
          (unless (equal? launch-token supplied-token)
            (log-racket-mode-fatal "Authorization failed: ~v"
                                   supplied-token)
            (exit 'racket-mode-repl-auth-failure))
          (thread repl-manager-thread-thunk))))
    (accept-a-connection)))

(define (repl-manager-thread-thunk)
  (define session-id (format "repl-session-~a"
                             (begin0 next-session-number
                               (inc! next-session-number))))
  (log-racket-mode-info "start ~v" session-id)
  (parameterize ([error-display-handler    our-error-display-handler]
                 [current-session-id       session-id]
                 [current-repl-msg-chan    (make-channel)]
                 [current-interaction-chan (make-get-interaction)])
    (do-run
     (initial-run-config
      (λ ()
        ;; Write a sexpr containing the session-id, which the client
        ;; can use in certain commands that need to run in the context
        ;; of a specific REPL. We wait to do so until this ready-thunk
        ;; to ensure the `sessions` hash table has this session before
        ;; any subsequent commands use call-with-session-context.
        (elisp-writeln `(ok ,session-id) (current-output-port))
        (flush-output)
        (display-commented (string-append "\n" (banner))))))))

(define (do-run cfg) ;run-config? -> void?
  (match-define (run-config maybe-mod
                            mem-limit
                            pretty-print?
                            context-level
                            cmd-line-args
                            debug-files
                            ready-thunk)   cfg)
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
              ;; When exn:fail during module load, re-run.
              (define (load-exn-handler exn)
                (define new-mod
                  (match mod-path
                    [`(submod ,(== file) main)
                     (log-racket-mode-debug "~v not found, retry as ~v"
                                            mod-path (build-path dir file))
                     (->mod/existing (build-path dir file))]
                    ;; Else display exn and retry as "empty" REPL.
                    [_
                     (display-exn exn)
                     #f]))
                (channel-put (current-repl-msg-chan)
                             (struct-copy run-config cfg [maybe-mod new-mod]))
                (sync never-evt)) ;manager thread will shutdown custodian
              (with-handlers ([exn? load-exn-handler])
                (maybe-configure-runtime mod-path) ;FIRST: see #281
                (current-namespace
                 (parameterize ([current-load-relative-directory dir]
                                [current-directory               dir])
                   (dynamic-require mod-path #f)
                   (module->namespace mod-path)))
                (maybe-warn-about-submodules mod-path context-level)
                (check-#%top-interaction)))))
        ;; 3. Record information about our session
        (hash-set! sessions
                   (current-session-id)
                   (session (current-thread)
                            (current-repl-msg-chan)
                            (current-interaction-chan)
                            (current-namespace)
                            maybe-mod
                            (get-repl-submit-predicate maybe-mod)))
        (log-racket-mode-debug "sessions: ~v" sessions)
        ;; 4. Now that the program has run, and `sessions` is updated,
        ;; call the ready-thunk. On REPL startup this lets us wait
        ;; sending the repl-session-id until `sessions` is updated.
        ;; And for subsequent run commands, this lets us it wait to
        ;; send a response.
        (ready-thunk)
        ;; 5. read-eval-print-loop
        (parameterize ([current-prompt-read (make-prompt-read maybe-mod)]
                       [current-module-name-resolver module-name-resolver-for-repl])
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
       [(exn:break _msg _marks continue) (break-thread repl-thread) (continue)]
       [e e])
     (λ () (sync (current-repl-msg-chan)))))
  (match context-level
    ['profile  (clear-profile-info!)]
    ['coverage (clear-test-coverage-info!)]
    [_         (void)])
  (custodian-shutdown-all repl-cust)
  (newline) ;; FIXME: Move this to racket-mode.el instead?
  (match message
    [(? run-config? new-cfg) (do-run new-cfg)]
    [(load-gui repl?)        (require-gui repl?) (do-run cfg)]))

(define/contract ((make-prompt-read m))
  (-> (or/c #f mod?) (-> any))
  (begin0 (get-interaction (maybe-mod->prompt-string m))
    ;; let debug-instrumented code break again
    (next-break 'all)))

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
          (channel-put (current-repl-msg-chan)
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

;; These are plain procedures not parameters. Therefore to reset them
;; for each user program run, we must call them each time with the
;; original value. What original value? It suffices to use the value
;; in effect when this back end starts, i.e. the default
;; port-xxx-handler.

(define the-default-output-handlers
  (for/hash ([get/set (in-list (list port-write-handler
                                     port-display-handler
                                     port-print-handler))])
    (values get/set (get/set (current-output-port)))))

(define (set-output-handlers)
  (for ([(get/set v) (in-hash the-default-output-handlers)])
    (get/set (current-output-port) v)))

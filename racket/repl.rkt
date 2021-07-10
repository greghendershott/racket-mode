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
         "repl-session.rkt"
         "stack-checkpoint.rkt"
         (only-in "syntax.rkt" with-expanded-syntax-caching-evaluator)
         "util.rkt")

(provide start-repl-session-server
         repl-tcp-port-number
         run
         break-repl-thread
         repl-zero-column)

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
   [extra-submods   (listof (listof symbol?))]
   [memory-limit    exact-nonnegative-integer?] ;0 = no limit
   [pretty-print?   boolean?]
   [columns         exact-positive-integer?]
   [pixels/char     exact-positive-integer?]
   [context-level   context-level?]
   [cmd-line-args   (vectorof string?)]
   [debug-files     (set/c path?)]
   [ready-thunk     (-> any/c)]))

(define (initial-run-config ready-thunk)
  (run-config #f    ;maybe-mod
              '()   ;extra-submods
              0     ;memory-limit
              #f    ;pretty-print?
              79    ;columns
              12    ;pixels/char
              'low  ;context-level
              #()   ;cmd-line-args
              (set) ;debug-files
              ready-thunk))

;; Command. Called from a command-server thread
(define/contract (break-repl-thread sid kind)
  (-> any/c (or/c 'break 'hang-up 'terminate) any)
  (match (get-session sid)
    [(struct* session ([thread t]))
     (log-racket-mode-debug "break-repl-thread: ~v ~v" sid kind)
     (break-thread t (case kind [(hang-up terminate) kind] [else #f]))]
    [_ (log-racket-mode-error "break-repl-thread: ~v not in `sessions`" sid)]))

;; Command. Called from a command-server thread
(struct zero-column (chan))
(define (repl-zero-column)
  (define ch (make-channel))
  (channel-put (current-repl-msg-chan) (zero-column ch))
  (sync ch))

;; Command. Called from a command-server thread
(define/contract (run what subs mem pp cols pix/char ctx args dbgs)
  (-> list? (listof (listof symbol?)) number? elisp-bool/c number? number? context-level? list? (listof path-string?)
      list?)
  (unless (current-repl-msg-chan)
    (error 'run "current-repl-msg-chan was #f; current-session-id=~v"
           (current-session-id)))
  (define ready-channel (make-channel))
  (channel-put (current-repl-msg-chan)
               (run-config (->mod/existing what)
                           subs
                           mem
                           (as-racket-bool pp)
                           cols
                           pix/char
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

;; Define these at the module level so that they are initialized when
;; the module loads -- before other threads like the session manager
;; or command manager threads try to use them.
(define listener
  (tcp-listen 0  ;choose port dynamically, a.k.a. "ephemeral" port
              64
              #f ;reuse? not recommended for ephemeral ports
              "127.0.0.1"))
(define repl-tcp-port-number
  (let-values ([(_loc-addr port _rem-addr _rem-port) (tcp-addresses listener #t)])
    (log-racket-mode-info "TCP port ~v chosen for REPL sessions" port)
    port))

(define (start-repl-session-server launch-token)
  (thread (listener-thread-thunk launch-token)))

(define ((listener-thread-thunk launch-token))
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
          (remove-session! (current-session-id)))
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
          (port-count-lines! in)  ;#519
          (port-count-lines! out) ;for fresh-line
          (thread repl-manager-thread-thunk))))
    (accept-a-connection)))

(define (repl-manager-thread-thunk)
  (define session-id (next-session-id!))
  (log-racket-mode-info "start ~v" session-id)
  (parameterize* ([error-display-handler racket-mode-error-display-handler]
                  [current-session-id    session-id]
                  [current-repl-msg-chan (make-channel)])
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
                            extra-submods-to-run
                            mem-limit
                            pretty-print?
                            columns
                            pixels/char
                            context-level
                            cmd-line-args
                            debug-files
                            ready-thunk)   cfg)
  (define-values (dir file mod-path) (maybe-mod->dir/file/rmp maybe-mod))
  ;; Set current-directory -- but not current-load-relative-directory,
  ;; see #492 -- to the source file's directory.
  (current-directory dir)
  ;; Make src-loc->string provide full pathnames
  (prevent-path-elision-by-srcloc->string)
  ;; Custodian for the REPL.
  (define repl-cust (make-custodian))
  (when (< 0 mem-limit)
    (custodian-limit-memory repl-cust
                            (inexact->exact (round (* 1024 1024 mem-limit)))
                            repl-cust))

  ;; repl-thunk loads the user program and enters read-eval-print-loop
  (define (repl-thunk)
    ;; 0. Command line arguments
    (current-command-line-arguments cmd-line-args)
    ;; 1. Set print hooks and output handlers
    (set-print-parameters pretty-print? columns pixels/char)
    (set-output-handlers)
    ;; 2. Record as much info about our session as we can, before
    ;; possibly entering module->namespace.
    (set-session! (current-session-id) maybe-mod #f)
    ;; 3. If module, require and enter its namespace, etc.
    (with-expanded-syntax-caching-evaluator
      (when (and maybe-mod mod-path)
        ;; When exn:fail during module load, re-run.
        (define (load-exn-handler exn)
          (display-exn exn)
          (channel-put (current-repl-msg-chan)
                       (struct-copy run-config cfg [maybe-mod #f]))
          (sync never-evt)) ;manager thread will shutdown custodian
        (with-handlers ([exn? load-exn-handler])
          (with-stack-checkpoint
            #;(maybe-configure-runtime mod-path) ;FIRST: see #281
            (namespace-require mod-path)
            (for ([submod (in-list extra-submods-to-run)])
              (define submod-spec `(submod ,(build-path dir file) ,@submod))
              (when (module-declared? submod-spec)
                (dynamic-require submod-spec #f)))
            ;; User's program may have changed current-directory;
            ;; use parameterize to set for module->namespace but
            ;; restore user's value for REPL.
            (current-namespace
             (parameterize ([current-directory dir])
               (module->namespace mod-path)))
            (maybe-warn-about-submodules mod-path context-level)
            (check-#%top-interaction)))))
    ;; 4. Update information about our session -- now that
    ;; current-namespace is possibly updated, and, it is OK to
    ;; call get-repl-submit-predicate.
    (set-session! (current-session-id)
                  maybe-mod
                  (get-repl-submit-predicate maybe-mod))
    ;; 5. Now that the program has run, and `sessions` is updated,
    ;; call the ready-thunk. On REPL startup this lets us wait
    ;; sending the repl-session-id until `sessions` is updated.
    ;; And for subsequent run commands, this lets us it wait to
    ;; send a response.
    (ready-thunk)
    ;; 6. read-eval-print-loop
    (parameterize ([current-prompt-read (make-prompt-read maybe-mod)])
      ;; Note that read-eval-print-loop catches all non-break
      ;; exceptions.
      (read-eval-print-loop)))

  ;; Create thread for repl-thunk
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
         [compile-enforce-module-constants (eq? context-level 'low)]
         [compile-context-preservation-enabled (not (eq? context-level 'low))]
         [current-eval
          (cond [(debug-level? context-level) (make-debug-eval-handler debug-files)]
                [(instrument-level? context-level)(make-instrumented-eval-handler)]
                [else (let ([oe (current-eval)]) (λ (e) (with-stack-checkpoint (oe e))))])]
         [instrumenting-enabled (instrument-level? context-level)]
         [profiling-enabled (eq? context-level 'profile)]
         [test-coverage-enabled (eq? context-level 'coverage)])
      ;; Run repl-thunk on a plain thread, or, on GUI eventspace
      ;; thread via queue-callback. Return the thread.
      (define current-eventspace (txt/gui (make-parameter #f) current-eventspace))
      (parameterize ([current-eventspace ((txt/gui void make-eventspace))])
        (define t/v ((txt/gui thread    queue-callback           ) repl-thunk))
        (define thd ((txt/gui (λ _ t/v) eventspace-handler-thread) (current-eventspace)))
        thd)))

  (define (clean-up-and-run some-cfg)
    (case context-level
      [(profile)  (clear-profile-info!)]
      [(coverage) (clear-test-coverage-info!)])
    (custodian-shutdown-all repl-cust)
    (fresh-line)
    (do-run some-cfg))

  ;; While the repl thread is in read-eval-print-loop, here on the
  ;; repl session thread we wait for messages via repl-msg-chan. Also
  ;; catch breaks, in which case we (a) break the REPL thread so
  ;; display-exn runs there, and (b) continue from our break.
  (let get-message ()
    (define message
      (call-with-exception-handler
       (match-lambda
         [(and (or (? exn:break:terminate?) (? exn:break:hang-up?)) e) e]
         [(exn:break _msg _marks continue) (break-thread repl-thread) (continue)]
         [e e])
       (λ () (sync (current-repl-msg-chan)))))
    (match message
      [(? run-config? c) (clean-up-and-run c)]
      [(zero-column ch)  (zero-column!)
                         (channel-put ch 'done)
                         (get-message)]
      [v (log-racket-mode-warning "ignoring unknown repl-msg-chan message: ~v" v)
         (get-message)])))

(define/contract ((make-prompt-read m))
  (-> (or/c #f mod?) (-> any))
  (begin0 (get-interaction (maybe-mod->prompt-string m))
    ;; let debug-instrumented code break again
    (next-break 'all)))

;; <https://docs.racket-lang.org/tools/lang-languages-customization.html#(part._.R.E.P.L_.Submit_.Predicate)>
(define drracket:submit-predicate/c (-> input-port? boolean? boolean?))
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

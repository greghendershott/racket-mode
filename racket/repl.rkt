#lang racket/base
;; Do NOT use `at-exp` in this file! See issue #290.

(require racket/contract
         racket/format
         racket/match
         (only-in racket/path path-only file-name-from-path)
         racket/set
         (only-in racket/string string-join)
         racket/tcp
         (only-in "debug.rkt" make-debug-eval-handler next-break)
         "elisp.rkt"
         "error.rkt"
         "gui.rkt"
         "instrument.rkt"
         "interactions.rkt"
         "print.rkt"
         "repl-session.rkt"
         "stack-checkpoint.rkt"
         (only-in "syntax.rkt" with-expanded-syntax-caching-evaluator)
         "util.rkt")

(provide start-repl-session-server
         repl-tcp-port-number
         run
         break-repl-thread
         repl-zero-column
         maybe-module-path->file)

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

;; Attributes that may vary for each run.
(define-struct/contract run-config
  ([maybe-mod       (or/c #f module-path?)]
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
  (define mod-path
    (match what
      [(cons (? complete-path? path-string) submods)
       (define path (simplify-path (string->path path-string)))
       (if (null? submods)
           path
           (list* 'submod path submods))]))
  (define ready-channel (make-channel))
  (channel-put (current-repl-msg-chan)
               (run-config mod-path
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

(define/contract (maybe-module-path->file m)
  (-> (or/c #f module-path?) path?)
  (match m
    [(? path? p)                   p]
    [(list* 'submod (? path? p) _) p]
    [#f                            (current-directory)]))

(define/contract (maybe-module-path->prompt-string m)
  (-> (or/c #f module-path?) string?)
  (define (name p)
    (~a (file-name-from-path p)))
  (match m
    [(? path? p)          (name p)]
    [(list* 'submod p xs) (string-join (cons (name p) (map ~a xs)) "/")]
    [#f                   ""]))

;;; REPL session server

(define repl-tcp-port-number #f)

(define (start-repl-session-server launch-token accept-host tcp-port)
  (define listener
    (tcp-listen tcp-port ;0 == choose port dynamically, a.k.a. "ephemeral" port
                64
                (not (zero? tcp-port)) ;reuse not good for ephemeral ports
                accept-host))
  (set! repl-tcp-port-number
        (let-values ([(_loc-addr port _rem-addr _rem-port) (tcp-addresses listener #t)])
          port))
  (log-racket-mode-info "Accepting TCP connections from host ~v on port ~v"
                        accept-host
                        repl-tcp-port-number)
  (thread (listener-thread-thunk launch-token listener)))

(define ((listener-thread-thunk launch-token listener))
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
          (file-stream-buffer-mode in (if (eq? (system-type) 'windows)
                                          'none
                                          'block)) ;#582
          (file-stream-buffer-mode out 'none)
          ;; Immediately after connecting, the client must send us
          ;; exactly the same launch token value that it gave us as a
          ;; command line argument when it started us. Else we close
          ;; the connection. See issue #327.
          (define supplied-token (elisp-read in))
          (unless (equal? launch-token supplied-token)
            (log-racket-mode-fatal "Authorization failed: ~v"
                                   supplied-token)
            (exit 'racket-mode-repl-auth-failure))
          (port-count-lines! in)  ;but for #519 #556 see interactions.rkt
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
        (elisp-writeln `(ok ,session-id))
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
  (define file (maybe-module-path->file maybe-mod))
  (define dir (path-only file))
  ;; Set current-directory -- but not current-load-relative-directory,
  ;; see #492 -- to the source file's directory.
  (current-directory dir)
  ;; Make srcloc->string provide full pathnames
  (prevent-path-elision-by-srcloc->string)
  ;; Custodian for the REPL.
  (define repl-cust (make-custodian))
  (when (< 0 mem-limit)
    (custodian-limit-memory repl-cust
                            (inexact->exact (round (* 1024 1024 mem-limit)))
                            repl-cust))

  ;; repl-thunk loads the user program and enters read-eval-print-loop
  (define (repl-thunk)
    ;; Command line arguments
    (current-command-line-arguments cmd-line-args)
    ;; Set print hooks and output handlers
    (set-print-parameters pretty-print? columns pixels/char)
    (set-output-handlers)
    ;; Record as much info about our session as we can, before
    ;; possibly entering module->namespace.
    (set-session! (current-session-id) maybe-mod #f)
    ;; Set our initial value for current-namespace. When no module,
    ;; this will be the ns used in the REPL. Otherwise this is simply
    ;; the intial ns used for module->namespace, below, which returns
    ;; what we will set as current-namespace for the REPL.
    (current-namespace (make-initial-repl-namespace))
    ;; Now that the initial current-namespace is set, set some
    ;; parameters related to evaluation.
    (compile-enforce-module-constants (eq? context-level 'low))
    (compile-context-preservation-enabled (not (eq? context-level 'low)))
    (current-eval
     [cond [(debug-level? context-level) (make-debug-eval-handler debug-files)]
           [(instrument-level? context-level)(make-instrumented-eval-handler)]
           [else (let ([oe (current-eval)]) (λ (e) (with-stack-checkpoint (oe e))))]])
    (instrumenting-enabled (instrument-level? context-level))
    (profiling-enabled (eq? context-level 'profile))
    (test-coverage-enabled (eq? context-level 'coverage))
    ;; If module, require and enter its namespace, etc.
    (when maybe-mod
      (with-expanded-syntax-caching-evaluator
        (with-handlers (;; When exn during module load, display it,
                        ;; ask the manager thread to re-run, and wait
                        ;; for it to shut down our custodian.
                        [exn?
                         (λ (exn)
                           ((error-display-handler) (exn-message exn) exn)
                           (channel-put (current-repl-msg-chan)
                                        (struct-copy run-config cfg [maybe-mod #f]))
                           (sync never-evt))])
          (with-stack-checkpoint
            ;; First require the module so that if it has any errors
            ;; we stop here.
            (namespace-require maybe-mod)
            ;; Require desired extra submodules (e.g. main, test).
            (for ([submod (in-list extra-submods-to-run)])
              (define submod-spec `(submod ,file ,@submod))
              (when (module-declared? submod-spec)
                (dynamic-require submod-spec #f)))
            ;; configure-runtime important for e.g. Typed Racket REPL.
            ;; Do before we set current-namespace; see #281. On the
            ;; other hand, we don't want to do before
            ;; namespace-require, or we might get duplicate Typed
            ;; Racket error messages printed, as a result of it
            ;; directly calling error-display-handler for each type
            ;; check fail before raising a single exn:fail:syntax.
            (maybe-configure-runtime maybe-mod)
            ;; User's program may have changed current-directory;
            ;; use parameterize to set for module->namespace but
            ;; restore user's value for REPL.
            (current-namespace
             (parameterize ([current-directory dir])
               (module->namespace maybe-mod)))
            (check-#%top-interaction)))))
    ;; Update information about our session -- now that
    ;; current-namespace is possibly updated, and, it is OK to call
    ;; get-repl-submit-predicate.
    (set-session! (current-session-id)
                  maybe-mod
                  (get-repl-submit-predicate maybe-mod))
    ;; Now that user's program has run, and `sessions` is updated,
    ;; call the ready-thunk. On REPL session startup this lets us
    ;; postpone sending the repl-session-id until `sessions` is
    ;; updated. And for subsequent run commands, this lets us it wait
    ;; to send a response, which is useful for commands that want to
    ;; run after a run command has finished.
    (ready-thunk)
    ;; And finally, enter read-eval-print-loop.
    (parameterize ([current-prompt-read (make-prompt-read maybe-mod)])
      ;; Note that read-eval-print-loop catches all non-break
      ;; exceptions.
      (read-eval-print-loop)))

  ;; Create thread to run repl-thunk
  (define repl-thread
    (parameterize ([current-custodian repl-cust])
      ;; Run repl-thunk on a plain thread, or, on GUI eventspace
      ;; thread via queue-callback. Return the thread.
      (define current-eventspace (txt/gui (make-parameter #f) current-eventspace))
      (parameterize ([current-eventspace ((txt/gui void make-eventspace))])
        (define t/v ((txt/gui thread    queue-callback           ) repl-thunk))
        (define thd ((txt/gui (λ _ t/v) eventspace-handler-thread) (current-eventspace)))
        thd)))

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
      [(? run-config? c) (case context-level
                           [(profile)  (clear-profile-info!)]
                           [(coverage) (clear-test-coverage-info!)])
                         (custodian-shutdown-all repl-cust)
                         (fresh-line)
                         (do-run c)]
      [(zero-column ch)  (zero-column!)
                         (channel-put ch 'done)
                         (get-message)]
      [v (log-racket-mode-warning "ignoring unknown repl-msg-chan message: ~v" v)
         (get-message)])))

(define ((make-prompt-read m))
  (begin0 (get-interaction (maybe-module-path->prompt-string m))
    ;; let debug-instrumented code break again
    (next-break 'all)))

;; <https://docs.racket-lang.org/tools/lang-languages-customization.html#(part._.R.E.P.L_.Submit_.Predicate)>
(define drracket:submit-predicate/c (-> input-port? boolean? boolean?))
(define/contract (get-repl-submit-predicate m)
  (-> (or/c #f module-path?) (or/c #f drracket:submit-predicate/c))
  (and m
       (or (with-handlers ([exn:fail? (λ _ #f)])
             (match (with-input-from-file (maybe-module-path->file m)
                      read-language)
               [(? procedure? get-info)
                (match (get-info 'drracket:submit-predicate #f)
                  [#f #f]
                  [v  v])]
               [_ #f]))
           (with-handlers ([exn:fail? (λ _ #f)])
             (match (module->language-info m #t)
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

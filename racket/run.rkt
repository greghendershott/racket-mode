#lang racket/base

(require racket/cmdline
         racket/contract/base
         racket/contract/region
         racket/format
         racket/match
         racket/set
         racket/string
         "channel.rkt"
         "command-server.rkt"
         (only-in "debug.rkt" make-debug-eval-handler)
         "elisp.rkt"
         "error.rkt"
         "gui.rkt"
         "instrument.rkt"
         "interactions.rkt"
         "logger.rkt"
         "mod.rkt"
         "namespace.rkt"
         "print.rkt"
         (prefix-in stx-cache: "syntax.rkt")
         "util.rkt")

;; Main moving parts:
;;
;; 1. This main thread, which receives a couple messages on a channel
;;    (see channel.rkt). One message is a `rerun` struct with info
;;    about a new file/module to run. The main thread loops forever
;;    (the `run` function tail calls itself forever). The special case
;;    of racket/gui/base is handled with a custom module names
;;    resolver and another message.
;;
;; 2. A thread created for each run; loads a module and goes into
;;    a read-eval-print-loop.
;;
;; 3. A thread for a command server that listens on a TCP port (see
;;    command-server.rkt). One of the commands is a `run` command.

(module+ main
  (define-values (command-port launch-token run-info)
    (match (current-command-line-arguments)
      [(vector port)
       (values (string->number port)
               #f
               rerun-default)]
      [(vector port launch-token run-command)
       (values (string->number port)
               (elisp-read (open-input-string launch-token))
               (match (elisp-read (open-input-string run-command))
                 [(list 'run what mem pp ctx args dbgs skel)
                  (rerun (->mod/existing what)
                         mem
                         (as-racket-bool pp)
                         ctx
                         (list->vector args)
                         (list->set (map string->path dbgs))
                         (as-racket-bool skel)
                         void)]
                 [v (eprintf "Bad command-line arguments: ~v => ~v\n" run-command v)
                    (exit)]))]
      [v
       (eprintf "Bad command-line arguments: ~v\n" v)
       (exit)]))
  (start-command-server command-port launch-token)
  (start-logger-server (add1 command-port) launch-token)
  ;; Emacs on Windows comint-mode needs buffering disabled.
  (when (eq? (system-type 'os) 'windows)
    (file-stream-buffer-mode (current-output-port) 'none))
  (display (banner))
  (flush-output)
  (parameterize ([error-display-handler our-error-display-handler])
    (run run-info)))

(define (run rr) ;rerun? -> void?
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
      ;; repl-thunk will be called from another thread -- either a plain
      ;; thread when racket/gui/base is not (yet) instantiated, or, from
      ;; (eventspace-handler-thread (current-eventspace)).
      (define (repl-thunk)
        ;; 0. Command line arguments
        (current-command-line-arguments cmd-line-args)
        ;; 1. Set current-print and pretty-print hooks.
        (set-print-parameters pretty-print?)
        ;; 2. If module, require and enter its namespace, etc.
        (stx-cache:before-run maybe-mod)
        (when (and maybe-mod mod-path)
          (parameterize ([current-module-name-resolver module-name-resolver-for-run]
                         [current-eval (stx-cache:make-eval-handler maybe-mod)])
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
              (check-top-interaction))))
        (stx-cache:after-run maybe-mod)
        ;; 3. Tell command server to use our namespace and module.
        (attach-command-server (current-namespace) maybe-mod)
        ;; 3b. And call the ready-thunk command-server gave us from a
        ;; run command, so that it can send a response for the run
        ;; command. Because the command server runs on a different
        ;; thread, it is probably waiting with (sync some-channel) and
        ;; the thunk will simply channel-put.
        (ready-thunk)
        ;; 4. read-eval-print-loop
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
    [(? rerun? new-rr) (run new-rr)]
    [(load-gui repl?)  (require-gui repl?) (run rr)]))

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

(define (check-top-interaction)
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

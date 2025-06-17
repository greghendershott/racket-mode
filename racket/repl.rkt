;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base
;; Do NOT use `at-exp` in this file! See issue #290.

(require racket/contract
         racket/format
         racket/match
         (only-in racket/path path-only file-name-from-path)
         racket/set
         (only-in racket/string string-join)
         (only-in "debug.rkt" make-debug-eval-handler debug-enable-step)
         "elisp.rkt"
         "error.rkt"
         "gui.rkt"
         "interaction.rkt"
         "instrument.rkt"
         "print.rkt"
         "repl-output.rkt"
         "repl-session.rkt"
         "stack-checkpoint.rkt"
         (only-in "syntax.rkt" make-caching-load/use-compiled-handler)
         "util.rkt")

(provide repl-start
         repl-exit
         run
         repl-break
         repl-submit
         repl-input
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
(define (repl-start sid)
  (when (get-session sid)
    (error 'repl-start "session already exists with id ~a" sid))
  (define ready-ch (make-channel))
  (thread (repl-manager-thread-thunk sid ready-ch))
  (sync ready-ch))

(define (repl-exit)
  (unless (current-repl-msg-chan)
    (error 'repl-exit "No REPL session to exit"))
  (channel-put (current-repl-msg-chan) 'exit))

;; Command. Called from a command-server thread
(define (repl-break)
  (unless (current-repl-msg-chan)
    (error 'repl-break "No REPL session to break"))
  (channel-put (current-repl-msg-chan) 'break))

;; Command. Called from a command-server thread
(define/contract (repl-submit str echo)
  (-> string? any/c any)
  (unless (current-submissions)
    (error 'repl-submit "No REPL session for submit"))
  (channel-put (current-submissions) (cons str (as-racket-bool echo))))

;; Command. Called from a command-server thread
(define/contract (repl-input str)
  (-> string? any)
  (unless (current-repl-msg-chan)
    (error 'repl-input "No REPL session for input"))
  (channel-put (current-repl-msg-chan) `(input ,(string->bytes/utf-8 str))))

;; Command. Called from a command-server thread
(define/contract (run what subs mem pp cols pix/char ctx args dbgs)
  (-> list? (listof (listof symbol?)) number? elisp-bool/c number? number? context-level? list? (listof path-string?)
      list?)
  (unless (current-repl-msg-chan)
    (error 'run "current-repl-msg-chan was #f; current-session-id=~s"
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

;;; REPL sessions

(define ((repl-manager-thread-thunk session-id ready-ch))
  (log-racket-mode-info "starting repl session ~s" session-id)
  ;; Make pipe for user program input (as distinct form repl-submit
  ;; input).
  (parameterize* ([current-session-id          session-id]
                  [current-repl-output-manager (make-repl-output-manager session-id)]
                  [current-repl-msg-chan       (make-channel)]
                  [current-submissions         (make-channel)]
                  [error-display-handler       racket-mode-error-display-handler])
    (set-session! session-id #f)
    (do-run
     (initial-run-config
      (λ ()
        (channel-put ready-ch #t)
        (repl-output-message (banner)))))))

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
  (repl-output-run (maybe-module-path->prompt-string maybe-mod))
  (define file (maybe-module-path->file maybe-mod))
  (define dir (path-only file))
  ;; Set current-directory -- but not current-load-relative-directory,
  ;; see #492 -- to the source file's directory.
  (current-directory dir)
  ;; Custodian for the REPL.
  (define repl-cust (make-custodian))
  (when (< 0 mem-limit)
    (custodian-limit-memory repl-cust
                            (inexact->exact (round (* 1024 1024 mem-limit)))
                            repl-cust))
  (define (our-exit [_v #f])
    (repl-output-exit)
    (custodian-shutdown-all repl-cust)
    (remove-session! (current-session-id)))
  (exit-handler our-exit)

  ;; Input for user program (as distinct from REPL submissions, for
  ;; which see current-submissions and get-interaction).
  (define-values (user-pipe-in user-pipe-out) (make-pipe #f 'repl))

  ;; repl-thunk loads the user program and enters read-eval-print-loop
  (define (repl-thunk)
    ;; Command line arguments
    (current-command-line-arguments cmd-line-args)
    ;; Set ports
    (current-input-port user-pipe-in)
    (current-output-port (make-repl-output-port))
    (current-error-port  (make-repl-error-port))
    (when pretty-print?
      (global-port-print-handler
       (make-pretty-global-port-print-handler columns pixels/char)))
    ;; Record as much info about our session as we can, before
    ;; possibly entering module->namespace.
    (set-session! (current-session-id) maybe-mod)
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
     (cond [(debug-level? context-level) (make-debug-eval-handler debug-files)]
           [(instrument-level? context-level) (make-instrumented-eval-handler)]
           [else (let ([oe (current-eval)]) (λ (e) (with-stack-checkpoint (oe e))))]))
    (instrumenting-enabled (instrument-level? context-level))
    (profiling-enabled (eq? context-level 'profile))
    (test-coverage-enabled (eq? context-level 'coverage))
    (current-load/use-compiled (make-caching-load/use-compiled-handler))
    ;; If module, require and enter its namespace, etc.
    (when maybe-mod
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
          (configure/require/enter maybe-mod extra-submods-to-run dir)
          (check-#%top-interaction))))
    ;; Update information about our session -- now that
    ;; current-namespace is possibly updated.
    (set-session! (current-session-id) maybe-mod)
    ;; Now that user's program has run, and `sessions` is updated,
    ;; call the ready-thunk: useful for commands that want to run
    ;; after a run command has finished.
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
  ;; repl session thread we wait for messages via repl-msg-chan.
  (let get-message ()
    (match (sync (current-repl-msg-chan))
      [(? run-config? c) (case context-level
                           [(profile)  (clear-profile-info!)]
                           [(coverage) (clear-test-coverage-info!)])
                         (custodian-shutdown-all repl-cust)
                         (do-run c)]
      ['break            (break-thread repl-thread #f)
                         (get-message)]
      [`(input ,bstr)    (write-bytes bstr user-pipe-out)
                         (get-message)]
      ['exit             (our-exit)]
      [v (log-racket-mode-warning "ignoring unknown repl-msg-chan message: ~s" v)
         (get-message)])))

(define (make-prompt-read maybe-mod)
  (define (racket-mode-prompt-read)
    (define prompt (maybe-module-path->prompt-string maybe-mod))
    (define stx (get-interaction prompt))
    (debug-enable-step) ;let debug-instrumented code break again
    stx)
  racket-mode-prompt-read)

;; Change one of our non-false maybe-mod values (for which we use path
;; objects, not path-strings) into a module-path applied to
;; module-path-index-join.
(define (->module-path mod)
  (match mod
    [(? path? p)
     (module-path-index-join `(file ,(path->string p))
                             #f)]
    [(list* 'submod (? path? p) subs)
     (module-path-index-join `(submod "." ,@subs)
                             (module-path-index-join `(file ,(path->string p))
                                                     #f))]
    [_ (error "can't make module path from" mod)]))

(define (configure/require/enter mod extra-submods-to-run dir)
  (define mp (->module-path mod))
  (configure-runtime mp)
  (namespace-require mp)
  (for ([submod (in-list extra-submods-to-run)]) ;e.g. main, test
    (define sub-mp (module-path-index-join `(submod "." ,@submod) mp))
    (when (module-declared? sub-mp)
      (dynamic-require sub-mp #f)))
  ;; User's program may have changed current-directory, so
  ;; parameterize for module->namespace, restoring user value for
  ;; REPL.
  (current-namespace (parameterize ([current-directory dir])
                       (module->namespace mp))))

;; From racket-lang/racket/src/cs/main.sps
(define (configure-runtime m)
  ;; New-style configuration through a `configure-runtime` submodule:
  (let ([config-m (module-path-index-join '(submod "." configure-runtime) m)])
    (when (module-declared? config-m #t)
      (dynamic-require config-m #f)))
  ;; Old-style configuration with module language info:
  (let ([info (module->language-info m #t)])
    (when (and (vector? info) (= 3 (vector-length info)))
      (let* ([info-load (lambda (info)
                          ((dynamic-require (vector-ref info 0) (vector-ref info 1))
                           (vector-ref info 2)))]
             [get (info-load info)]
             [infos (get 'configure-runtime '())])
        (unless (and (list? infos)
                     (andmap (lambda (info) (and (vector? info) (= 3 (vector-length info))))
                             infos))
          (raise-argument-error 'runtime-configure "(listof (vector any any any))" infos))
        (for-each info-load infos)))))

(define (check-#%top-interaction)
  ;; Check that the lang defines #%top-interaction
  (unless (memq '#%top-interaction (namespace-mapped-symbols))
    (repl-output-message
     "Because the language used by this module provides no #%top-interaction\n you will be unable to evaluate expressions here in the REPL.")))

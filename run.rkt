#lang at-exp racket/base

(require racket/cmdline
         racket/contract/base
         racket/contract/region
         racket/format
         racket/match
         racket/runtime-path
         racket/pretty
         "channel.rkt"
         "cmds.rkt"
         "error.rkt"
         "gui.rkt"
         "instrument.rkt"
         "logger.rkt"
         "mod.rkt"
         "namespace.rkt"
         "util.rkt")

(module+ main
  (match (current-command-line-arguments)
    [(vector port) (start-command-server (string->number port))
                   (start-logger-server (add1 (string->number port)))]
    [v             (displayln "Expected exactly one argument: command port")
                   (exit)])
  ;; Emacs on Windows comint-mode needs buffering disabled.
  (when (eq? (system-type 'os) 'windows)
    (file-stream-buffer-mode (current-output-port) 'none))
  (display (banner))
  (flush-output)
  (parameterize ([error-display-handler our-error-display-handler])
    (run rerun-default)))

(define (run rr) ;rerun? -> void?
  (match-define (rerun maybe-mod
                       mem-limit
                       pretty-print?
                       context-level
                       cmd-line-args) rr)
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
         [current-eval (if (instrument-level? context-level)
                           (make-instrumented-eval-handler (current-eval))
                           (current-eval))]
         [instrumenting-enabled (instrument-level? context-level)]
         [profiling-enabled (eq? context-level 'profile)]
         [test-coverage-enabled (eq? context-level 'coverage)]
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
        (current-print (make-print-handler pretty-print?))
        (pretty-print-print-hook (make-pretty-print-print-hook))
        (pretty-print-size-hook (make-pretty-print-size-hook))
        ;; 2. If module, require and enter its namespace, etc.
        (when (and maybe-mod mod-path)
          (parameterize ([current-module-name-resolver repl-module-name-resolver])
            ;; When exn:fail? during module load, re-run with "empty"
            ;; module. Note: Unlikely now that we're using
            ;; dynamic-require/some-namespace.
            (with-handlers ([exn? (λ (x)
                                    (display-exn x)
                                    (put/stop (struct-copy rerun rr [maybe-mod #f])))])
              (current-namespace (dynamic-require/some-namespace maybe-mod))
              (maybe-warn-about-submodules mod-path context-level)
              (maybe-load-language-info mod-path)
              (check-top-interaction))))
        ;; 3. Tell command server to use our namespace and module.
        (attach-command-server (current-namespace) maybe-mod)
        ;; 4. read-eval-print-loop
        (parameterize ([current-prompt-read (make-prompt-read maybe-mod)]
                       [current-module-name-resolver repl-module-name-resolver])
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
  (define msg
    (call-with-exception-handler
     (match-lambda
       [(and (or (? exn:break:terminate?) (? exn:break:hang-up?)) e) e]
       [(exn:break msg marks continue) (break-thread repl-thread) (continue)]
       [e e])
     (λ () (sync main-channel))))
  (match context-level
    ['profile  (clear-profile-info!)]
    ['coverage (clear-test-coverage-info!)]
    [_         (void)])
  (custodian-shutdown-all repl-cust)
  (newline) ;; FIXME: Move this to racket-mode.el instead?
  (match msg
    [(? rerun? x)  (run x)]
    [(? load-gui?) (require-gui) (run rr)]))

(define (maybe-load-language-info path)
  ;; Load language-info (if any) and do configure-runtime.
  ;; Important for langs like Typed Racket.
  (with-handlers ([exn:fail? void])
    (define info (module->language-info path #t))
    (when info
      (define get-info ((dynamic-require (vector-ref info 0)
                                         (vector-ref info 1))
                        (vector-ref info 2)))
      (define configs (get-info 'configure-runtime '()))
      (for ([config (in-list configs)])
        ((dynamic-require (vector-ref config 0)
                          (vector-ref config 1))
         (vector-ref config 2))))
    (define cr-submod `(submod ,@(match path
                                   [(list 'submod sub-paths ...) sub-paths]
                                   [_ (list path)])
                        configure-runtime))
    (when (module-declared? cr-submod)
      (dynamic-require cr-submod #f))))

(define (check-top-interaction)
  ;; Check that the lang defines #%top-interaction
  (unless (memq '#%top-interaction (namespace-mapped-symbols))
    (display-commented
     @~a{Because the language used by this module provides no `#%top-interaction'
         you will be unable to evaluate expressions here in the REPL.})))

;; Catch attempt to load racket/gui/base for the first time.
(define repl-module-name-resolver
  (let ([orig-resolver (current-module-name-resolver)])
    (case-lambda
      [(rmp ns)
       (orig-resolver rmp ns)]
      [(mp rmp stx)
       (repl-module-name-resolver mp rmp stx #t)]
      [(mp rmp stx load?)
       (when (and load? (memq mp '(racket/gui/base
                                   racket/gui/dynamic
                                   scheme/gui/base)))
         (unless (gui-required?)
           (put/stop (load-gui))))
       (orig-resolver mp rmp stx load?)])))

;; Note: The `dynamic-require`s seem to be necessary otherwise
;; file/convertible's convertible? always returns #f. Which seeems to
;; be a namespace issue that I don't understand.
(define-runtime-path image.rkt "image.rkt")

(define (make-print-handler pretty-print?)
  (cond [pretty-print? pretty-print-handler]
        [else (make-plain-print-handler)]))

(define (make-plain-print-handler)
  (let ([convert (dynamic-require image.rkt 'convert-image)])
    (λ (v)
      (void (unless (void? v)
              (print (convert v))
              (newline))))))

(define (make-pretty-print-size-hook [orig (pretty-print-size-hook)])
  (let ([convert? (dynamic-require image.rkt 'convert-image?)]
        [width (floor (/ (pretty-print-columns) 4))]) ;magic number? yep.
    (λ (value display? port)
      (cond [(convert? value) width]
            [else (orig value display? port)]))))

(define (make-pretty-print-print-hook [orig (pretty-print-print-hook)])
  (let ([convert? (dynamic-require image.rkt 'convert-image?)]
        [convert  (dynamic-require image.rkt 'convert-image)])
    (λ (value display? port)
      (cond [(convert? value) (print (convert value) port)]
            [else (orig value display? port)]))))

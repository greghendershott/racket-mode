#lang racket/base

(require (for-syntax syntax/parse)
         racket/match
         racket/string
         racket/format
         "cmds.rkt"
         "error.rkt"
         "gui.rkt"
         "logger.rkt"
         "util.rkt")

(module+ main
  ;; Emacs on Windows comint-mode needs buffering disabled
  (when (eq? (system-type 'os) 'windows)
    (file-stream-buffer-mode (current-output-port) 'none))
  (display (banner))
  (parameterize ([error-display-handler our-error-display-handler])
    (run #f)))

;; (or/c #f path-string?)
(define (run path-str)
  (define-values (path dir) (path-string->path&dir path-str))
  ;; Always set current-directory and current-load-relative-directory
  ;; to match the source file.
  (current-directory dir)
  (current-load-relative-directory dir)
  ;; Make src-loc->string provide full pathnames
  (show-full-path-in-errors)
  ;; Custodian for the user REPL.
  (define user-cust (make-custodian))
  ;; If racket/gui/base isn't loaded, the current-eventspace parameter
  ;; doesn't exist, so make a "dummy" parameter of that name.
  (define current-eventspace (txt/gui (make-parameter #f) current-eventspace))
  (parameterize*
      ([current-custodian user-cust]
       ;; Use parameterize* so that this value...
       [current-namespace ((txt/gui make-base-namespace make-gui-namespace))]
       ;; ...is in effect when setting this:
       [current-eventspace ((txt/gui void make-eventspace))]
       [compile-enforce-module-constants #f]
       [compile-context-preservation-enabled #t])
    ;; repl-thunk will be called from another thread -- either a plain
    ;; thread when racket/gui/base is not (yet) instantiated, or, from
    ;; (event-handler-thread (current-eventspace)).
    (define (repl-thunk)
      ;; 1. If module, load its lang info, require, and enter its namespace.
      (when (and path (module-path? path))
        (parameterize ([current-module-name-resolver repl-module-name-resolver])
          ;; exn:fail? during module load => re-run with "empty" module
          (with-handlers ([exn? (λ (x) (display-exn x) (put/stop (rerun #f)))])
            (maybe-load-language-info path)
            (namespace-require path)
            (current-namespace (module->namespace path))
            (check-top-interaction))))
      ;; 2. read-eval-print-loop
      (parameterize ([current-prompt-read (make-prompt-read path)]
                     [current-module-name-resolver repl-module-name-resolver])
        ;; Note that read-eval-print-loop catches all non-break exceptions.
        (read-eval-print-loop)))
    ;; Main thread: Run repl-thunk on a plain thread, or, on the user
    ;; eventspace thread via queue-callback.
    ((txt/gui thread queue-callback) repl-thunk))
  ;; Main thread: Wait for message from REPL thread. Catch breaks.
  (define msg (with-handlers ([exn:break? (lambda (exn) (display-exn exn) 'break)])
                (channel-get ch)))
  (custodian-shutdown-all user-cust)
  (newline) ;; FIXME: Move this to racket-mode.el instead?
  (match msg
    ['break     (run #f)]
    [(rerun p)  (run p)]
    [(load-gui) (require-gui) (run path-str)]))

(define (maybe-load-language-info path)
  ;; Load language-info (if any) and do configure-runtime.
  ;; Important for langs like Typed Racket.
  (define info (module->language-info path #t))
  (when info
    (define get-info ((dynamic-require (vector-ref info 0)
                                       (vector-ref info 1))
                      (vector-ref info 2)))
    (define configs (get-info 'configure-runtime '()))
    (for ([config (in-list configs)])
      ((dynamic-require (vector-ref config 0)
                        (vector-ref config 1))
       (vector-ref config 2)))
    (define cr-submod `(submod ,path configure-runtime))
    (when (module-declared? cr-submod)
      (dynamic-require cr-submod #f))))

(define (check-top-interaction)
  ;; Check that the lang defines #%top-interaction
  (unless (memq '#%top-interaction (namespace-mapped-symbols))
    (error 'repl "The module's language provides no `#%top-interaction' and\ncannot be used in a REPL.")))

;; Messages via the channel from the repl thread to the main thread.
(define ch (make-channel))
(struct rerun (path)) ;(or/c #f path-string?)
(struct load-gui ())

;; To be called from REPL thread. Puts message for the main thread to
;; the channel, and blocks itself; main thread will kill the REPL
;; thread. Net effect: "Exit the thread with a return value".
(define (put/stop v) ;; any/c -> any
  (channel-put ch v)
  (sync never-evt))

;; Catch attempt to load racket/gui/base for the first time.
(define repl-module-name-resolver
  (let ([orig-resolver (current-module-name-resolver)])
    (case-lambda
      [(rmp ns)
       (orig-resolver rmp ns)]
      [(mp rmp stx load?)
       (when (and (eq? mp 'racket/gui/base) load?)
         (unless (gui-required?)
           (put/stop (load-gui))))
       (orig-resolver mp rmp stx load?)])))

;; path-string? -> (values (or/c #f path?) path?)
(define (path-string->path&dir path-str)
  (define path (and path-str
                    (not (equal? path-str ""))
                    (string? path-str)
                    (path-str->existing-file-path path-str)))
  (define dir (cond [path (define-values (base _ __) (split-path path))
                          (cond [(eq? base 'relative) (current-directory)]
                                [else base])]
                    [else (current-directory)]))
  (values path dir))

;; path-string? -> (or/c #f path?)
(define (path-str->existing-file-path path-str)
  (define (not-found s)
    (eprintf "; ~a not found\n" s)
    #f)
  (with-handlers ([exn:fail? (λ (_) (not-found path-str))])
    (define path (expand-user-path (string->path path-str)))
    (cond [(file-exists? path) path]
          [else (not-found (path->string path))])))

(define (make-prompt-read path)
  (define-values (base name dir?) (cond [path (split-path path)]
                                        [else (values "" "" #f)]))
  (λ ()
    (flush-output (current-error-port))
    (display name) (display "> ")
    (define in ((current-get-interaction-input-port)))
    (define stx ((current-read-interaction) (object-name in) in))
    (syntax-case stx ()
      [(uq cmd)
       (eq? 'unquote (syntax-e #'uq))
       (case (syntax-e #'cmd)
         [(run) (put/stop (rerun (~a (read))))]
         [(top) (put/stop (rerun #f))]
         [(def) (def (read))]
         [(doc) (doc (read-line))]
         [(exp) (exp1 (read))]
         [(exp+) (exp+)]
         [(exp!) (exp! (read))]
         [(log) (log-display (map string->symbol (string-split (read-line))))]
         [(pwd) (display-commented (~v (current-directory)))]
         [(cd) (cd (~a (read)))]
         [else (usage)])]
      [_ stx])))

(define (usage)
  (displayln
   "Commands:
,run </path/to/file.rkt>
,top
,def <identifier>
,doc <string>
,exp <stx>
,exp+
,exp! <stx>
,pwd
,cd <path>
,log <opts> ...")
  (void))

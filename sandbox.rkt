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
  (display (banner))
  (run #f))

;; (or/c #f path-string?)
(define (run path-str)
  (define-values (path load-dir) (path-string->path&load-dir path-str))
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
       [error-display-handler our-error-display-handler]
       [compile-enforce-module-constants #f]
       [compile-context-preservation-enabled #t]
       [current-load-relative-directory load-dir])
    ;; repl-thunk will be called from another thread -- either a plain
    ;; thread when racket/gui/base is not (yet) instantiated, or, from
    ;; (event-handler-thread (current-eventspace)).
    (define (repl-thunk)
      ;; 1. If module, require it and enter its namespace
      (when (and path (module-path? path))
        (parameterize ([current-module-name-resolver repl-module-name-resolver])
          ;; exn:fail during module load => re-run
          (with-handlers ([exn:fail? (λ (x) (display-exn x) (put/stop (rerun #f)))])
            (dynamic-require path 0))
          (current-namespace (module->namespace path))))
      ;; 2. read-eval-print-loop
      (parameterize ([current-prompt-read (make-prompt-read path)]
                     [current-module-name-resolver repl-module-name-resolver])
        ;; exn:fail during read-eval-print-loop => more cowbell
        (let repl ()
          (with-handlers ([exn:fail? (λ (exn) (display-exn exn) (repl))])
            (read-eval-print-loop)))))
    ;; Main thread: Run repl-thunk on a plain thread, or, on the user
    ;; eventspace thread via queue-callback.
    ((txt/gui thread queue-callback) repl-thunk))
  ;; Main thread: Wait for message from REPL thread.
  (define msg (channel-get ch))
  (custodian-shutdown-all user-cust)
  (newline) ;; FIXME: Move this to racket-mode.el instead?
  (match msg
    [(rerun p)  (run p)]
    [(load-gui) (require-gui) (run path-str)]))

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
(define (path-string->path&load-dir path-str)
  (define path (and path-str
                    (not (equal? path-str ""))
                    (string? path-str)
                    (path-str->existing-file-path path-str)))
  (define load-dir (cond [path (define-values (base _ __) (split-path path))
                               (cond [(eq? base 'relative) (current-directory)]
                                     [else base])]
                         [else (current-directory)]))
  (values path load-dir))

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
    (let loop ()
      (display name) (display "> ")
      (flush-output (current-error-port))
      (with-handlers ([exn:fail? (λ (exn)
                                   (display-exn exn)
                                   (loop))])
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
             [(exp) (exp1)]
             [(exp+) (exp+)]
             [(exp!) (exp!)]
             [(log) (log-display (map string->symbol (string-split (read-line))))]
             [(pwd) (display-commented (~v (current-directory)))]
             [(cd) (cd (~a (read)))]
             [else stx])]
          [_ stx])))))

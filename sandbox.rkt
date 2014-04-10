#lang racket/base

;; Provide `,run` -- which unlike XREPL's `,enter` -- is closer to
;; DrRacket's Run (F5) command: It completely resets the REPL.

;; Unfortunately this supersedes XREPL and all its handy features. It
;; would be preferable to add this to XREPL. But I'm not sure how to
;; juggle the sandbox approach with what XREPL does, and, I'm not sure
;; if this would be welcome in XREPL. So for now, do this as its own
;; thing.
;;
;; In case it turns out to remain its own thing, I did duplicate
;; _some_ XREPL functionality -- the ,log command. I also make it work
;; with loggers created by 5.3.2's `define-logger` (which is a pull
;; request I did awhile ago that hasn't yet been accepted for XREPL).

(require (for-syntax racket/base
                     syntax/parse)
         racket/sandbox
         racket/match
         racket/format
         racket/string
         racket/list
         racket/pretty
         racket/runtime-path
         syntax/srcloc
         "defn.rkt")

(define (run-file path-str)
  (display (banner))
  (let loop ([path-str path-str])
    (define next (do-run path-str))
    (unless (eq? next 'exit)
      (newline)
      (loop next))))

(struct run-new-sandbox (path)) ;(or/c #f path-string?)

;; do-run :: (or/c #f path-string?) -> (or/c #f path-string? 'exit)
;;
;; Takes a path-string? for a .rkt file, or #f meaning top-level #lang
;; racket.
;;
;; Returns similar path-string? or #f if REPL should be
;; restarted, else 'exit if we should simply exit.
(define (do-run path-str)
  (define-values (path load-dir) (path-string->path&load-dir path-str))
  (call-with-trusted-sandbox-configuration
   (lambda ()
     ;; Need to set some parameters so they're in effect _before_
     ;; creating the sandboxed evaluator. Note that using
     ;; `call-with-trusted-sandbox-configuration` above sets a number
     ;; of parameters to be permissive (e.g. `sandbox-memory-limit`,
     ;; `sandbox-eval-limits`, and `sandbox-security-guard`) so we
     ;; don't need to set them here.
     (parameterize ([current-namespace (make-base-empty-namespace)]
                    [sandbox-input (current-input-port)]
                    [sandbox-output (current-output-port)]
                    [sandbox-error-output (current-error-port)]
                    [sandbox-propagate-exceptions #f]
                    [compile-enforce-module-constants #f]
                    [compile-context-preservation-enabled #t]
                    [current-load-relative-directory load-dir]
                    [current-prompt-read (make-prompt-read path)]
                    [error-display-handler our-error-display-handler])
       (match (make-eval path)
         [(and x (or #f 'exit)) x]
         [e (parameterize ([current-eval e])
              (with-handlers
                  ([exn:fail:sandbox-terminated? (lambda (exn)
                                                   (display-exn exn)
                                                   'exit)]
                   [run-new-sandbox? (lambda (b)
                                       (kill-evaluator (current-eval))
                                       (run-new-sandbox-path b))])
                (our-read-eval-print-loop)))])))))

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
  (with-handlers ([exn:fail? (lambda (_) (not-found path-str))])
    (define path (expand-user-path (string->path path-str)))
    (cond [(file-exists? path) path]
          [else (not-found (path->string path))])))

;; Eval something in sandbox evaluator namespace
(define-syntax (with-sandbox stx)
  (syntax-parse stx
    [(_ e:expr ...+)
     #'(call-in-sandbox-context (current-eval) (lambda () e ...))]))

;; (or/c #f path-string?) -> (or/c #f 'exit evaluator?)
(define (make-eval path)
  ;; Make a module evaluator if non-#f path, else plain
  ;; evaluator.  If exn:fail? creating a module evaluator --
  ;; e.g. it had a syntax error -- return #f saying to try again
  ;; making a plain evaluator. But if exn:fail? creating a plain
  ;; evaluator, return 'exit meaning give up.
  (with-handlers ([exn:fail? (lambda (exn)
                               (display-exn exn)
                               (cond [path #f]
                                     [else 'exit]))])
    (cond [path (make-module-evaluator path)]
          [else (make-evaluator 'racket)])))

(define orig-resolver (current-module-name-resolver))
(current-module-name-resolver
 (case-lambda
   [(rmp ns) (orig-resolver rmp ns)]
   [(mp rmp stx load?)
    (when (and (eq? mp 'racket/gui/base) load?)
      (error 'racket-mode-repl "Not compatible with racket/gui/base."))
   (orig-resolver mp rmp stx load?)]))

(define (make-prompt-read path)
  (define-values (base name dir?) (cond [path (split-path path)]
                                        [else (values "" "" #f)]))
  (lambda ()
    (let loop ()
      (display name) (display "> ")
      (flush-output (current-error-port))
      (with-handlers ([exn:fail? (lambda (exn)
                                   (display-exn exn)
                                   (loop))])
        (define in ((current-get-interaction-input-port)))
        (define stx ((current-read-interaction) (object-name in) in))
        (syntax-case stx ()
          [(uq cmd)
           (eq? 'unquote (syntax-e #'uq))
           (case (syntax-e #'cmd)
             [(run) (raise (run-new-sandbox (~a (read))))]
             [(top) (raise (run-new-sandbox #f))]
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

;; This is almost exactly like Racket's read-eval-print-loop except it
;; does NOT cons #%top-interaction to the read form. Because the
;; racket/sandbox evaluator will do that. (If we did it twice, we'd
;; get double output, e.g. from Typed Racket printing the types
;; twice.)
(define (our-read-eval-print-loop)
  (let repl-loop ()
    ;; This prompt catches all error escapes, including from read and print.
    (call-with-continuation-prompt
     ;; 1. proc
     (lambda ()
       (let ([v ((current-prompt-read))])
         (unless (eof-object? v)
           (call-with-values
               (lambda ()
                 ;; This prompt catches escapes during evaluation.
                 ;; Unlike the outer prompt, the handler prints
                 ;; the results.
                 (call-with-continuation-prompt
                  (lambda ()
                    ((current-eval) v)))) ;just plain v, no #%top-interaction
             (lambda results (for-each (current-print) results)))
           ;; Abort to loop. Calling `repl-loop' directly wouldn't be tail call.
           (abort-current-continuation (default-continuation-prompt-tag)))))
     ;; 2. prompt-tag
     (default-continuation-prompt-tag)
     ;; 3. handler
     (lambda args (repl-loop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (our-error-display-handler str exn)
  (unless (equal? "Check failure" (exn-message exn)) ;rackunit check fails
    (display-commented str)
    (display-srclocs exn)
    (unless (exn:fail:user? exn)
      (display-context exn))))

(define (display-srclocs exn)
  (when (exn:srclocs? exn)
    (let* ([srclocs ((exn:srclocs-accessor exn) exn)]
           ;; Some exns' first srcloc is already in exn-message; skip it
           [srclocs (cond [(or (exn:fail:read? exn)
                               (exn:fail:syntax? exn)
                               (exn:fail:contract:variable? exn))
                           (cdr srclocs)]
                          [else srclocs])])
      (for ([srcloc srclocs])
        (display-commented (source-location->string srcloc))))))

(define (display-context exn)
  (define ctx (continuation-mark-set->context (exn-continuation-marks exn)))
  (match (context->string ctx)
    ["" (void)]
    [s (display-commented "Context:")
       (display-commented s)]))

(define (context->string xs)
  (string-join (for/list ([x xs]
                          [n 10]
                          #:break (system-context? x))
                 (context-item->string x))
               "\n"))

(define-runtime-path sandbox.rkt ".")
(define (system-context? ci)
  (match-define (cons id src) ci)
  (or (not src)
      (let ([src (srcloc-source src)])
        (and (path? src)
             (or (equal? src sandbox.rkt)
                 (under-system-path? src))))))

(define (under-system-path? path)
  (define excluded-collections
    '("typed/racket" "racket/sandbox" "racket/contract" "racket/private"))
  (define-values (dir base _) (split-path path))
  (not (not (for/or ([collection (in-list excluded-collections)])
              (collection-file-path base collection #:fail (lambda _ #f))))))

(module+ test
  (require rackunit)
  (check-true (under-system-path?
   (string->path "/Applications/Racket_v5.93/share/pkgs/typed-racket-lib/typed-racket/tc-setup.rkt"))))

(define (context-item->string ci)
  (match-define (cons id src) ci)
  (string-append (if (or src id) " " "")
                 (if src (source-location->string src) "")
                 (if (and src id) " " "")
                 (if id (format "~a" id) "")))

(define (display-exn exn)
  (our-error-display-handler (exn-message exn) exn))

(define (display-commented str)
  (eprintf "; ~a\n"
           (regexp-replace* "\n" str "\n; ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-log-receiver-thread (make-parameter #f))
(define global-logger (current-logger))
(define other-level 'fatal)

;; Default a couple specific loggers one notch above their "noisy"
;; level. That way, if someone sets "all other" loggers to e.g. debug,
;; these won't get noisy. They need to be specifically cranked up.
(define logger-levels (make-hasheq '([cm-accomplice . warning]
                                     [gc . info])))

(define racket-log-file (build-path (find-system-path 'temp-dir) "racket-log"))
(with-output-to-file racket-log-file #:exists 'truncate void)

(define (update-log-receiver)
  (show-logger-levels) ;handy to show after setting
  (cond [(current-log-receiver-thread) => kill-thread])
  (let* ([args (append (list global-logger)
                       (flatten (for/list ([(k v) logger-levels])
                                  (list v k)))
                       (list other-level))]
         [r (apply make-log-receiver args)])
    (current-log-receiver-thread
     (thread
      (λ ()
         (let loop ()
           (match (sync r)
             [(vector l m v name)
              ;; To stderr
              (eprintf "; [~a] ~a\n" l m)
              (flush-output)
              ;; To /tmp/racket-log (can `tail -f' it)
              (with-output-to-file racket-log-file #:exists 'append
                                   (lambda ()
                                     (display (format "[~a] ~a\n" l m))))])
           (loop)))))))

(define (show-logger-levels)
  (define wid 20)
  (define (pr k v)
    (printf "; ~a ~a\n"
            (~a k
                #:min-width wid
                #:max-width wid
                #:limit-marker "...")
            v))
  (pr "Logger" "Level")
  (pr (make-string wid #\-) "-------")
  (for ([(k v) logger-levels])
    (pr k v))
  (pr "[all other]" other-level)
  (printf "; Writing ~v.\n" racket-log-file))

(define (log-display specs)
  (match specs
    [(list) (show-logger-levels)]
    [(list (and level (or 'none 'fatal 'error 'warning 'info 'debug)))
     (set! other-level level)
     (update-log-receiver)]
    [(list logger 'default)
     (hash-remove! logger-levels logger)
     (update-log-receiver)]
    [(list logger (and level (or 'none 'fatal 'error 'warning 'info 'debug)))
     (hash-set! logger-levels logger level)
     (update-log-receiver)]
    [_ (eprintf
        (string-join
         '("; Usage:"
           ",log                  -- show the levels currently in effect."
           ",log <logger> <level> -- set logger to level debug|info|warning|error|fatal|none"
           ",log <logger> default -- set logger to use the default, 'all other' level."
           ",log <level>          -- set the default level, for 'all other' loggers.\n")
         "\n; "))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define last-stx #f)

(define (exp1)
  (set! last-stx (with-sandbox (expand-once (read))))
  (pp-stx last-stx))

(define (exp+)
  (when last-stx
    (define this-stx (with-sandbox (expand-once last-stx)))
    (cond [(equal? (syntax->datum last-stx) (syntax->datum this-stx))
           (display-commented "Already fully expanded.")
           (set! last-stx #f)]
          [else
           (pp-stx this-stx)
           (set! last-stx this-stx)])))

(define (exp!)
  (set! last-stx #f)
  (with-sandbox (pp-stx (expand (read)))))

(define (pp-stx stx)
  (newline)
  (pretty-print (syntax->datum stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (def sym)
  (with-sandbox
   (display-definition (symbol->string sym))))

(define (doc str)
  (eval `(begin
          (require racket/help)
          (help ,(string-trim str))
          (newline))))

(define (cd s)
  (let ([old-wd (current-directory)])
    (current-directory s)
    (unless (directory-exists? (current-directory))
      (display-commented (format "~v doesn't exist." (current-directory)))
      (current-directory old-wd))
    (display-commented (format "In ~v" (current-directory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (run-file #f))

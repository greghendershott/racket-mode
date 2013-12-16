#lang racket/base

;; Provide `run!`, which (unlike `enter!`) is closer to DrRacket's Run
;; (F5) command: It completely resets the REPL.

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

(provide (rename-out [run-file run!]))
(require racket/sandbox racket/match "defn.rkt")

(define (run-file path-str)
  (display (banner))
  (let loop ([path-str path-str])
    (define next (do-run path-str))
    (unless (eq? next 'exit)
      (newline)
      (loop next))))

(struct exn:run-new-sandbox (path))

;; (or/c #f path-string?) -> (or/c f path-string? 'exit)
;;
;; Takes a path-string? for a .rkt file, or #f meanining empty #lang
;; racket. Returns similar if REPL should be restarted on a new file,
;; else 'exit if we should simply exit.
(define (do-run path-str)
  (define (path-str->existing-file-path path-str) ;path-str? -> (or/c #f path?)
    (with-handlers ([exn:fail? (lambda (_) (eprintf "; ~a not found\n" path-str))])
      (define path (expand-user-path (string->path path-str)))
      (cond [(file-exists? path) path]
            [else (eprintf "; ~a not found\n" (path->string path)) #f])))
  (define path (and (string? path-str)
                    (not (equal? path-str ""))
                    (path-str->existing-file-path path-str)))
  (define load-dir (cond [path (define-values (base _ __) (split-path path))
                               base]
                         [else (current-directory)]))
  (call-with-trusted-sandbox-configuration
   (lambda ()
     ;; Need to set some parameters so they're in effect _before_
     ;; creating the sandboxed evaluator (which is why there's a
     ;; nested `parameterize` below). Note that the
     ;; `call-with-trusted-sandbox-configuration` above sets a number
     ;; of parameters to be permissive (such as
     ;; `sandbox-memory-limit`, `sandbox-eval-limits`, and
     ;; `sandbox-security-guard`) so we don't need to set them here.
     (parameterize ([current-namespace (make-empty-namespace)]
                    [sandbox-input (current-input-port)]
                    [sandbox-output (current-output-port)]
                    [sandbox-error-output (current-error-port)]
                    [sandbox-propagate-exceptions #f]
                    [compile-enforce-module-constants #f]
                    [compile-context-preservation-enabled #t]
                    [current-load-relative-directory load-dir]
                    [current-read-interaction read-interaction]
                    [current-prompt-read (make-prompt-read path)]
                    [error-display-handler -error-display-handler])
       ;; Make a module evaluator (or plain evaluator if path is #f).
       (parameterize ([current-eval (cond [path (make-module-evaluator path)]
                                          [else (make-evaluator 'racket)])])
         (with-handlers ([exn:fail:sandbox-terminated?
                          (lambda (exn)
                            (eprintf "; ~a\n" (exn-message exn))
                             'exit)]
                         [exn:run-new-sandbox?
                          (lambda (b)   ;run another sandbox
                            (kill-evaluator (current-eval))
                            (exn:run-new-sandbox-path b))])
           (read-eval-print-loop)))))))

(define (make-prompt-read path)
  (define-values (base name dir?) (cond [path (split-path path)]
                                        [else (values "" "" #f)]))
  (lambda ()
    (let loop ()
      (display name) (display "> ")
      (define in ((current-get-interaction-input-port)))
      (with-handlers ([exn:fail? (lambda (exn)
                                   ;; Don't exit on read error, just try again
                                   (eprintf "; ~a\n" (exn-message exn))
                                   (loop))])
        ((current-read-interaction) (object-name in) in)))))

;; From xrepl: "Makes it easy to use meta-tools without user-namespace
;; contamination."
(define (eval-sexpr-for-user form)
  (eval (namespace-syntax-introduce (datum->syntax #f form))))

(define (read-interaction src in)
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (define stx (read-syntax src in))
    ;; Check for special commands
    (syntax-case stx (run! log! def! doc!)
      [(run!)
       (raise (exn:run-new-sandbox #f))]
      [(run! str) ;; "module.rkt"
       (string? (syntax-e #'str))
       (raise (exn:run-new-sandbox (syntax-e #'str)))]
      [(run! sym) ;; module
       (symbol? (syntax-e #'sym))
       (raise (exn:run-new-sandbox (symbol->string (syntax-e #'sym))))]
      [(log! specs ...)
       (log-display (syntax->datum #'(specs ...)))]
      [(def! sym)
       (symbol? (syntax-e #'sym))
       (call-in-sandbox-context ;; to use sandbox evaluator's namespace
        (current-eval)
        (lambda ()
          (display-definition (symbol->string (syntax-e #'sym)))))]
      [(doc! sym)
       (symbol? (syntax-e #'sym))
       (eval-sexpr-for-user `(begin
                               (require racket/help)
                               (help ,(namespace-syntax-introduce #'sym))))]
      ;; The usual
      [_ stx])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the reverse of the default error display handler. We show
;; the context "stack" first, from its bottom to top, then the
;; immediate error at the bottom. I prefer that: easier to scan
;; visually from end of buffer. Also, this enables the "click last
;; button" command to find the button for the immediate error's
;; source.

(define (-error-display-handler str exn)
  (define context (continuation-mark-set->context (exn-continuation-marks exn)))
  (eprintf "~a; ~a\n"
           (if (exn:fail:user? exn) "" (context->string context))
           (regexp-replace "\n" str "\n;")))

(define (context->string xs)
  (for/fold ([s ""])
            ([x (in-list (reverse xs))])
    (match-define (cons id src) x)
    (string-append s
                   (if (or src id) "; " "")
                   (if src
                       (let ([source (srcloc-source src)]
                             [line (srcloc-line src)]
                             [col  (srcloc-column src)])
                         (if (and line col)
                             (format "~a:~a:~a"
                                     (if (path? source)
                                         (path->string source)
                                         source)
                                     line
                                     col)
                             (format "~a" source)))
                       "")
                   (if (and src id) " " "")
                   (if id
                       (format "~a" id)
                       "")
                   (if (or src id) "\n" ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-log-receiver-thread (make-parameter #f))
(define global-logger (current-logger))

(define racket-log-file "/tmp/racket-log")
(with-output-to-file racket-log-file #:exists 'truncate void)

(define (log-display specs)
  (cond [(current-log-receiver-thread) => kill-thread])
  (unless (null? specs)
    (let ([r (apply make-log-receiver (list* global-logger specs))])
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
                                      (display (format "[~a] ~a\n" l m))))
               ])
            (loop))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (run-file #f))

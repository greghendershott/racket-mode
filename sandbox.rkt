#lang racket/base

;; Provide `run!`, which (unlike `enter!`) is closer to DrRacket's Run
;; (F5) command: It completely resets the REPL.

;; Unfortunately this supersedes XREPL and all its handy features. It
;; would be preferable to add this to XREPL. But I'm not sure how to
;; juggle the sandbox approach with what XREPL does, and, I'm not sure
;; if this would be welcome in XREPL. So for now, do this as its own
;; thing.
;;
;; If it turns out to remain its own thing, I _may_ want to duplicate
;; _some_ XREPL functionality, such as the ,log command. In the case
;; of `,log`, I could also update it to work with define-logger in
;; Racket 5.3.2, which is a pull request I did awhile ago that hasn't
;; yet been accepted for XREPL. Perhaps there is even some spiffy way
;; to make the logger output go into a dedicated Emacs window, instead
;; of being mixed in with the REPL output.

(require racket/sandbox racket/function)

(define (run-file path-str)
  (display (banner))
  (let loop ([path-str path-str])
    (define next (do-run path-str))
    (unless (eq? next 'exit)
      (loop next))))

;; Takes a path-string? for a .rkt file, or #f meanining empty #lang
;; racket. Returns similar if REPL should be restarted on a new file,
;; else 'exit if we should simply exit.
(define (do-run path-str) ;(or/c #f path-string?) -> (or/c f path-string? 'exit)
  (define (path-str->existing-file-path path-str) ;path-str? -> (or/c #f path?)
    (with-handlers ([exn:fail? (const #f)])
      (define path (expand-user-path (string->path path-str)))
      (cond [(file-exists? path) path]
            [else (eprintf "; ~a not found\n" (path->string path)) #f])))
  (define path (and (string? path-str)
                    (not (equal? path-str ""))
                    (path-str->existing-file-path path-str)))
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
                    [current-read-interaction read-interaction]
                    [current-prompt-read (make-prompt-read path)]
                    [error-display-handler -error-display-handler])
       ;; Make a module evaluator (or plain evaluator if path is #f).
       (parameterize ([current-eval (cond [path (make-module-evaluator path)]
                                          [else (make-evaluator 'racket)])])
         (with-handlers ([exn:fail:sandbox-terminated?
                          (lambda (exn) (eprintf "; ~a\n" (exn-message exn)) 'exit)]
                         [box? (lambda (b) (unbox b))]) ;run another sandbox
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

(define (read-interaction src in)
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (define stx (read-syntax src in))
    ;; Check for special run commands
    (syntax-case stx (run!)
      [(run!)                           ;empty module
       (raise (box #f))]
      [(run! path)                      ;"foo.rkt"
       (string? (syntax-e #'path))
       (raise (box (syntax-e #'path)))]
      [(run! path)                      ;foo.rkt
       (symbol? (syntax-e #'path))
       (raise (box (symbol->string (syntax-e #'path))))]
      ;; The usual
      [_ stx])))

;; Prefix lines with #\; so they appear in comment font-lock
(define (-error-display-handler s exn)
  (let* ([s (regexp-replace* #rx"^\n+|\n+$" s "")]
         [s (regexp-replace* #rx"\n\n+" s "\n")]
         [s (regexp-replace* #rx"\n" s "\n;")])
    (printf "; ~a\n" s)))

;; ;; Example use:
;; (run-file "/Users/greg/src/scheme/misc/hello.rkt"))

(module+ main
  (run-file #f))

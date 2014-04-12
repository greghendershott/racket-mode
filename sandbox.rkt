#lang racket/base

(require (for-syntax syntax/parse)
         racket/match
         racket/string
         racket/pretty
         racket/format
         "defn.rkt"
         "error.rkt"
         "gui.rkt"
         "logger.rkt")

(module+ main
  (display (banner))
  (run #f))

;; (or/c #f path-string?)
(define (run path-str)
  (define-values (path load-dir) (path-string->path&load-dir path-str))
  (define user-cust (make-custodian (current-custodian)))
  (define current-eventspace (txt/gui (make-parameter #f) current-eventspace))
  (define ch (make-channel))
  (parameterize*
      ([current-custodian user-cust]
       [current-namespace ((txt/gui make-base-namespace make-gui-namespace))]
       [current-eventspace ((txt/gui void make-eventspace))]
       [compile-enforce-module-constants #f]
       [compile-context-preservation-enabled #t]
       [current-load-relative-directory load-dir])
    ;; repl-thunk will be called from another thread -- either a plain
    ;; thread when racket/gui/base is not (yet) instantiated, or, from
    ;; (event-handler-thread (current-eventspace)).
    (define (repl-thunk)
      (with-handlers ([rerun?    (lambda (x) (channel-put ch x))]
                      [load-gui? (lambda (x) (channel-put ch x))])
        (when (and path (module-path? path))
          (parameterize ([current-module-name-resolver repl-module-name-resolver])
            (dynamic-require path 0))
          (current-namespace (module->namespace path)))
        (parameterize ([current-prompt-read (make-prompt-read path)]
                       [error-display-handler our-error-display-handler]
                       [current-module-name-resolver repl-module-name-resolver])
          (read-eval-print-loop))))
    ;; Main thread: Run repl-thunk on a plain thread, or, on the user
    ;; eventspace thread via queue-callback.
    ((txt/gui thread queue-callback) repl-thunk))
  ;; Main thread: Wait for message from REPL thread.
  (define msg (channel-get ch))
  (custodian-shutdown-all user-cust)
  (newline)
  (match msg
    [(rerun p)  (run p)]
    [(load-gui) (require-gui) (run path-str)]))

(struct rerun (path)) ;(or/c #f path-string?)
(struct load-gui ())

(define repl-module-name-resolver
  (let ([orig-resolver (current-module-name-resolver)])
    (case-lambda
      [(rmp ns)
       (orig-resolver rmp ns)]
      [(mp rmp stx load?)
       (when (and (eq? mp 'racket/gui/base) load?)
         (unless (gui-required?)
           (raise (load-gui))))
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
  (with-handlers ([exn:fail? (lambda (_) (not-found path-str))])
    (define path (expand-user-path (string->path path-str)))
    (cond [(file-exists? path) path]
          [else (not-found (path->string path))])))

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
             [(run) (raise (rerun (~a (read))))]
             [(top) (raise (rerun #f))]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define last-stx #f)

(define (exp1)
  (set! last-stx (expand-once (read)))
  (pp-stx last-stx))

(define (exp+)
  (when last-stx
    (define this-stx (expand-once last-stx))
    (cond [(equal? (syntax->datum last-stx) (syntax->datum this-stx))
           (display-commented "Already fully expanded.")
           (set! last-stx #f)]
          [else
           (pp-stx this-stx)
           (set! last-stx this-stx)])))

(define (exp!)
  (set! last-stx #f)
  (pp-stx (expand (read))))

(define (pp-stx stx)
  (newline)
  (pretty-print (syntax->datum stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (def sym)
  (display-definition (symbol->string sym)))

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

#lang racket/base

;; In this version, always require racket/gui/base
;; Just to get it working pumping main message loop.
;;
;; From there, work back to on-demand load of r/g/b, if possible.

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/format
         racket/string
         racket/list
         racket/pretty
         racket/runtime-path
         syntax/srcloc
         "defn.rkt"
         "imports-gui.rkt")

(module+ main
  (display (banner))
  (run #f))

(define main-cust (current-custodian))

;; (or/c #f path-string?)
(define (run path-str)
  (printf "run ~a\n" path-str)
  (define-values (path load-dir) (path-string->path&load-dir path-str))
  (when (and path (imports-gui? path))
    (require-racket/gui/base))
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
    ;; This will be called from another thread -- either a plain
    ;; thread when racket/gui/base is not (yet) instantiated, or, from
    ;; (event-handler-thread (current-eventspace)).
    (define (repl-thunk)
      (when (and path (module-path? path))
        (dynamic-require path 0)
        (current-namespace (module->namespace path)))
      (parameterize
          ([current-prompt-read (make-prompt-read path)]
           [error-display-handler our-error-display-handler]
           [current-module-name-resolver repl-module-name-resolver])
        (with-handlers ([void (lambda (x) (channel-put ch x))])
          (read-eval-print-loop))))
    ((txt/gui thread queue-callback) repl-thunk))
  ;; Wait for message to run again
  (define msg (channel-get ch))
  (custodian-shutdown-all user-cust)
  (newline)
  (match msg
    [(rerun p) (run p)]
    [(load-gui) (require-racket/gui/base) (run path-str)]))

(struct rerun (path)) ;(or/c #f path-string?)
(struct load-gui ())

;; This is #f until racket/gui/base is required the first time
(define root-eventspace #f)

(define (repl-gui-available?)
  (not (not root-eventspace)))

(define (require-racket/gui/base)
  (unless root-eventspace
    (displayln "on-demand one-time instantiation of racket/gui/base")
    (parameterize ([current-custodian main-cust])
      (define current-eventspace (dynamic-require 'racket/gui/base
                                                  'current-eventspace))
      (define make-eventspace    (dynamic-require 'racket/gui/base
                                                  'make-eventspace))
      (set! root-eventspace (make-eventspace))
      (current-eventspace root-eventspace))))

;; Like mz/mr from racket/sandbox.
(define-syntax txt/gui
  (syntax-rules ()
    [(_ txtval guisym)
     (if (repl-gui-available?)
         (dynamic-require 'racket/gui/base 'guisym)
         txtval)]))

;; This just to catch (require racket/gui/base) -- directly or
;; transitively -- at the REPL prompt. Currently we don't handle that,
;; so error.
(define repl-module-name-resolver
  (let ([orig-resolver (current-module-name-resolver)])
    (case-lambda
      [(rmp ns)
       (orig-resolver rmp ns)]
      [(mp rmp stx load?)
       (unless root-eventspace
         (when (and (eq? mp 'racket/gui/base) load?)
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

(define (our-error-display-handler str exn)
  (when (exn? exn)
    (unless (equal? "Check failure" (exn-message exn)) ;rackunit check fails
      (display-commented str)
      (display-srclocs exn)
      (unless (exn:fail:user? exn)
        (display-context exn)))))

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

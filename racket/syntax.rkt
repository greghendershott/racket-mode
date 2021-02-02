#lang racket/base

(require (only-in openssl/md5 md5)
         racket/contract
         racket/file
         racket/match
         (only-in racket/path path-only)
         syntax/modread
         syntax/parse/define
         "online-check-syntax.rkt")

(provide with-expanded-syntax-caching-evaluator
         file->syntax
         file->expanded-syntax
         string->expanded-syntax
         path->existing-syntax
         path->existing-expanded-syntax)

;; Return a syntax object for the contents of `path`. The resulting
;; syntax is applied to `k` while the parameter
;; current-load-relative-directory is set correctly for `path`.
(define/contract (file->syntax path [k values])
  (->* (path-string?)
       ((-> syntax? any))
       any)
  (define dir (path-only path))
  (parameterize ([current-load-relative-directory dir]
                 [current-directory               dir])
    (k
     (with-module-reading-parameterization
       (λ ()
         (with-input-from-file path
           (λ ()
             (port-count-lines! (current-input-port))
             (match (read-syntax)
               [(? eof-object?) #'""]
               [stx stx]))))))))

;; Same but from a string, where `path` is used for the load relative
;; directory and given to read-syntax as the source
(define/contract (string->syntax path code-str [k values])
  (->* (path-string? string?)
       ((-> syntax? any))
       any)
  (define dir (path-only path))
  (parameterize ([current-load-relative-directory dir]
                 [current-directory               dir])
    (k
     (with-module-reading-parameterization
       (λ ()
         (define in (open-input-string code-str path))
         (port-count-lines! in)
         (match (read-syntax path in)
           [(? eof-object?) #'""]
           [stx stx]))))))

;;; Expanded syntax caching

(define (call-with-expanded-syntax-caching-evaluator thk)
  (parameterize ([current-eval (make-eval-handler)])
    (thk)))

(define-simple-macro (with-expanded-syntax-caching-evaluator e:expr ...+)
  (call-with-expanded-syntax-caching-evaluator (λ () e ...)))

(define ((make-eval-handler [orig-eval (current-eval)]) e)
  (cond [(and (syntax? e)
              (not (compiled-expression? (syntax-e e)))
              (syntax-source e)
              (path-string? (syntax-source e))
              (complete-path? (syntax-source e))
              (file-exists? (syntax-source e)))
         (define expanded-stx (expand e))
         (define-values (code-str digest) (file->string+digest (syntax-source e)))
         (cache-set! (syntax-source e) code-str e expanded-stx digest)
         (orig-eval expanded-stx)]
        [else (orig-eval e)]))

(define (->path v)
  (cond [(path? v) v]
        [(path-string? v) (string->path v)]
        [else (error '->path "not path? or path-string?" v)]))

;; Returns the result of applying `k` to the expanded syntax, with the
;; correct parameterization of current-namespace and
;; current-load-relative-directory. Note that `k` deliberately does
;; not default to `values` because trying to use the syntax without
;; the correct parameterizations will often result in bugs, sometimes
;; subtle and confusing. So this "CPS" approach guides you to do the
;; right thing.
(define/contract (file->expanded-syntax path-str k)
  (-> path-string? (-> syntax? any) any)
  (define path (->path path-str))
  (define-values (code-str digest) (file->string+digest path))
  (match (cache-get path)
    [(and ce (struct* cache-entry ([exp-stx exp-stx] [digest (== digest)])))
     (log-racket-mode-syntax-cache-info "file->expanded-syntax cache hit ~v ~v" path digest)
     (with-cache-entry-params ce
       (k exp-stx))]
    [_
     (log-racket-mode-syntax-cache-info "file->expanded-syntax cache MISS ~v ~v" path digest)
     (file->syntax
      path
      (λ (stx)
        ;; Create and parameterize a namespace here. file->syntax
        ;; already parameterized the directory before calling us.
        (parameterize ([current-namespace (make-base-namespace)])
          (define exp-stx (with-online-check-syntax path (expand stx)))
          (cache-set! path code-str stx exp-stx digest)
          (k exp-stx))))]))

;; Same but when you don't have a file.
(define/contract (string->expanded-syntax path-str code-str k)
  (-> path-string? string? (-> syntax? any) any)
  (define path (->path path-str))
  (define digest (string->digest code-str))
  (match (cache-get path)
    [(and ce (struct* cache-entry ([exp-stx exp-stx] [digest (== digest)])))
     (log-racket-mode-syntax-cache-info "string->expanded-syntax cache hit ~v ~v" path digest)
     (with-cache-entry-params ce
       (k exp-stx))]
    [_
     (log-racket-mode-syntax-cache-info "string->expanded-syntax cache MISS ~v ~v" path digest)
     (string->syntax
      path-str code-str
      (λ (stx)
        ;; Create and parameterize a namespace here. string->syntax
        ;; already parameterized the directory before calling us.
        (parameterize ([current-namespace (make-base-namespace)])
          (define exp-stx (with-online-check-syntax path (expand stx)))
          (cache-set! path code-str stx exp-stx digest)
          (k exp-stx))))]))

;; Like string->syntax but given only the path-str and only if syntax
;; already in the cache, as a result of previously calling
;; string->expanded-syntax. Intended for use by identifier.rkt.
(define/contract (path->existing-syntax path-str k)
  (-> path-string? (-> syntax? any) any)
  (define path (->path path-str))
  (match (cache-get path)
    [(and ce (struct* cache-entry ([stx stx])))
     (log-racket-mode-syntax-cache-info "path->existing-syntax cache hit ~v (ignoring digest)" path)
     (with-cache-entry-params ce
       (k stx))]
    [#f
     (match (path->code path)
       [(code code-str digest)
        (log-racket-mode-syntax-cache-info "path->existing-syntax cache MISS ~v (ignoring digest); re-expanding and re-caching" path)
        (string->syntax
         path-str code-str
         (λ (stx)
           ;; Create and parameterize a namespace here. string->syntax
           ;; already parameterized the directory before calling us.
           (parameterize ([current-namespace (make-base-namespace)])
             (define exp-stx (with-online-check-syntax path (expand stx)))
             (cache-set! path code-str stx exp-stx digest)
             (k stx))))]
       [#f
        (log-racket-mode-syntax-cache-warning "path->existing-syntax cache MISS ~v (ignoring digest); no code string cached for path, cannot re-expand" path)
        #f])]))

;; Like string->expanded-syntax but given only the path-str and only
;; if expanded syntax already in the cache, as a result of previously
;; calling string->expanded-syntax. Intended for use by
;; identifier.rkt.
(define/contract (path->existing-expanded-syntax path-str k)
  (-> path-string? (-> syntax? any) any)
  (define path (->path path-str))
  (match (cache-get path)
    [(and ce (struct* cache-entry ([exp-stx exp-stx])))
     (log-racket-mode-syntax-cache-info "path->existing-expanded-syntax cache hit ~v (ignoring digest)" path)
     (with-cache-entry-params ce
       (k exp-stx))]
    [#f
     (match (path->code path)
       [(code code-str digest)
        (log-racket-mode-syntax-cache-info "path->existing-expanded-syntax cache MISS ~v (ignoring digest); re-expanding and re-caching" path)
        (string->syntax
         path-str code-str
         (λ (stx)
           ;; Create and parameterize a namespace here. string->syntax
           ;; already parameterized the directory before calling us.
           (parameterize ([current-namespace (make-base-namespace)])
             (define exp-stx (with-online-check-syntax path (expand stx)))
             (cache-set! path code-str stx exp-stx digest)
             (k exp-stx))))]
       [#f
        (log-racket-mode-syntax-cache-warning "path->existing-expanded-syntax cache MISS ~v (ignoring digest); no code string cached for path, cannot re-expand" path)
        #f])]))

(define/contract (file->digest path)
  (-> path? string?)
  (call-with-input-file path md5))

(define/contract (file->string+digest path)
  (-> path? (values string? string?))
  (define str (file->string path))
  (define digest (string->digest str))
  (values str digest))

(define/contract (string->digest str)
  (-> string? string?)
  (md5 (open-input-string str)))

(module+ test
  (require rackunit
           racket/file)
  (define this-path (syntax-source #'here))
  (define this-string (file->string this-path))
  (check-equal? (file->digest this-path)
                (string->digest this-string))
  ;; Note: This test will only succeed if the same syntax object put
  ;; in the cache by file->expanded-syntax is retrieved from the cache
  ;; by string->expanded-syntax. In other words, two identical calls
  ;; to file->syntax do not produce equal? syntax objects.
  (check-equal? (file->expanded-syntax this-path values)
                (string->expanded-syntax this-path this-string values))
  (check-equal? (path->existing-expanded-syntax this-path (λ (_stx) 42))
                42))

(module cache racket/base
  (require racket/contract
           racket/match
           racket/path
           syntax/parse/define
           "online-check-syntax.rkt")

  (provide log-racket-mode-syntax-cache-debug
           log-racket-mode-syntax-cache-info
           log-racket-mode-syntax-cache-warning
           (struct-out code)
           (rename-out [get-code path->code])
           (struct-out cache-entry)
           cache-set!
           cache-get
           with-cache-entry-params)

  (define-logger racket-mode-syntax-cache)

  ;; This lookup table allows the path->existing-syntax and
  ;; path->existing-expanded-syntax functions to deal with a cache
  ;; miss by re-expanding (and re-caching). Those are intended to
  ;; support identifier.rkt and find.rkt working with syntax from
  ;; "live" code strings that aren't in a file -- e.g. the code
  ;; strings originated from a check-syntax command. This lookup table
  ;; is never cleaned up, but I believe (?) it's much less "heavy"
  ;; than the syntax, expanded syntax, and namespace values in the
  ;; main cache.
  (define path->code (make-hash)) ;(hash/c path? code?)
  (struct code (str digest))
  (define (get-code path)
    (hash-ref path->code path #f))

  ;; This is the main cache. Our eviction strategy is for cache
  ;; entries to be ephemerons keyed on the namespace. In practice this
  ;; means that a major GC will evict nearly all of these, except
  ;; maybe those for the most recently run file in each REPL session
  ;; and its non-compiled required files loaded by our caching
  ;; eval-handler.
  ;;
  ;; That is a fairly "chunky" approach: Nothing is freed until a
  ;; major GC, at which point possibly too much is freed. But I'm not
  ;; sure what else to do, exactly. Could the Emacs front end tell us
  ;; some explicit "working set" to preserve (or not)? Sure, but the
  ;; desirable "working set" is not just a list of Emacs buffer files.
  ;; For example, the caching done by the eval-handler means that
  ;; things like visit-definition, which need to walk fully expanded
  ;; syntax to find the location, will find that already available in
  ;; the cache. This speeds up a very common usage pattern: Visit a
  ;; file, then visit the definition of something in a required file.
  ;; That scenario was a big motivation for caching. [Yes,
  ;; check-syntax tells us the defining file -- but not the location
  ;; within; to find that we need syntax and often expanded syntax.]
  (define cache (make-hash)) ;(hash/c path? (ephemeron/c namespace? cache-entry?)
  (struct cache-entry (stx exp-stx digest dir namespace online))

  (define/contract (cache-set! path code-str stx exp-stx digest)
    (-> path? string? syntax? syntax? string? any)
    (log-racket-mode-syntax-cache-debug "cache-set: ~v" path)
    (define dir (current-load-relative-directory))
    (define namespace (current-namespace))
    (define online (current-online-check-syntax))
    (hash-set! cache path
               (make-ephemeron
                namespace
                (cache-entry stx exp-stx digest dir namespace online)))
    (hash-set! path->code path (code code-str digest)))

  (define/contract (cache-get path)
    (-> path? (or/c #f cache-entry?))
    (log-cache-stats)
    (match (hash-ref cache path #f)
      [(? ephemeron? e)
       (define v (ephemeron-value e))
       (log-racket-mode-syntax-cache-debug "cache-get ~v: v=~v" path v)
       v]
      [#f
       (log-racket-mode-syntax-cache-debug "cache-get ~v: not found" path)
       #f]))

  ;; "If your parameterize form uses a half dozen parameters, you're
  ;; probably missing some" -- not Alan Perlis
  (define-simple-macro (with-cache-entry-params ce:expr e:expr ...+)
    (match-let ([(struct* cache-entry ([dir dir] [namespace ns] [online ol])) ce])
      (parameterize ([current-namespace               ns]
                     [current-load-relative-directory dir]
                     [current-directory               dir]
                     [current-online-check-syntax     ol])
        e ...)))

  (define (log-cache-stats)
    (when (log-level? (current-logger) 'debug 'racket-mode-syntax-cache)
      (define-values (active-count active evicted-count evicted)
        (for/fold ([active-count 0]
                   [active '()]
                   [evicted-count 0]
                   [evicted '()])
                  ([(k v) (in-hash cache)])
          (define n (path->string (file-name-from-path k)))
          (if (ephemeron-value v)
              (values (add1 active-count) (cons n active) evicted-count evicted)
              (values active-count active (add1 evicted-count) (cons n evicted)))))
      (log-racket-mode-syntax-cache-debug
       "~v syntax cache entries...\n~v active: ~v\n~v evicted: ~v"
       (+ active-count evicted-count)
       active-count active
       evicted-count evicted))))
(require 'cache)

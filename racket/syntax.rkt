#lang racket/base

(require (only-in openssl/md5 md5)
         racket/contract
         racket/match
         (only-in racket/path path-only)
         syntax/modread
         syntax/parse/define
         "mod.rkt")

(provide with-expanded-syntax-caching-evaluator
         file->syntax
         file->expanded-syntax
         string->expanded-syntax
         path->existing-syntax
         path->existing-expanded-syntax)

(define-logger racket-mode-syntax-cache)

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
         (define in (open-input-string code-str))
         (port-count-lines! in)
         (match (read-syntax path in)
           [(? eof-object?) #'""]
           [stx stx]))))))

;;; expanded syntax caching

(define/contract (call-with-expanded-syntax-caching-evaluator maybe-mod thk)
  (-> (or/c mod? #f) (-> any) any)
  (before-run maybe-mod)
  (begin0
      (parameterize ([current-eval (make-eval-handler maybe-mod)])
        (thk))
    (after-run maybe-mod)))

(define-simple-macro (with-expanded-syntax-caching-evaluator mm:expr e:expr ...+)
  (call-with-expanded-syntax-caching-evaluator mm (λ () e ...)))

;; Call this early in a file run, _before_ any evaluation.
(define (before-run _maybe-mod)
  ;; Don't actually flush the entire cache anymore. Because we're also
  ;; using this for check-syntax. TODO: Some new strategy, or, let the
  ;; cache grow indefinitely?
  ;;
  ;; Note: The case of same path with different digest is handled when
  ;; we lookup items from the hash. It's considered a cache miss, we
  ;; expand again, and that is the new value in the hash for that
  ;; path.
  (void))

(define ((make-eval-handler _maybe-mod [orig-eval (current-eval)]) e)
  (cond [(and (syntax? e)
              (syntax-source e)
              (path-string? (syntax-source e))
              (not (compiled-expression? (syntax-e e))))
         (define expanded-stx (expand e))
         (cache-set! (syntax-source e)
                     e
                     expanded-stx
                     (file->digest (syntax-source e))
                     (current-namespace)
                     (current-load-relative-directory))
         (orig-eval expanded-stx)]
        [else (orig-eval e)]))

(define (after-run _maybe-mod)
  (void))

;; cache : (hash/c path? cache-entry?)
(struct cache-entry (stx exp-stx digest namespace load-relative-directory))
(define cache (make-hash))
(define last-mod #f)

(define/contract (cache-set! path stx exp-stx digest namespace load-rel-dir)
  (-> path? syntax? syntax? string? namespace? path-string? any)
  (hash-set! cache path
             (cache-entry stx
                          exp-stx
                          digest
                          namespace
                          load-rel-dir)))

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
  (define digest (file->digest path))
  (match (hash-ref cache path #f)
    [(cache-entry _stx exp-stx (== digest) namespace load-rel-dir)
     (log-racket-mode-syntax-cache-info "file->expanded-syntax cache hit ~v ~v" path digest)
     (parameterize ([current-namespace               namespace]
                    [current-load-relative-directory load-rel-dir]
                    [current-directory               load-rel-dir])
       (k exp-stx))]
    [_
     (log-racket-mode-syntax-cache-info "file->expanded-syntax cache MISS ~v ~v" path digest)
     (file->syntax
      path
      (λ (stx)
        ;; Create and parameterize a namespace here. file->syntax
        ;; already parameterized the directory before calling us.
        (parameterize ([current-namespace (make-base-namespace)])
          (define exp-stx (expand stx))
          (cache-set! path stx exp-stx digest (current-namespace) (current-load-relative-directory))
          (k exp-stx))))]))

;; Same but when you don't have a file.
(define/contract (string->expanded-syntax path-str code-str k)
  (-> path-string? string? (-> syntax? any) any)
  (define path (->path path-str))
  (define digest (string->digest code-str))
  (match (hash-ref cache path #f)
    [(cache-entry _stx exp-stx (== digest) namespace load-rel-dir)
     (log-racket-mode-syntax-cache-info "string->expanded-syntax cache hit ~v ~v" path digest)
     (parameterize ([current-namespace               namespace]
                    [current-load-relative-directory load-rel-dir]
                    [current-directory               load-rel-dir])
       (k exp-stx))]
    [_
     (log-racket-mode-syntax-cache-info "string->expanded-syntax cache MISS ~v ~v" path digest)
     (string->syntax
      path-str code-str
      (λ (stx)
        ;; Create and parameterize a namespace here. string->syntax
        ;; already parameterized the directory before calling us.
        (parameterize ([current-namespace (make-base-namespace)])
          (define exp-stx (expand stx))
          (cache-set! path stx exp-stx digest (current-namespace) (current-load-relative-directory))
          (k exp-stx))))]))

(define/contract (file->digest path)
  (-> path? string?)
  (call-with-input-file path md5))

(define/contract (string->digest str)
  (-> string? string?)
  (md5 (open-input-string str)))

;; Like string->syntax but given only the path-str and only if syntax
;; already in the cache, as a result of previously calling
;; string->expanded-syntax. Intended for use by identifier.rkt.
(define/contract (path->existing-syntax path-str k)
  (-> path-string? (-> syntax? any) any)
  (define path (->path path-str))
  (match (hash-ref cache path #f)
    [(cache-entry stx _exp-stx _digest namespace load-rel-dir)
     (log-racket-mode-syntax-cache-info "path->existing-syntax cache hit ~v (ignoring digest)" path)
     (parameterize ([current-namespace               namespace]
                    [current-load-relative-directory load-rel-dir]
                    [current-directory               load-rel-dir])
       (k stx))]
    [#f
     (log-racket-mode-syntax-cache-warning "path->existing-syntax cache MISS ~v (ignoring digest)" path)
     #f]))

;; Like string->expanded-syntax but given only the path-str and only
;; if expanded syntax already in the cache, as a result of previously
;; calling string->expanded-syntax. Intended for use by
;; identifier.rkt.
(define/contract (path->existing-expanded-syntax path-str k)
  (-> path-string? (-> syntax? any) any)
  (define path (->path path-str))
  (match (hash-ref cache path #f)
    [(cache-entry _stx exp-stx _digest namespace load-rel-dir)
     (log-racket-mode-syntax-cache-info "path->existing-expanded-syntax cache hit ~v (ignoring digest)" path)
     (parameterize ([current-namespace               namespace]
                    [current-load-relative-directory load-rel-dir]
                    [current-directory               load-rel-dir])
       (k exp-stx))]
    [#f
     (log-racket-mode-syntax-cache-warning "path->existing-expanded-syntax cache MISS ~v (ignoring digest)" path)
     #f]))

(module+ test
  (require rackunit
           racket/file)
  (define this-path (syntax-source #'here))
  (define this-string (file->string this-path))
  (check-equal? (file->digest this-path)
                (string->digest this-string))
  (check-equal? (file->expanded-syntax this-path values)
                (string->expanded-syntax this-path this-string values))
  (check-equal? (path->existing-expanded-syntax this-path (λ (_stx) 42))
                42))

#lang racket/base

(require (only-in openssl/md5 md5)
         racket/contract
         racket/match
         (only-in racket/path path-only)
         racket/promise
         syntax/modread
         syntax/parse/define
         "mod.rkt")

(provide with-expanded-syntax-caching-evaluator
         file->syntax
         file->expanded-syntax
         file->expanded-syntax-and-namespace
         string->syntax
         string->expanded-syntax
         string->expanded-syntax-and-namespace
         path->existing-expanded-syntax)

(define-logger racket-mode-syntax-cache)

;; Return a syntax object for the contents of `path`. The resulting
;; syntax is applied to `k` while the parameter
;; current-load-relative-directory is set correctly for `path`.
(define/contract (file->syntax path [k values])
  (->* (path-string?)
       ((-> syntax? syntax?))
       syntax?)
  (define dir (path-only path))
  (parameterize ([current-load-relative-directory dir]
                 [current-directory               dir])
    (k
     (with-module-reading-parameterization
       (λ ()
         (with-input-from-file path
           (λ ()
             (port-count-lines! (current-input-port))
             (read-syntax))))))))

;; Same but from a string, where `path` is used for the load relative
;; directory and given to read-syntax as the source
(define/contract (string->syntax path code-str [k values])
  (->* (path-string? string?)
       ((-> syntax? syntax?))
       syntax?)
  (define dir (path-only path))
  (parameterize ([current-load-relative-directory dir]
                 [current-directory               dir])
    (k
     (with-module-reading-parameterization
       (λ ()
         (define in (open-input-string code-str))
         (port-count-lines! in)
         (read-syntax path in))))))

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

;; cache : (hash/c path? cache-entry?)
(struct cache-entry (digest promise namespace))
(define cache (make-hash))
(define last-mod #f)

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
  (void)
  #;
  (unless (equal? last-mod maybe-mod)
    (hash-clear! cache)
    (set! last-mod maybe-mod)))

(define ((make-eval-handler maybe-mod [orig-eval (current-eval)]) e)
  (cond [(and (syntax? e)
              (syntax-source e)
              (path-string? (syntax-source e))
              (not (compiled-expression? (syntax-e e))))
         (define expanded-stx (expand e))
         (cache-set! (syntax-source e)
                     (file->digest (syntax-source e))
                     expanded-stx)
         (orig-eval expanded-stx)]
        [else (orig-eval e)]))

(define (after-run maybe-mod)
  ;; When the rkt file being run has a compiled zo that was used, then
  ;; our eval-hander above won't expand and cache any syntax. That
  ;; means when the user does a command that needs expanded syntax
  ;; (e.g. find-completion), they will need to wait for expansion. But
  ;; if you call this _after_ the file was run, it will cache-set! the
  ;; expansion using `delay/thread` -- i.e. the work will happen "in
  ;; the background". (Furthermore, when we already have a cache entry
  ;; for the file and digest, from a previous run, we'll just use
  ;; that.) As a result, it's likely to be mostly or entirely ready
  ;; when the user does a command.
  (define-values (dir base _) (maybe-mod->dir/file/rmp maybe-mod))
  (when (and dir base)
    (define path (build-path dir base))
    (cache-set! path
                (file->digest path)
                (delay/thread (file->syntax path expand)))))

;; cache-set! takes (or/c syntax? promise?) so that, if the cache
;; already has an entry for the file and digest, it can avoid doing
;; any work.
(define/contract (cache-set! path digest stx-or-promise)
  (-> path? string? (or/c syntax? promise?) any)
  (match (hash-ref cache path #f)
    [(cache-entry (== digest) _ _)
     (log-racket-mode-syntax-cache-info "cache-set! HIT ~v ~v" path digest)
     (void)]
    [_
     (log-racket-mode-syntax-cache-info "cache-set! MISS ~v ~v" path digest)
     (hash-set! cache path
                (cache-entry digest
                             stx-or-promise
                             (current-namespace)))]))

(define (->path v)
  (cond [(path? v) v]
        [(path-string? v) (string->path v)]
        [else (error '->path "not path? or path-string?" v)]))

;; returns the namespace in which the cached stx was expanded
(define/contract (file->expanded-syntax-and-namespace path-str)
  (-> path-string? (values syntax? namespace?))
  (define path (->path path-str))
  (define digest (file->digest path))
  (match (hash-ref cache path #f)
    [(cache-entry (== digest) promise namespace)
     (log-racket-mode-syntax-cache-info "file->expanded-syntax cache hit ~v ~v" path digest)
     (values (force promise) namespace)]
    [_
     (log-racket-mode-syntax-cache-info "file->expanded-syntax cache MISS ~v ~v" path digest)
     (define stx (file->syntax path expand))
     (cache-set! path digest stx)
     (values stx (current-namespace))]))

;; returns the namespace in which the cached stx was expanded
(define/contract (string->expanded-syntax-and-namespace path-str code-str)
  (-> path-string? string? (values syntax? namespace?))
  (define path (->path path-str))
  (define digest (string->digest code-str))
  (match (hash-ref cache path #f)
    [(cache-entry (== digest) promise namespace)
     (log-racket-mode-syntax-cache-info "string->expanded-syntax cache hit ~v ~v" path digest)
     (values (force promise) namespace)]
    [_
     (log-racket-mode-syntax-cache-info "string->expanded-syntax cache MISS ~v ~v" path digest)
     (define stx (string->syntax path code-str expand))
     (cache-set! path digest stx)
     (values stx (current-namespace))]))

;; Effectively (file->syntax path expand), caching.
(define/contract (file->expanded-syntax path-str)
  (-> path-string? syntax?)
  (define-values (stx _ns) (file->expanded-syntax-and-namespace path-str))
  stx)

;; Effectively (string->syntax path code-str expand), caching.
(define/contract (string->expanded-syntax path-str code-str)
  (-> path-string? string? syntax?)
  (define-values (stx _ns) (string->expanded-syntax-and-namespace path-str code-str))
  stx)

(define/contract (file->digest path)
  (-> path? string?)
  (call-with-input-file path md5))

(define/contract (string->digest str)
  (-> string? string?)
  (md5 (open-input-string str)))

;; Simply get syntax corresponding to the expanded module, if it
;; already exists in the cache. Intended for use by identifier.rkt.
(define/contract (path->existing-expanded-syntax path-str)
  (-> path-string? (or/c #f syntax?))
  (define path (->path path-str))
  (match (hash-ref cache path #f)
    [(cache-entry _digest promise _syntax)
     (log-racket-mode-syntax-cache-info "path->existing-expanded-syntax cache hit ~v (ignoring digest)" path)
     (force promise)]
    [#f
     (log-racket-mode-syntax-cache-info "path->existing-expanded-syntax cache MISS ~v (ignoring digest)" path)
     #f]))

(module+ test
  (require rackunit
           racket/file)
  (define this-path (syntax-source #'here))
  (define this-string (file->string this-path))
  (check-equal? (file->digest this-path)
                (string->digest this-string))
  (check-equal? (parameterize ([current-namespace (make-base-namespace)])
                  (file->expanded-syntax this-path))
                (parameterize ([current-namespace (make-base-namespace)])
                  (string->expanded-syntax this-path this-string))))

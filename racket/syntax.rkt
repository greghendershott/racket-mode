#lang racket/base

(require (only-in compiler/cm [get-file-sha1 file->digest])
         racket/contract
         racket/match
         racket/promise
         syntax/modread
         syntax/parse/define
         "mod.rkt")

(provide with-expanded-syntax-caching-evaluator
         file->syntax
         file->expanded-syntax)

;; Return a syntax object or #f for the contents of `file`. The
;; resulting syntax is applied to `k` while the parameters
;; current-load-relative-directory and current-namespace are still set
;; appropriately.
(define/contract (file->syntax file [k values])
  (->* (path-string?)
       ((-> syntax? syntax?))
       (or/c #f syntax?))
  (define-values (base _ __) (split-path file))
  (parameterize ([current-load-relative-directory base]
                 [current-namespace (make-base-namespace)])
    (with-handlers ([exn:fail? (λ _ #f)])
      (k
       (with-module-reading-parameterization
         (λ ()
           (with-input-from-file file read-syntax/count-lines)))))))

(define (read-syntax/count-lines)
  (port-count-lines! (current-input-port))
  (read-syntax))

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

;; cache : (hash/c file (cons/c digest-string? (or/c promise? syntax?)))
(define cache (make-hash))
(define last-mod #f)

;; Call this early in a file run, _before_ any evaluation. If it's not
;; the same file as before, we empty the cache -- to free up memory.
;; If it's the same file, we keep the cache.
(define (before-run maybe-mod)
  (unless (equal? last-mod maybe-mod)
    (hash-clear! cache)
    (set! last-mod maybe-mod)))

(define ((make-eval-handler maybe-mod [orig-eval (current-eval)]) e)
  (cond [(and (syntax? e)
              (syntax-source e)
              (path-string? (syntax-source e))
              (not (compiled-expression? (syntax-e e))))
         (define expanded-stx (expand e))
         (cache-set! (syntax-source e) (λ () expanded-stx))
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
    (cache-set! path (λ () (delay/thread (file->syntax path expand))))))

;; cache-set! takes a thunk so that, if the cache already has an entry
;; for the file and digest, it can avoid doing any work. Furthermore,
;; if you already have a digest for file, supply it to avoid redoing
;; that work, too.
(define/contract (cache-set! file thk [digest #f])
  (->* (path-string? (-> (or/c promise? syntax?)))
       ((or/c #f string?))
       any)
  (let ([digest (or digest (file->digest file))])
    (match (hash-ref cache file #f)
      [(cons (== digest) _)
       (void)]
      [_
       (hash-set! cache file (cons digest (thk)))])))

(define (file->expanded-syntax file)
  (define digest (file->digest file))
  (match (hash-ref cache file #f)
    [(cons (== digest) promise)
     (force promise)]
    [_
     (define stx (file->syntax file expand))
     (cache-set! file (λ () stx) digest)
     stx]))

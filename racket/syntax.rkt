;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (only-in openssl/md5 md5)
         racket/contract
         racket/file
         racket/match
         (only-in racket/path path-only)
         syntax/modread
         "online-check-syntax.rkt")

(provide make-caching-load/use-compiled-handler
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

;; Various functions to obtain syntax or fully-expanded syntax from
;; files or strings, backed by a cache, as well as a compiled load
;; handler that warms the cache. Note: The cache stores expansions
;; from expand ("enriched") -- /not/ from expand-syntax.

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

;; Compiled load handler: This is an optimization to warm the cache
;; with expansions done for loads that need to compile, including
;; imports that need to compile. Can speed up scenarios like visiting
;; a definition in a required file.
(define (make-caching-load/use-compiled-handler)
  (define old-handler (current-load/use-compiled))
  (define old-compile (current-compile))
  (define (new-compile stx immediate?)
    (match (syntax-source stx)
      [(? path? file)
       (define exp-stx (expand stx))
       (define-values (code-str digest) (file->string+digest file))
       (cache-set! file code-str stx exp-stx digest)
       (old-compile exp-stx immediate?)]
      [_ (old-compile stx immediate?)]))
  (define (new-handler file mod)
    (parameterize ([current-compile new-compile])
      (old-handler file mod)))
  new-handler)

(define (->path v)
  (cond [(path? v) v]
        [(path-string? v) (string->path v)]
        [else (error '->path "not path? or path-string?" v)]))

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

  (define sema (make-semaphore 1))
  (define-simple-macro (with-sema e:expr ...+)
    (call-with-semaphore sema (λ () e ...)))

  ;; This lookup table allows the path->existing-syntax and
  ;; path->existing-expanded-syntax functions to deal with a cache
  ;; miss by re-expanding (and re-caching). Those are intended to
  ;; support identifier.rkt and find.rkt working with syntax from
  ;; "live" code strings that aren't in a file -- e.g. the code
  ;; strings originated from a check-syntax command. This lookup table
  ;; is never cleaned up, but I believe (?) it's much less "heavy"
  ;; than the syntax, expanded syntax, and namespace values in the
  ;; main cache.
  (define path->code (make-hash)) ;(hash/c path? string?)
  (struct code (str digest))
  (define (get-code path)
    (hash-ref path->code path #f))

  ;; The main cache is an association list in order from MRU to LRU.
  ;; The keys are paths. The values are either cache-entry (not
  ;; evictable) or an ephemeron keyed by the namespace (evictable when
  ;; the ns is not otherwise reachable). Approximately the first
  ;; `mru-to-keep` items in the list are non-evictable cache-entry
  ;; items; the rest are evictable ephemerons. (It can be one more; we
  ;; don't really care, so we don't track whether a set/get moves an
  ;; item in/out of that first `mru-to-keep`.)
  ;;
  ;; Note: After making changes to mru-to-keep, cache-set!, or
  ;; cache-get, it would be wise to run the slow-test submodule in
  ;; check-syntax.rkt as a smoke test.
  (define cache null)
  (struct cache-entry (stx exp-stx digest dir namespace online))
  (define mru-to-keep 8)

  (define (not-evicted? v)
    (or (cache-entry? v)
        (ephemeron-value v)))

  (define (promote v) ;make non-evictable
    (match v
      [(? cache-entry? ce) ce]
      [(? ephemeron? e) (or (ephemeron-value e) e)]))

  (define (demote v) ;make evictable
    (match v
      [(and (struct* cache-entry ([namespace ns])) ce) (make-ephemeron ns ce)]
      [(? ephemeron? e) e]))

  (define (promote/demote n v)
    (if (< n mru-to-keep)
        (promote v)
        (demote v)))

  (define/contract (cache-set! path code-str stx exp-stx digest)
    (-> path? string? syntax? syntax? string? any)
    (with-sema
      (log-racket-mode-syntax-cache-debug "cache-set: ~v" path)
      (hash-set! path->code path (code code-str digest))
      ;; This is written to walk the existing association list just
      ;; once to build the new tail onto which we'll cons a new item
      ;; for `path`. When building the new tail, we don't keep any old
      ;; item for `path` or any already-evicted items. We promote or
      ;; demote items we do keep.
      (define head
        (cons path (cache-entry stx exp-stx digest
                                (current-load-relative-directory)
                                (current-namespace)
                                (current-online-check-syntax))))
      (define tail
        (for*/list ([(k+v n) (in-indexed cache)]
                    [k (in-value (car k+v))] #:unless (equal? k path)
                    [v (in-value (cdr k+v))] #:when (not-evicted? v))
          (cons k (promote/demote n v))))
      (set! cache (cons head tail))))

  (define/contract (cache-get path)
    (-> path? (or/c #f cache-entry?))
    (with-sema
      ;; This is written to walk the existing association list just
      ;; once, to look for `path` while building the new tail. If
      ;; found, it becomes the new head. Regardless, in the new tail
      ;; we don't keep already-evicted items. We promote or demote
      ;; items we do keep.
      (define-values (head reversed-tail)
        (for*/fold ([head #f]
                    [tail null])
                   ([(k+v n) (in-indexed cache)]
                    [k (in-value (car k+v))]
                    [v (in-value (cdr k+v))] #:when (not-evicted? v))
          (cond
            [(equal? k path) ;found: don't add to tail, will be new head
             (values (cons k (promote v))
                     tail)]
            [else
             (values head
                     (cons (cons k (promote/demote n v))
                           tail))])))
      (define tail (reverse reversed-tail))
      (log-racket-mode-syntax-cache-debug "cache-get ~v => ~v ~v" path head tail)
      (match head
        [(cons _ (? cache-entry? ce)) (set! cache (cons head tail)) ce]
        [#f                           (set! cache tail)             #f])))

  ;; "If your parameterize form uses a half dozen parameters, you're
  ;; probably missing some" -- not Alan Perlis
  (define-simple-macro (with-cache-entry-params ce:expr e:expr ...+)
    (match-let ([(struct* cache-entry ([dir dir] [namespace ns] [online ol])) ce])
      (parameterize ([current-namespace               ns]
                     [current-load-relative-directory dir]
                     [current-directory               dir]
                     [current-online-check-syntax     ol])
        e ...))))
(require 'cache)

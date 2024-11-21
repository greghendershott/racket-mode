;; Copyright (c) 2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/match
         racket/set
         (only-in syntax/modresolve
                  resolve-module-path)
         setup/dirs
         setup/getinfo
         pkg/lib
         "define-fallbacks.rkt")

(define-fallbacks setup/dirs
  [(get-base-documentation-packages) '("racket-doc")]
  [(get-distribution-documentation-packages) '("main-distribution") ])

(provide lib-pkg-sort)

;; This code for classifying packages as "base" or "main-dist" is
;; borrowed from racket-index/scribblings/main/private/pkg.rkt
(define base-pkgs #f)
(define main-dist-pkgs #f)
(define pkg-cache-for-pkg-directory (make-hash))

(define (get-base-pkgs)
  (unless base-pkgs
    (set! base-pkgs (find-pkgs (get-base-documentation-packages))))
  base-pkgs)

(define (get-main-dist-pkgs)
  (unless main-dist-pkgs
    (set! main-dist-pkgs (find-pkgs (get-distribution-documentation-packages)
                                    #:exclude (list->set (get-base-pkgs)))))
  main-dist-pkgs)

(define (find-pkgs root-pkg-names #:exclude [excludes (set)])
  (define result '())
  (define seen (set-copy excludes))
  (for ([root-pkg-name (in-list root-pkg-names)])
    (match (pkg-directory
            root-pkg-name
            #:cache pkg-cache-for-pkg-directory)
      [#f '()]
      [_
       (let loop ([pkg root-pkg-name])
         (unless (set-member? seen pkg)
           (set-add! seen pkg)
           (match (pkg-directory pkg #:cache pkg-cache-for-pkg-directory)
             [#f
              ;; these are platform dependent packages (like racket-win32-i386-3)
              ;; they have no deps, and if they are platform dependent,
              ;; they are not that useful (for documentation search) anyway
              (set! result (cons pkg result))]
             [dir
              (set! result (cons pkg result))
              (define get-info (get-info/full dir))
              (define direct-deps
                (for/list ([dep (extract-pkg-dependencies get-info #:build-deps? #f)])
                  (match dep
                    [(? string?) dep]
                    [(cons dep _) dep])))
              ;; we need to recur. For example, 2dtabular is in 2d-lib,
              ;; which is not a direct dep of main-distribution
              (for ([dep direct-deps])
                (loop dep))])))]))
  result)

;; However we can't follow the example of web search, which builds its
;; index at doc build time. The package info known at doc build time
;; doesn't make it into the xref index.
;;
;; So instead: When a doc index item has an "exported from lib", we
;; use resolve-module-path and path->pkg. However this is moderately
;; expensive, and should be done lazily (definitely not eagerly for
;; all 32K+ xref-index items) and cached.

(define pkg-cache-for-path->pkg (make-hash))
(define ns (make-base-namespace))
(define (pkg-name mp)
  (match (parameterize ([current-namespace ns])
           (resolve-module-path mp))
    [(or (? path? p)
         (list* 'submod (? path? p)))
     (path->pkg p
                #:cache pkg-cache-for-path->pkg)]
    [_ #f]))

(define cache (make-hash))
(define (lib-pkg-sort maybe-mod-path)
  (hash-ref!
   cache
   maybe-mod-path
   (λ ()
     (with-handlers ([exn:fail? (λ _ 9)])
       (define p (pkg-name maybe-mod-path))
       (cond [(not p)                         0]
             [(member p (get-base-pkgs))      1]
             [(member p (get-main-dist-pkgs)) 2]
             [else                            3])))))

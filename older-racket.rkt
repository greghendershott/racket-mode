#lang racket/base

(require racket/list
         racket/function
         racket/path) ;explode-path NOT in racket/base prior to 6.0

;; A few functions we need that were added in Racket 6.0. This lets us
;; run on Racket 5.3.5 (perhaps earlier, but I haven't tested).
(provide find-collects-dir
         path->collects-relative
         current-directory-for-user)
         
(define (our-find-collects-dir)
  (apply build-path
         (reverse (cdr (reverse (explode-path (collection-path "racket")))))))

(define find-collects-dir
  (dynamic-require 'setup/dirs 'find-collects-dir
                   (const our-find-collects-dir)))

;; Warning: This is only the subset of path->collects-relative
;; functionality that we actually use.
(define (our-path->collects-relative path)
  (define cs (explode-path (find-collects-dir)))
  (define ps (explode-path (simplify-path path)))
  (cond [(> (length ps) (length cs))
         (define-values (as bs) (split-at ps (length cs)))
         (cond [(equal? as cs)
                (cons 'collects (map (compose string->bytes/utf-8
                                              path-element->string)
                                     bs))]
               [else path])]
        [else path]))

(define path->collects-relative
  (dynamic-require 'setup/collects 'path->collects-relative
                   (const our-path->collects-relative)))

;; This is a no-op, but that suffices for our use here because we only
;; use it in 6.0+ to revert back to pre-6.0 behavior.
(define (our-current-directory-for-user v)
  (void))

(define current-directory-for-user
  (dynamic-require 'racket/base 'current-directory-for-user
                   (const our-current-directory-for-user)))

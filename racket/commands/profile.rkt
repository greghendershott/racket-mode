#lang racket/base

(require racket/match
         (only-in "../instrument.rkt" get-profile-info))

(provide get-profile)

(define (get-profile)
  ;; TODO: Filter files from racket-mode itself, b/c just noise?
  (for/list ([x (in-list (get-profile-info))])
    (match-define (list count msec name stx _ ...) x)
    (list count
          msec
          (and name (symbol->string name))
          (and (syntax-source stx) (path? (syntax-source stx))
               (path->string (syntax-source stx)))
          (syntax-position stx)
          (and (syntax-position stx) (syntax-span stx)
               (+ (syntax-position stx) (syntax-span stx))))))

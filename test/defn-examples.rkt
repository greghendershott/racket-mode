#lang racket/base

(require racket/contract)

;; For tests of defn.rkt.
;;
;; Until I can figure out how to make this work as a submodule of its
;; `test` submodule.

(define (plain x) x)
(provide plain)
(provide (rename-out [plain renamed]))

(define (contracted1 x) x)
(provide (contract-out [contracted1 (-> any/c any)]))
(define (contracted2 x) x)
(provide/contract [contracted2 (-> any/c any)])

(define (c/r x) x)
(provide (contract-out [rename c/r contracted/renamed (-> any/c any)]))

(define-syntax-rule (plain-definer name)
  (begin
    (define (name x) x)
    (provide name)))
(plain-definer plain-by-macro)

(define-syntax-rule (contracted-definer name)
  (begin
    (define (name x) x)
    (provide (contract-out [name (-> any/c any)]))))
(contracted-definer contracted-by-macro)



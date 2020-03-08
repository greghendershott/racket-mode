#lang racket/base

(require racket/contract)

;; Examples for test/find.rkt.

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

;; This is here to try to trip naive matching, by having a definition
;; of `sub` that is not actually provided, unlike the one in the `sub`
;; module just below.
(module red-herring racket/base
  (define (sub) #f))

(module sub racket/base
  (define (sub x) x)
  (provide sub
           (rename-out [sub sub/renamed])))
(require 'sub)
(provide sub sub/renamed)

;; Likewise, another case of naive matching:
(module red-herring-2 racket/base
  (define (foo) #f))

(define (foo x) x)
(provide foo)

;; Issue 317
(define a-number 42)
(provide a-number)
(define a-parameter (make-parameter #f))
(provide a-parameter)

(module m racket/base
  (define from-m #f)
  (provide from-m))
(require 'm)
(provide (contract-out [from-m any/c]))

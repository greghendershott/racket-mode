;; Copyright (c) 2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(provide safe-dynamic-require
         module-installed?
         rhombus-installed?)

;; Although dynamic-require calls `fail-thunk` when `id` does not
;; exist in `mod`, it raises exn:fail if `mod` doesn't exist.
;;
;; This wrapper calls fail-thunk consistently.
(define (safe-dynamic-require mod id [fail-thunk (λ () #f)])
  (with-handlers ([exn:fail? (λ _ (fail-thunk))])
    (dynamic-require mod id fail-thunk)))

;; Some predicates useful for e.g. tests that may run against various
;; versions of Racket.

(define (module-installed? mod)
  (and (safe-dynamic-require mod #f)
       #t))

(define (rhombus-installed?)
  (module-installed? 'rhombus))

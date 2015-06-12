#lang racket/base

;; This file exists to avoid circular dep between cmds.rkt and
;; debugger.rkt. (Which might be a sign the code should get refactored
;; among files, more than this.)

(provide debug-step?)

;; Parameter-ish signature, but not a parameter because we want to
;; share it among threads.
(define debug-step?
  (let ([old #t])
    (case-lambda
      [() old]
      [(new) (set! old (not (not new)))])))

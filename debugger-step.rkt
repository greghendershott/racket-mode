#lang racket/base

;; This file exists to avoid circular dep between cmds.rkt and
;; debugger.rkt. (Which might be a sign the code should get refactored
;; among files, more than this.)

(provide debug-step?)

(define debug-step? (make-parameter #t))

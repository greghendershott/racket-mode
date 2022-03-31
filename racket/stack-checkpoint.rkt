;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/list
         racket/match
         syntax/parse/define)

(provide with-stack-checkpoint
         continuation-mark-set->trimmed-context)

;;; Inspired by drracket/private/stack-checkpoint.rkt.

;; Run a thunk, and if an exception is raised, make it possible to
;; trim the stack so that the surrounding context is hidden
(define checkpoints (make-weak-hasheq))
(define (call-with-stack-checkpoint thunk)
  (define checkpoint #f)
  (call-with-exception-handler
   (λ (exn)
     (when checkpoint ; just in case there's an exception before it's set
       (define key (if (exn? exn) (exn-continuation-marks exn) exn))
       (unless (hash-has-key? checkpoints key)
         (hash-set! checkpoints key checkpoint)))
     exn)
   (λ ()
     (set! checkpoint (current-continuation-marks))
     (thunk))))

(define-simple-macro (with-stack-checkpoint e:expr ...+)
  (call-with-stack-checkpoint (λ () e ...)))

;; Like continuation-mark-set->context, but trims any tail registered
;; as a checkpoint, as well as removing items lacking srcloc.
(define (continuation-mark-set->trimmed-context cms)
  (define stack (continuation-mark-set->context cms))
  (filter
   cdr ;only non-#f srcloc
   (match (hash-ref checkpoints cms #f)
     [(? continuation-mark-set? v)
      (define checkpoint (continuation-mark-set->context v))
      ;; To drop the common tail, reverse both and use drop-common-prefix.
      (define-values (trimmed _) (drop-common-prefix (reverse stack)
                                                     (reverse checkpoint)))
      (match trimmed
        ;; The mark for call-with-stack-checkpoint is the head; ignore
        ;; it. Reverse the remainder back to stack order.
        [(cons _ xs) (reverse xs)]
        ;; Can happen with Racket < 7.0 and debugger REPL.
        [_           '()])]
     [#f stack])))

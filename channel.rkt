#lang racket/base

(require racket/match
         racket/contract
         "mod.rkt")

(provide message-to-main-thread-channel
         (struct-out message-to-main-thread)
         (struct-out load-gui)
         (struct-out rerun)
         rerun-default
         context-level?
         instrument-level?
         profile/coverage-level?)


;;; Definitions for the context-level member of rerun

(define profile/coverage-levels
  ;; "sibling" levels that need instrument plus...
  '(profile    ;profiling-enabled
    coverage)) ;execute-counts-enabled

(define instrument-levels
  `(high     ;compile-context-preservation-enabled #t + instrument
    ,@profile/coverage-levels))

(define context-levels
  `(low      ;compile-context-preservation-enabled #f
    medium   ;compile-context-preservation-enabled #t
    ,@instrument-levels))

(define-syntax-rule (memq? x xs)
  (not (not (memq x xs))))

(define (context-level? v)
  (memq? v context-levels))

(define (instrument-level? v)
  (memq? v instrument-levels))

(define (profile/coverage-level? v)
  (memq? v profile/coverage-levels))

;;; Messages to the main thread via a channel

(define message-to-main-thread-channel (make-channel))

(define-struct/contract message-to-main-thread ())

(define-struct/contract (load-gui message-to-main-thread)
  ([in-repl? boolean?]))

(define-struct/contract (rerun message-to-main-thread)
  ([maybe-mod     (or/c #f mod?)]
   [memory-limit  exact-nonnegative-integer?] ;0 = no limit
   [pretty-print? boolean?]
   [context-level context-level?]
   ;; The following contract is the weaker `vector?` instead of
   ;; `(vectorof string?)` because latter fails under Racket 6.0 and
   ;; 6.1 when the value is accessed from the struct and passed to
   ;; `current-command-line-arguments`. WAT.
   [cmd-line-args vector?]
   [ready-thunk   (-> any/c)]))

(define rerun-default (rerun #f
                             0
                             #f
                             'low
                             #()
                             void))

#lang racket/base

(require racket/match
         racket/contract
         "mod.rkt")

(provide main-channel
         (struct-out msg)
         (struct-out load-gui)
         (struct-out rerun)
         rerun-default
         context-level?
         instrument-level?
         profile/coverage-level?
         put/stop)

;; Definitions for the context-level member of rerun

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

;; Messages to the main thread via a channel
(define main-channel (make-channel))
(define-struct/contract msg ())
(define-struct/contract [load-gui msg] ())
(define-struct/contract [rerun msg]
  ([maybe-mod     (or/c #f mod?)]
   [memory-limit  exact-nonnegative-integer?] ;0 = no limit
   [pretty-print? boolean?]
   [context-level context-level?]
   ;; The following contract is the weaker `vector?` instead of
   ;; `(vectorof string?)` because latter fails under Racket 6.0 and
   ;; 6.1 when the value is accessed from the struct and passed to
   ;; `current-command-line-arguments`. WAT.
   [cmd-line-args vector?]))

(define rerun-default (rerun #f 0 #f 'low #()))

;; To be called from REPL thread. Puts message for the main thread to
;; the channel, and blocks itself; main thread will kill the REPL
;; thread. Effectively "exit the thread with a return value".
(define (put/stop v) ;; msg? -> void?
  (channel-put main-channel v)
  (void (sync never-evt)))

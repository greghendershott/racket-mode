#lang at-exp racket/base

(require racket/format
         racket/match
         "util.rkt")

(provide next-session-id!
         call-with-session-context
         current-session-id
         current-repl-msg-chan
         current-session-maybe-mod
         current-session-submit-pred
         (struct-out session)
         get-session
         set-session!
         remove-session!)

;;; REPL session "housekeeping"

;; Session IDs are strings based on time + monotonic number
(define next-session-id!
  (let ([n 0])
    (λ ()
      (format "repl-session-~a-~a"
              (current-inexact-milliseconds)
              (begin0 n
                (inc! n))))))

;; Each REPL session has an entry in this hash-table.
(define sessions (make-hash)) ;string? => session?

(struct session
  (thread           ;thread? the repl manager thread
   repl-msg-chan    ;channel?
   maybe-mod        ;(or/c #f mod?)
   namespace        ;namespace?
   submit-pred)     ;(or/c #f drracket:submit-predicate/c)
  #:transparent)

(define (get-session sid)
  (hash-ref sessions sid #f))

(define (set-session! sid maybe-mod repl-submit-predicate)
  (hash-set! sessions sid (session (current-thread)
                                   (current-repl-msg-chan)
                                   maybe-mod
                                   (current-namespace)
                                   repl-submit-predicate))
  (log-racket-mode-debug @~a{(set-session! @~v[sid] @~v[maybe-mod] @~v[repl-submit-predicate]) => sessions: @~v[sessions]}))

(define (remove-session! sid)
  (hash-remove! sessions sid)
  (log-racket-mode-debug @~a{(remove-session! @~v[sid]) => sessions: @~v[sessions]}))

(define current-session-id (make-parameter #f))
(define current-repl-msg-chan (make-parameter #f))
(define current-session-maybe-mod (make-parameter #f))
(define current-session-submit-pred (make-parameter #f))

;; A way to parameterize e.g. commands that need to work with a
;; specific REPL session. Called from e.g. a command-server thread.
(define (call-with-session-context sid proc . args)
  (match (get-session sid)
    [(? session? s)
     (log-racket-mode-debug @~a{(call-with-session-context @~v[sid] @~v[proc] @~v[args]) => @~v[s]})
     (parameterize ([current-session-id          sid]
                    [current-repl-msg-chan       (session-repl-msg-chan s)]
                    [current-session-maybe-mod   (session-maybe-mod s)]
                    [current-namespace           (session-namespace s)]
                    [current-session-submit-pred (session-submit-pred s)])
       (apply proc args))]
    [_
     (if (equal? sid '())
         (log-racket-mode-debug @~a{(call-with-session-context @~v[sid] @~v[proc] @~v[args]): no specific session})
         (log-racket-mode-warning @~a{(call-with-session-context @~v[sid] @~v[proc] @~v[args]): @~v[sid] not found in @~v[sessions]}))
     (apply proc args)]))

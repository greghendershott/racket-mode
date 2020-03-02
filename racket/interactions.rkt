#lang racket/base

(require racket/match
         "fresh-line.rkt"
         "util.rkt")

(provide current-interaction-chan
         current-sync/yield
         make-get-interaction
         get-interaction)

;; A channel to which a thread puts interactions that it reads using
;; the current-read-interaction handler (which can be set by a lang
;; from its configure-runtime, so, this should be compatible with
;; any lang, even non-sexpr langs).
;;
;; This is its own thread and channel for a couple reasons:
;;
;; - Issue #311. A consumer can use sync/timeout to avoid displaying a
;;   prompt when multiple interactions are waiting.
;;
;; - Debugging. We can switch from the normal REPL to a debugger REPL,
;;   without input being stuck inside a read call for the former.
;;
;; One wrinkle is we need to be careful about calling yield instead of
;; sync when the gui is active. See issue #326.
;;
;; Another wrinkle: We should handle eof-object? and exn:fail:network?
;; by doing an exit and letting the exit-handler in run.rkt cleanup
;; the TCP connection. This handles the case where e.g. the user kills
;; the REPL buffer and its process on the client/Emacs side. We used
;; to have code here in an effort support lang/datalog using eof as an
;; expression separator -- but that just causes an endless loop 100%
;; CPU spike with an abandoned tcp-input-port. So give up on that,
;; reverting issue #305.

(define current-interaction-chan (make-parameter #f))

;; Call this from a REPL manager thread after current-input-port is
;; set appropriately (e.g. to a TCP input port not stdin).
(define (make-get-interaction)
  (define ch (make-channel))
  (current-interaction-chan ch)
  (thread read-interaction/put-channel)
  ch)

(define (read-interaction/put-channel)
  (define in ((current-get-interaction-input-port)))
  (define (read-interaction)
    (with-handlers ([exn:fail? values])
      ((current-read-interaction) (object-name in) in)))
  (match (read-interaction)
    [(? eof-object?) (log-racket-mode-info "read-interaction: eof")
                     (exit 'get-interaction-eof)]
    [(? exn:fail? e) (channel-put (current-interaction-chan) e)] ;raise in other thread
    [v (channel-put (current-interaction-chan) v)])
  (read-interaction/put-channel))

(define current-sync/yield (make-parameter sync)) ;see issue #326

(define (get-interaction prompt)
  (match (or (sync/timeout 0.01 (current-interaction-chan)) ;see issue #311
             (begin (display-prompt prompt)
                    ((current-sync/yield) (current-interaction-chan))))
    [(? exn:fail:network?) (log-racket-mode-info "get-interaction: exn:fail:network\n")
                           (exit 'get-interaction-exn:fail:network)]
    [(? exn:fail? exn) (raise exn)]
    [v v]))

(define (display-prompt str)
  (flush-output (current-error-port))
  (fresh-line)
  (display str)
  (display "> ")
  (flush-output)
  (zero-column!))

#lang racket/base

(require racket/match
         "fresh-line.rkt"
         "stack-checkpoint.rkt"
         "util.rkt")

(provide get-interaction)

;; Note: We should handle eof-object? and exn:fail:network? by doing
;; an exit and letting the exit-handler in run.rkt cleanup the TCP
;; connection. This handles the case where e.g. the user kills the
;; REPL buffer and its process on the client/Emacs side. We used to
;; have code here in an effort support lang/datalog using eof as an
;; expression separator -- but that just causes an endless loop 100%
;; CPU spike with an abandoned tcp-input-port. So give up on that,
;; reverting issue #305.

(define (get-interaction prompt)
  (match (with-handlers ([exn:fail? values])
           (define in ((current-get-interaction-input-port)))
           (unless (already-more-to-read? in) ;#311
             (display-prompt prompt))
           (with-stack-checkpoint
             ((current-read-interaction) prompt in)))
    [(? eof-object?)
     (log-racket-mode-info "get-interaction: eof")
     (exit 'get-interaction-eof)]
    [(? exn:fail:network?)
     (log-racket-mode-info "get-interaction: exn:fail:network\n")
     (exit 'get-interaction-exn:fail:network)]
    [(? exn:fail? exn)
     (raise exn)]
    [v v]))

(define (already-more-to-read? in)
  ;; Is there already at least one more read-able expression on in?
  ;;
  ;; - Use a "peeking read" so that, if the answer is yes, we don't
  ;;   actually consume it (which could cause #449).
  ;;
  ;; - Use a thread + channel + sync/timeout so that, if the answer is
  ;;   no because there is only a partial sexp -- e.g. "(+ 1" -- we
  ;;   don't get stuck here.
  (define ch (make-channel))
  (thread
   (λ ()
     (channel-put ch
                  (with-handlers ([exn:fail? (λ _ #f)])
                    (let* ([buf  (make-bytes 4096 0)]
                           [len  (peek-bytes-avail!* buf 0 #f in)]
                           [bstr (subbytes buf 0 len)]
                           [v    (read (open-input-bytes bstr))])
                      (not (eof-object? v)))))))
  (sync/timeout 0.01 ch))

(define (display-prompt str)
  (flush-output (current-error-port))
  (fresh-line)
  (display str)
  (display "> ")
  (flush-output)
  (zero-column!))

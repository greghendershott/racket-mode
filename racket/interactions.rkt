#lang racket/base

(require racket/match
         racket/port
         "stack-checkpoint.rkt"
         "util.rkt")

(provide get-interaction)

;; Note: We handle eof-object? and exn:fail:network? by doing an exit
;; and letting the exit-handler in run.rkt cleanup the TCP connection.
;; This handles the case where e.g. the user kills the REPL buffer and
;; its process on the client/Emacs side. We used to have code here in
;; an effort support lang/datalog using eof as an expression separator
;; -- but that just causes an endless loop 100% CPU spike with an
;; abandoned tcp-input-port. So give up on that, reverting issue #305.

(define (get-interaction prompt)
  ;; Using with-handlers here would be a mistake; see #543.
  (call-with-exception-handler
   (λ (e)
     (cond [(exn:fail:network? e)
            (log-racket-mode-info "get-interaction: exn:fail:network")
            (exit 'get-interaction-exn:fail:network)]
           [else e]))
   (λ ()
     (define in ((current-get-interaction-input-port)))
     ;; Need to port-count-lines! here -- not sufficient to do to REPL
     ;; TCP input port upon connection -- because racket/gui/base sets
     ;; current-get-interaction-port to wrap the original input port.
     ;; See issues #519 #556.
     (port-count-lines! in)
     (unless (already-more-to-read? in) ;#311
       (display-prompt prompt))
     (match (with-stack-checkpoint
              ((current-read-interaction) prompt in))
       [(? eof-object?)
        (log-racket-mode-info "get-interaction: eof")
        (exit 'get-interaction-eof)]
       [v
        (zero-column!)
        v]))))

(define (already-more-to-read? in)
  ;; Is there already at least one more expression available to read
  ;; from the input port?
  ;;
  ;; - Use a "peeking read" so that, if the answer is yes, we don't
  ;;   actually consume it (which could cause #449).
  ;;
  ;;   To handle multiple expressions the underlying tcp-input-port
  ;;   needs block buffer-mode; see issue #582 (it seems to be fine
  ;;   that racket/gui/base's current-get-interaction-port wrapper for
  ;;   that underlying tcp port reports #f for the buffer-mode).
  ;;
  ;; - Use a thread + channel + sync/timeout so that, if the answer is
  ;;   no because there is only a partial sexp -- e.g. "(+ 1" -- we
  ;;   don't get stuck inside `read`.
  (define ch (make-channel))
  (thread
   (λ ()
     (channel-put ch
                  (with-handlers ([exn:fail? (λ _ #f)])
                    (define pin (peeking-input-port in))
                    (define v ((current-read-interaction) #f pin))
                    (not (eof-object? v))))))
  (sync/timeout 0.01 ch))

(define (display-prompt str)
  (fresh-line)
  (display str)
  (display "> ")
  (flush-output))

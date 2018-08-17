#lang racket/base

(provide fresh-line
         zero-column!)

;; Borrowed from xrepl

(define last-output-port #f)
(define last-error-port  #f)

(define (maybe-new-output-ports)
  (define-syntax-rule (maybe last cur)
    (unless (eq? last cur)
      (when (and last
                 (not (port-closed? last)))
        (flush-output last)) ;just in case
      (set! last cur)
      (flush-output last)
      (port-count-lines! last)))
  (maybe last-output-port (current-output-port))
  (maybe last-error-port (current-error-port)))

(define (fresh-line [stderr? #f])
  (maybe-new-output-ports)
  (define port (if stderr? last-error-port last-output-port))
  (flush-output port)
  (define-values [line col pos] (port-next-location port))
  (unless (eq? col 0) (newline)))

(define (zero-column!)
  ;; there's a problem whenever there's some printout followed by a
  ;; read: the cursor will be at column zero, but the port counting
  ;; will think that it's still right after the printout; call this
  ;; function in such cases to adjust the column to 0.
  (maybe-new-output-ports)
  (define-values [line col pos] (port-next-location last-output-port))
  (set-port-next-location! last-output-port line 0 pos))

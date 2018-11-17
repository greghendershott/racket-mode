#lang racket/base

(require racket/pretty
         racket/runtime-path
         "image.rkt")

(provide set-print-parameters)

(define (set-print-parameters pretty-print?)
  (cond [pretty-print?
         (current-print pretty-print-handler)
         (pretty-print-print-hook (make-pretty-print-print-hook))
         (pretty-print-size-hook (make-pretty-print-size-hook))]
        [else
         (current-print plain-print-handler)])
  (print-syntax-width +inf.0))

(define (plain-print-handler v)
  (void (unless (void? v)
          (print (convert-image v))
          (newline))))

(define (make-pretty-print-size-hook)
  (let ([orig (pretty-print-size-hook)]
        [width (floor (/ (pretty-print-columns) 4))]) ;magic number? yep.
    (λ (value display? port)
      (cond [(convert-image? value) width]
            [else                   (orig value display? port)]))))

(define (make-pretty-print-print-hook)
  (let ([orig (pretty-print-print-hook)])
    (λ (value display? port)
      (cond [(convert-image? value) (print (convert-image value) port)]
            [else                   (orig value display? port)]))))

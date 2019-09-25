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

;; We have nothing valuable to say about (a) pretty-print-columns as
;; it relates to Emacs window width or (b) the width in "characters"
;; of any particular image. Even so, we must say something. We must
;; supply a pretty-print-size-hook that returns _some_ integer width
;; -- else our pretty-print-hook won't be called. So simply return
;; pretty-print-columns. This means that e.g. a list of images will be
;; printed on multiple lines even if they could fit on one line.
;; That's less-worse than trying to be "clever"; see issue #402.
(define (make-pretty-print-size-hook)
  (let ([orig (pretty-print-size-hook)])
    (λ (value display? port)
      (cond [(convert-image? value) (pretty-print-columns)]
            [else                   (orig value display? port)]))))

(define (make-pretty-print-print-hook)
  (let ([orig (pretty-print-print-hook)])
    (λ (value display? port)
      (cond [(convert-image? value) (print (convert-image value) port)]
            [else                   (orig value display? port)]))))

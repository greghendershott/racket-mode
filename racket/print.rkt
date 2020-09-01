#lang racket/base

(require racket/match
         racket/pretty
         "image.rkt")

(provide set-print-parameters)

(define (set-print-parameters pretty? columns pixels/char)
  (cond [pretty?
         (current-print pretty-print-handler)
         (pretty-print-columns columns)
         (pretty-print-size-hook (make-pretty-print-size-hook pixels/char))
         (pretty-print-print-hook (make-pretty-print-print-hook))]
        [else
         (current-print plain-print-handler)])
  (print-syntax-width +inf.0))

(define (plain-print-handler v)
  (unless (void? v)
    (println (match (convert-image v)
               [(cons path-name _pixel-width) path-name]
               [_ v]))))

;; pretty-print uses separate size and print hooks -- and the size
;; hook can even be called more than once per object. Avoid calling
;; convert-image two (or more!) times per object. That could be slow
;; for large images; furthermore each call creates a temp file.
;;
;; Instead: Call convert-image once in the size hook, storing the
;; result in a hash-table for use across later calls to the size
;; and/or print hook. Remove in the print hook.
;;
;; (Note: Although I had tried using the pre-print and post-print
;; hooks, they seemed to be called inconsistently.)

(define ht (make-hasheq))

(define (make-pretty-print-size-hook pixels/char)
  (define (racket-mode-size-hook value _display? _port)
    (define (not-found)
      (match (convert-image value)
        [(cons path-name pixel-width)
         (define char-width (ceiling (/ pixel-width pixels/char)))
         (define path+width (cons path-name char-width))
         path+width]
        [#f #f]))
    (match (hash-ref! ht value not-found)
      [(cons _path-name char-width) char-width]
      [#f #f]))
  racket-mode-size-hook)

(define (make-pretty-print-print-hook)
  (define orig (pretty-print-print-hook))
  (define (racket-mode-print-hook value display? port)
    (match (hash-ref ht value #f)
      [(cons path-name _char-width)
       (hash-remove! ht value)
       ((if display? display print) path-name port)]
      [#f (orig value display? port)]))
  racket-mode-print-hook)

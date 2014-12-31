#lang racket/base

;;; Portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

(require file/convertible
         racket/file
         racket/vector)

(provide maybe-convert-image)

;; save-temporary-image : bytes? -> path?
;;
;; Write the bytes to a temporary file and return its name.
(define (save-temporary-image png-bytes)
  (define filename (make-temporary-file "racket-image-~a.png"))
  (with-output-to-file filename #:exists 'truncate
    (λ () (display png-bytes)))
  filename)

;; maybe-convert-image : any/c -> any/c
;;
;; Replace PNG image values, writing them to a temporary file, and replace
;; with a string "#<Image: filename>".
(define (maybe-convert-image v)
  (define (png? v)
    (and (convertible? v) (convert v 'png-bytes)))
  (cond [(png? v)    => (λ (b) (format "#<Image: ~a>" (save-temporary-image b)))]
        ;; [(list? v)   (map        maybe-convert-image v)]
        ;; [(vector? v) (vector-map maybe-convert-image v)]
        [else        v]))

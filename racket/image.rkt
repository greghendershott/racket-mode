#lang racket/base

;;; Portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

(require file/convertible
         racket/file
         racket/match)

(provide convert-image?
         convert-image)

;; Write bytes to a temporary file and return "#<Image: filename>".
(define (save-temporary-image bytes+ext)
  (match-define (cons bstr ext) bytes+ext)
  (define filename (make-temporary-file (string-append "racket-image-~a." ext)))
  (with-output-to-file filename #:exists 'truncate
    (λ () (display bstr)))
  (format "#<Image: ~a>" filename))

(define (convert-image? v)
  (convertible? v))

(define (convert-image v)
  (cond [(and (convertible? v)
              (or (maybe-convert v 'svg-bytes "svg")
                  (maybe-convert v 'png-bytes "png")))
         => save-temporary-image]
        [else v]))

(define (maybe-convert v fmt ext)
  (match (convert v fmt)
    [(? bytes? bstr) (cons bstr ext)]
    [_ #f]))

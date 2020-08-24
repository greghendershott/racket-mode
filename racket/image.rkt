#lang racket/base

;;; Portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

(require file/convertible
         racket/file
         racket/format
         racket/match)

(provide convert-image)

(define (convert-image v)
  (and (convertible? v)
       (or (convert-and-save v 'svg-bytes+bounds "svg")
           (convert-and-save v 'png-bytes+bounds "png"))))

(define (convert-and-save v fmt ext)
  (match (convert v fmt)
    [(list* (? bytes? bstr) width _)
     (define filename (make-temporary-file (~a "racket-image-~a." ext)))
     (with-output-to-file filename #:exists 'truncate (λ () (display bstr)))
     (cons (format "#<Image: ~a>" filename) width)]
    [_ #f]))


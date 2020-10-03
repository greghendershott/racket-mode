#lang racket/base

;;; Portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

(require file/convertible
         racket/file
         racket/format
         racket/match)

(provide convert-image)

(define (convert-image v)
  (and (convertible? v)
       (for/or ([fmt/ext (in-list '((svg-bytes+bounds "svg")
                                    (png-bytes+bounds "png")
                                    (svg              "svg")
                                    (png-bytes        "png")))])
         (apply convert-and-save v fmt/ext))))

(define (convert-and-save v fmt ext)
  (define (default-width _) 4096)
  (match (convert v fmt)
    [(or (list* (? bytes? bstr) width _)                  ;bytes+bounds
         (and (? bytes? bstr) (app default-width width))) ;bytes
     (define filename (make-temporary-file (~a "racket-image-~a." ext)))
     (with-output-to-file filename #:exists 'truncate (λ () (display bstr)))
     (cons (format "#<Image: ~a>" filename) width)]
    [_ #f]))

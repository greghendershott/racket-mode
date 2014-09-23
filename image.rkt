#lang racket/base

;;; Portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

(require file/convertible
         racket/file
         racket/pretty)

(provide current-print/images)

;; save-temporary-image : bytes? -> path?
;;
;; Write the bytes to a temporary file and return its name.
(define (save-temporary-image png-bytes)
  (define filename (make-temporary-file "racket-image-~a.png"))
  (with-output-to-file filename #:exists 'truncate
    (λ () (display png-bytes)))
  filename)

;; current-print/images : any/c -> void?
;;
;; A replacement for current-print that intercepts PNG image values
;; and writes them to a temporary file, printing #<Image: filename>.
(define (current-print/images value)
  (void
   (unless (void? value)
     (cond [(and (convertible? value)
                 (convert value 'png-bytes))
            => (lambda (png-bytes)
                 ;; (The above could be problematic if a future version
                 ;; of racket suddenly decides it can "convert" strings
                 ;; to picts)
                 (printf "#<Image: ~a>\n" (save-temporary-image png-bytes)))]
           [else (print value)
                 (newline)]))))

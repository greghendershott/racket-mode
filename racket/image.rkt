#lang racket/base

;;; Portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

(require file/convertible
         racket/file
         racket/format
         racket/match)

(provide emacs-can-use-svg!
         convert-image)

;; Emacs front end tells us whether SVG is an image file type Emacs
;; can render.
(define use-svg? #t)
(define (emacs-can-use-svg! command-line-flag-str)
  (set! use-svg? (equal? command-line-flag-str "--use-svg")))

(define (convert-image v)
  (and (convertible? v)
       ;; Rationale for the order here:
       ;;
       ;; - Try bounded before unbounded flavors. Because we want
       ;;   accurate image width, if available, for pretty-printing.
       ;;
       ;; - Within each flavor: Try svg (if this Emacs can use it)
       ;;   before png. Because space.
       (let ([fmts/exts (if use-svg?
                            '((svg-bytes+bounds "svg")
                              (png-bytes+bounds "png")
                              (svg-bytes        "svg")
                              (png-bytes        "png"))
                            '((png-bytes+bounds "png")
                              (png-bytes        "png")))])
         (for/or ([fmt/ext (in-list fmts/exts)])
           (apply convert-and-save v fmt/ext)))))

(define (convert-and-save v fmt ext)
  (define (default-width _) 4096)
  (match (convert v fmt)
    [(or (list* (? bytes? bstr) width _)                  ;bytes+bounds
         (and (? bytes? bstr) (app default-width width))) ;bytes
     (define filename (make-temporary-file (~a "racket-image-~a." ext)))
     (with-output-to-file filename #:exists 'truncate (λ () (display bstr)))
     (cons (format "#<Image: ~a>" filename) width)]
    [_ #f]))

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

;;; Portions Copyright (C) 2012 Jose Antonio Ortega Ruiz.

(require file/convertible
         racket/file
         racket/format
         racket/match)

(provide set-use-svg?!
         convert-image)

;; Emacs front end tells us whether SVG is an image file type Emacs
;; can render. This comes via a command line flag when we start up.
(define use-svg? #t)
(define (set-use-svg?! v) (set! use-svg? v))

;; For a given value, pretty-print-size-hook can be called multiple
;; times (!) followed once by pretty-print-print-hook. So because
;; convert-and-save does non-trivial work, we cache.
(define ht (make-weak-hasheq)) ;weak because #624

(define (convert-image v #:remove-from-cache? [remove? #f])
  (and (convertible? v)
       (begin0 (hash-ref! ht v
                          (λ () (raw-convert-image v)))
         (when remove?
           (hash-remove! ht v)))))

(define (raw-convert-image v)
  ;; Rationale for the order here:
  ;;
  ;; - Try bounded before unbounded flavors. Because we want
  ;;   accurate image width, if available, for pretty-printing.
  ;;
  ;; - Within each flavor: Try svg (if this Emacs can use it)
  ;;   before png. Because space.
  (define fmts/exts (if use-svg?
                        '((svg-bytes+bounds8 "svg")
                          (png-bytes+bounds8 "png")
                          (svg-bytes+bounds  "svg")
                          (png-bytes+bounds  "png")
                          (svg-bytes         "svg")
                          (png-bytes         "png"))
                        '((png-bytes+bounds8 "png")
                          (png-bytes+bounds  "png")
                          (png-bytes         "png"))))
  (for/or ([fmt/ext (in-list fmts/exts)])
    (apply convert-and-save v fmt/ext)))

(define (convert-and-save v fmt ext)
  (define (default-width _) 4096)
  (match (convert v fmt #f)
    [(or (list* (? bytes? bstr) width _)                  ;bytes+bounds
         (and (? bytes? bstr) (app default-width width))) ;bytes
     (define filename (make-temporary-file (~a "racket-image-~a." ext)))
     (with-output-to-file filename #:exists 'truncate (λ () (display bstr)))
     (cons (path->string filename) width)]
    [#f #f]))

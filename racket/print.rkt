;; Copyright (c) 2013-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/match
         racket/pretty
         "image.rkt"
         (only-in "repl-output.rkt"
                  print-images-as-specials?))

(provide make-pretty-global-port-print-handler)

(define (make-pretty-global-port-print-handler columns pixels/char)
  (define (racket-mode-pretty-global-port-print-handler v out [depth 0])
    (unless (void? v)
      (if (print-images-as-specials?)
          (parameterize ([print-syntax-width     +inf.0]
                         [pretty-print-columns   columns]
                         [pretty-print-size-hook (size-hook pixels/char)]
                         [pretty-print-print-hook print-hook])
            (pretty-print v out depth #:newline? #f))
          (pretty-print v out depth #:newline? #f))))
  racket-mode-pretty-global-port-print-handler)

;; Return char width of convertible image.
(define ((size-hook pixels/char) value _display? _port)
  (match (convert-image value) ;caches
    [(cons _path-name pixel-width)
     (inexact->exact
      (ceiling
       (/ pixel-width pixels/char)))]
    [#f #f]))

;; Note: "The print-hook procedure is applied to a value for printing
;; when the sizing hook (see pretty-print-size-hook) returns an
;; integer size for the value." i.e. But not called otherwise.
(define (print-hook value _display? port)
  (match (convert-image value #:remove-from-cache? #t)
    [(cons path-name _pixel-width)
     (write-special (cons 'image path-name) port)]))

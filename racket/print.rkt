;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/format
         racket/match
         racket/pretty
         "repl-output.rkt"
         "image.rkt")

(provide make-racket-mode-print-handler)

(define (make-racket-mode-print-handler pretty? columns pixels/char)
  (define (racket-mode-print-handler v)
   (unless (void? v)
     (parameterize ([current-output-port (make-repl-value-port)]
                    [print-syntax-width +inf.0])
       (cond
         [pretty?
          (parameterize ([pretty-print-columns columns]
                         [pretty-print-size-hook (make-pp-size-hook pixels/char)]
                         [pretty-print-print-hook (make-pp-print-hook)])
            (pretty-print v))]
         [else
          (match (convert-image v)
            [(cons path-name _pixel-width)
             (write-special (cons 'image path-name))]
            [_
             (print v)])]))))
  racket-mode-print-handler)

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
;;
;; Also: "The print-hook procedure is applied to a value for printing
;; when the sizing hook (see pretty-print-size-hook) returns an
;; integer size for the value." i.e. But not called otherwise.

(define ht (make-weak-hasheq)) ;weak because #624

(define (make-pp-size-hook pixels/char)
  (define (racket-mode-size-hook value display? port)
    (define (not-found)
      (match (convert-image value)
        [(cons path-name pixel-width)
         (define char-width (inexact->exact (ceiling (/ pixel-width pixels/char))))
         (cons path-name char-width)]
        [#f #f]))
    (match (hash-ref! ht value not-found)
      [(cons _path-name char-width) char-width]
      [#f #f]))
  racket-mode-size-hook)

;; Only called if size-hook returned an integer size.
(define (make-pp-print-hook)
  (define orig (pretty-print-print-hook))
  (define (racket-mode-print-hook value display? port)
    (match (hash-ref ht value #f)
      [(cons path-name _char-width)
       (hash-remove! ht value)
       (write-special (cons 'image path-name))]
      [_ ;shouldn't happen, but...
       (orig value display? port)]))
  racket-mode-print-hook)

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/match
         racket/port
         racket/pretty
         "image.rkt"
         "repl-output.rkt")

(provide make-racket-mode-print-handler
         current-value-port)

(define current-value-port (make-parameter #f))

(define (make-racket-mode-print-handler pretty? columns pixels/char)
  (define (racket-mode-print-handler v)
    (unless (void? v)
      (define-values (in out) (make-value-pipe))
      (parameterize ([current-output-port out]
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
              (print v)])]))
      (drain-value-pipe in out)))
  racket-mode-print-handler)

;; Because pretty-print does a print for each value within a list,
;; plus for each space and newline, etc., it can result in many calls
;; to repl-output-value with short strings.
;;
;; To avoid this: Use for current-output-port a pipe of unlimited size
;; to accumulate all the pretty-printed bytes and specials. Finally
;; drain it using read-bytes-avail! to consolidate runs of bytes
;; (interrupted only by specials, if any) up to a fixed buffer size.

(define (make-value-pipe)
  (make-pipe-with-specials))

(define (drain-value-pipe in out)
  (flush-output out)
  (close-output-port out)
  (define buffer (make-bytes 2048))
  (let loop ()
    (match (read-bytes-avail! buffer in)
      [(? exact-nonnegative-integer? len)
       (define v (bytes->string/utf-8 (subbytes buffer 0 len)))
       (repl-output-value v)
       (loop)]
      [(? procedure? read-special)
       ;; m-p-w-specials ignores the position arguments so just pass
       ;; something satisfying the contract.
       (define v (read-special #f #f #f 1))
       (repl-output-value-special v)
       (loop)]
      [(? eof-object?) (void)])))

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

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

;;; `racket-open-require-path' uses `tq' to run us. We repeatedly
;;; read-line a query and display the answer as lines terminated by a
;;; blank line.
;;;
;;; This was created because the original attempt, using
;;; `racket--eval/sexpr', couldn't keep up with fast typing. This new
;;; approach is more direct (e.g. no converting to/from sexprs) and
;;; fast enough. Using `tq' provides a "type-ahead buffer" (in lieu of
;;; the old approach's use of `run-with-timer') even though in my
;;; testing so far it's rarely needed.
;;;
;;; The case where `find-module-path-completions' isn't available: We
;;; don't error, we simply always return empty matches. (This might
;;; not be ideal but I initially had trouble making `tq' recognize
;;; e.g. an (exit 1) here and handle it smoothly. Maybe it would work
;;; to change our "protocol" to have an initial question and answer
;;; devoted to this. For example "HELLO?\n" => "OK\n\n" / "ERROR\n\n".
;;; Thereafter the status quo loop.)

(require racket/match
         "util.rkt")

(module+ main
  (define dir (current-directory)) ;FIXME: Get from command-line
  (define display-choices (init dir))
  (let loop ()
    (define str (read-line))
    (unless (string=? "" str)
      (display-choices str)
      (displayln "") ;; terminating blank line
      (flush-output)
      (loop)))
  (exit 0))

(define-polyfill (find-module-path-completions dir)
  #:module drracket/find-module-path-completions
  (λ (_str) (list)))

(define (init dir)
  (define get (find-module-path-completions dir))
  (λ (str)
    (for ([x (in-list (get str))])
      (displayln (path->string (cadr x))))))

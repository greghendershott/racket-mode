;; Copyright (c) 2013-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require setup/xref)

(provide get-xref)

;; A single xref instance for all our modules to share.
;;
;; Will block safely until ready, if used from e.g. delay/thread or
;; delay/idle (which, although we're not doing now, we've done before,
;; and might do again someday).
(define sema (make-semaphore 1))
(define xref (call-with-semaphore sema load-collections-xref))
(define (get-xref) (call-with-semaphore sema (λ () xref)))

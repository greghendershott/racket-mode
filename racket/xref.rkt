;; Copyright (c) 2013-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require setup/xref)

(provide xref
         xref-ready-evt)

(define xref-ready-evt never-evt)
(define xref (begin0 (load-collections-xref)
               (set! xref-ready-evt always-evt)))

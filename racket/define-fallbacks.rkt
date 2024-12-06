;; Copyright (c) 2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base
                     (only-in "safe-dynamic-require.rkt"
                              safe-dynamic-require))
         syntax/parse/define)

(provide define-fallbacks)

;; safe-dynamic-require is most useful in scenarios where an entire
;; module might not be installed. Note that tools like
;; go-to-definition will always go to the safe-dynamic-require site,
;; because that is the binding site. Any binding from a normal
;; (non-dynamic) require is shadowed by the dynamic require.
;;
;; Another scenario is where a module is always installed, but over
;; time has added exports; therefore an older version might be
;; installed. In this case it can be nicer to do a plain, non-dynamic
;; require of the module, and use define-fallbacks to create
;; definitions /only/ for identifiers not supplied by the installed
;; version of the module. As a result, tools like go-to-definition
;; will handle normally imported bindings in the usual way (go to the
;; definition in that other module's source), which is very
;; convenient.

(define-syntax-parser define-fallback
  [(_ mod:id (id:id arg:expr ...) body:expr ...+)
   (if (safe-dynamic-require (syntax-e #'mod) (syntax-e #'id))
       #'(begin)
       #'(define (id arg ...)
           body ...))])

(define-syntax-parser define-fallbacks
  [(_ mod:id [(id:id arg:expr ...) body:expr ...+] ...+)
   #`(begin
       (define-fallback mod (id arg ...) body ...) ...)])

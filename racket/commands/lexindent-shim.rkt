#lang racket/base

;; This is just a "shim" to provide a more-helpful error message to
;; the user -- at the time they attempt to use a specific command that
;; needs the `lexindent` function -- to handle e.g. that lexindent.rkt
;; requires an unavailable collection.
;;
;; This is in preparation for splitting out token-map.rkt et al into
;; their own package(s). I don't yet know what the name will be, ergo
;; the "???" in the error message. (It won't necessarily be a package,
;; at first, pending review by other people. Plus eventually it might
;; be distributed as part of the drracket-tool-lib package. All TBD.)

(require racket/runtime-path)

(provide lexindent)

(define (fail-thunk . _)
  (error
   'lexindent
   "Could not load lexindenter; maybe you need to `raco pkg install ???`"))

(define-runtime-path lexindent.rkt "lexindent.rkt")

(define lexindent
  (with-handlers ([exn:fail? fail-thunk])
    (dynamic-require lexindent.rkt 'lexindent fail-thunk)))

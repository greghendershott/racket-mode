#lang racket/base

;; This is just a "shim" to provide a more-helpful error message to
;; the user, at the time they attempt to use a specific command that
;; needs the `lexindent` function. To handle e.g. that lexindent.rkt
;; requires an unavailable collection.
;;
;; This is in preparation for splitting out token-map.rkt et al into
;; their own package(s). I don't yet know what the name will be, ergo
;; the "???" in the error message. (Pending review by other people, it
;; won't necessarily even be a package, at first. Eventually it might
;; even become part of the drracket package. All TBD.)

(provide lexindent)

(define (fail-thunk . _)
  (error 'lexindent "Could not load lexindenter; maybe you need to `raco pkg install ???`"))

(define lexindent
  (with-handlers ([exn:fail? fail-thunk])
    (dynamic-require "lexindent.rkt" 'lexindent fail-thunk)))

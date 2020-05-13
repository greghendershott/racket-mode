#lang info

;; NOTE: Racket Mode is **NOT** delivered as a Racket package --
;; instead both the Emacs Lisp front and the Racket back end are
;; delivered as an Emacs MELPA package.
;;
;; Therefore this info.rkt is **NOT** actually used now to build
;; anything -- instead it is simply for informational purposes, e.g.
;; it documents the packages that a user of the Minimal Racket
;; distribution would need to `raco pkg install` manually.

(define name "Racket Mode")

(define deps '(["base" #:version "6.9"]
               "drracket-tool-lib"
               "macro-debugger-text-lib"
               "rackunit-lib"))

(define compile-omit-paths
  '(;; Even if we were actually making a package from this info.rkt,
    ;; we would want to exclude these example files:
    "racket/example"
    ;; Since we are NOT actually making a package, for avoidance of
    ;; doubt let's exlude the entire racket/ tree:
    "racket/"))

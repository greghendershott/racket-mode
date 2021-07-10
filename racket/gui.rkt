#lang racket/base

;; Note that racket/gui/dynamic is in `base` package --- requiring it
;; does NOT create a dependency on the `gui-lib` package.
(require racket/gui/dynamic)

(provide txt/gui)

;; Load racket/gui/base eagerly, if available, instantiating it in our
;; namespace and under our main custodian (as opposed to those for
;; user programs).
(with-handlers ([exn:fail? void])
  (dynamic-require 'racket/gui/base #f))

;; Like mz/mr from racket/sandbox.
(define-syntax txt/gui
  (syntax-rules ()
    [(_ txtval guisym)
     (if (gui-available?)
         (dynamic-require 'racket/gui/base 'guisym)
         txtval)]))

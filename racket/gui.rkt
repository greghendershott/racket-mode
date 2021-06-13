#lang at-exp racket/base

(require (only-in racket/format ~a)
         "util.rkt")

(provide gui-required?
         require-gui
         txt/gui)

(define done? #f)

(define (gui-required?)
  done?)

(define (require-gui in-repl?)
  (when (gui-required?)
    (error 'require-gui "Already required"))
  (display-commented "On-demand, one-time instantiation of racket/gui/base.")
  (when in-repl?
    (display-commented more-explanation))
  (set! done? #t)
  (dynamic-require 'racket/gui/base #f))

;; Like mz/mr from racket/sandbox.
(define-syntax txt/gui
  (syntax-rules ()
    [(_ txtval guisym)
     (if (gui-required?)
         (gui-dyn-req 'guisym)
         txtval)]))

(define (gui-dyn-req sym)
  (dynamic-require 'racket/gui/base sym))

;; Extra explanation for situations like issue 93, entering `(require
;; redex)` in the REPL, as opposed to having it in a .rkt file.
(define more-explanation
  @~a{The namespace was reset. Any `require`s you entered in the REPL were "undone".
      This includes the `require` you just entered. You may want to enter it again.})

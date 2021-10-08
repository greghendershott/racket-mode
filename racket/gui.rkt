#lang racket/base

;; Note that racket/gui/dynamic is in `base` package --- requiring it
;; does NOT create a dependency on the `gui-lib` package.
(require racket/gui/dynamic)

(provide load-racket/gui/base!
         txt/gui
         make-initial-repl-namespace)

(define (load-racket/gui/base!)
  ;; Load racket/gui/base eagerly, if available, instantiating it in our
  ;; namespace and under our main custodian (as opposed to those for
  ;; user programs). This is our strategy to avoid "racket/gui/base
  ;; cannot be instantiated more than once per process". The only reason
  ;; it won't be loaded here now is if we're on a minimal Racket
  ;; installation where gui-lib is not installed.
  (with-handlers ([exn:fail? void])
    (dynamic-require 'racket/gui/base #f))

  ;; #301: On Windows, show then hide an initial frame.
  (when (and (gui-available?)
             (eq? (system-type) 'windows))
    (define make-object (dynamic-require 'racket/class 'make-object))
    (define frame% (dynamic-require 'racket/gui/base 'frame%))
    (define f (make-object frame% "Emacs Racket Mode initialization" #f 100 100))
    (define dynamic-send (dynamic-require 'racket/class 'dynamic-send))
    (dynamic-send f 'show #t)
    (dynamic-send f 'show #f)))

(define-namespace-anchor anchor)
(define (make-initial-repl-namespace)
  (define new-ns (make-base-namespace))
  (define our-ns (namespace-anchor->empty-namespace anchor))

  ;; If load-racket/gui/base! was called, and it was able to load
  ;; racket/gui/base, then it is important for REPL namespaces
  ;; initially to have racket/gui/base _attached_, regardless of
  ;; whether a given user program `require`s it. They might require it
  ;; in the REPL. See also issue #555.
  (when (gui-available?)
    (namespace-attach-module our-ns 'racket/gui/base new-ns))

  ;; Avoid potential problem (IIUC because Racket structs are
  ;; generative) with file/convertible by attaching the same instance
  ;; to user namespaces.
  ;;
  ;; Always do this. Things like pict-lib work without gui-lib, and we
  ;; can still do our feature where we "print images in the REPL". To
  ;; see how we do this using file/convertible, see print.rkt and
  ;; image.rkt.
  (namespace-attach-module our-ns 'file/convertible new-ns)

  new-ns)

;; Like mz/mr from racket/sandbox.
(define-syntax txt/gui
  (syntax-rules ()
    [(_ txtval guisym)
     (if (gui-available?)
         (dynamic-require 'racket/gui/base 'guisym)
         txtval)]))

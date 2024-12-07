;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

;; Note that racket/gui/dynamic is in `base` package --- requiring it
;; does NOT create a dependency on the `gui-lib` package.
(require racket/gui/dynamic
         racket/port
         racket/system)

(provide txt/gui
         make-initial-repl-namespace)

;; Attempt to load racket/gui/base eagerly, instantiating it in our
;; namespace and under our main custodian (as opposed to those used
;; for user programs). This is our strategy to avoid "racket/gui/base
;; cannot be instantiated more than once per process".
;;
;; The only scenarios where racket/gui/base won't be loaded eagerly
;; here:
;;
;; - It's not available: we're on a minimal Racket installation
;;   where gui-lib is not installed.
;;
;; - It can't initialize: e.g. gui-lib is installed but errors with
;;   'Gtk initialization failed for display ":0"', because we're on a
;;   headless system and our racket process wasn't run using xvfb-run.
;;   Because this leaves gui-lib in a "semi-initialized" state where
;;   `gui-available?` returns true but things don't actually work, we
;;   really want to avoid this, so we check by using another racket
;;   process.
(when (parameterize ([current-error-port (open-output-nowhere)])
        (system* (find-executable-path (find-system-path 'exec-file))
                 "-e" "(require racket/gui/base)"))
  (with-handlers ([exn:fail? void])
    (dynamic-require 'racket/gui/base #f)))

;; #301: On Windows, show then hide an initial frame.
(when (and (gui-available?)
           (eq? (system-type) 'windows))
  (define make-object (dynamic-require 'racket/class 'make-object))
  (define frame% (dynamic-require 'racket/gui/base 'frame%))
  (define f (make-object frame% "Emacs Racket Mode initialization" #f 100 100))
  (define dynamic-send (dynamic-require 'racket/class 'dynamic-send))
  (dynamic-send f 'show #t)
  (dynamic-send f 'show #f))

(define-namespace-anchor anchor)
(define our-ns (namespace-anchor->empty-namespace anchor))
(define (make-initial-repl-namespace)
  (define new-ns (make-base-namespace))

  ;; If we loaded racket/gui/base above, then it is important for REPL
  ;; namespaces initially to have racket/gui/base _attached_,
  ;; regardless of whether a given user program `require`s it; a user
  ;; could `require` it at a REPL prompt. See also issue #555.
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

  ;; Likewise for number-markup struct used in elisp.rkt to accomodate
  ;; htdp/bsl; issue #732.
  (with-handlers ([exn:fail? void])
    (namespace-attach-module our-ns 'simple-tree-text-markup/data new-ns))

  new-ns)

;; Like mz/mr from racket/sandbox.
(define-syntax txt/gui
  (syntax-rules ()
    [(_ txtval guisym)
     (if (gui-available?)
         (dynamic-require 'racket/gui/base 'guisym)
         txtval)]))

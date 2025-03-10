;; Copyright (c) 2013-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later.

#lang racket/base

;; This module acts as a "shim" or "launcher" for command-server.rkt.
;;
;; We dynamic-require command-server.rkt within an exn handler for
;; missing modules, to provide a better error UX when people are using
;; Minimal Racket; see issue #744. Any such error is written to stdout
;; as a "notification" for the Emacs front end, which can display it
;; in a dedicated buffer. Not only is this better than error text
;; flashing by in the echo bar and hiding in the *Messages* buffer,
;; our dedicated can supply a browse-url button to our docs section
;; about Minimal Racket.
;;
;; Note that the exn handler is active only during the dynamic extent
;; of the dynamic-require to extract the command-server-loop function.
;; Subsequently we call that function without any such handler in
;; effect.
;;
;; Use the same notification mechanism for other back end startup
;; failures, such as when they need a newer version of Racket.

;; Limit imports to those supplied by Minimal Racket!
(require racket/match
         (only-in racket/port open-output-nowhere)
         racket/runtime-path
         (only-in racket/string string-trim)
         (only-in racket/system system/exit-code)
         version/utils
         (only-in "image.rkt" set-use-svg?!))

;; Write a "notification" for the Emacs front end and exit.
(define (notify/exit kind data)
  (writeln `(startup-error ,kind ,data))
  (flush-output)
  (exit 13))

(define (assert-racket-version minimum-version)
  (define actual-version (version))
  (unless (version<=? minimum-version actual-version)
    (notify/exit
     'other
     (format "Racket Mode needs Racket ~a or newer but ~a is ~a."
             minimum-version
             (find-executable-path (find-system-path 'exec-file))
             actual-version))
    (flush-output)
    (exit 14)))

(define (macos-sequoia-or-newer?)
  (and (eq? 'macosx (system-type 'os))
       ;; Note: This is conservative; will return false if `sw_vers`
       ;; can't be found or doesn't produce a valid version string.
       (let ([out (open-output-string)])
         (parameterize ([current-output-port out])
           (and (zero? (system/exit-code "sw_vers -productVersion"))
                (let ([ver (string-trim (get-output-string out))])
                  (and (valid-version? ver)
                       (version<=? "15.0" ver))))))))

(module+ main
  (assert-racket-version (if (macos-sequoia-or-newer?)
                             "8.14.0.4" ;issue #722
                             "6.12"))   ;general requirement

  ;; Command-line flags (from Emacs front end invoking us)
  (match (current-command-line-arguments)
    [(vector "--use-svg" )       (set-use-svg?! #t)]
    [(vector "--do-not-use-svg") (set-use-svg?! #f)]
    [v
     (notify/exit
      'other
      (format "Bad command-line arguments:\n~s\n" v))])

  (define-runtime-path command-server.rkt "command-server.rkt")
  (define command-server-loop
    (with-handlers ([exn:fail:syntax:missing-module?
                     (λ (e)
                       (notify/exit
                        'missing-module
                        (format "~a" (exn:fail:syntax:missing-module-path e))))])
      (dynamic-require command-server.rkt 'command-server-loop)))

  ;; Save original current-{input output}-port to give to
  ;; command-server-loop for command I/O ...
  (let ([stdin  (current-input-port)]
        [stdout (current-output-port)])
    ;; ... and set no-ops so rando print can't bork the command I/O.
    (parameterize ([current-input-port  (open-input-bytes #"")]
                   [current-output-port (open-output-nowhere)])
      (command-server-loop stdin stdout))))

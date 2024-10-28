;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later.

#lang racket/base

(require racket/match
         racket/port
         (only-in racket/string string-trim)
         (only-in racket/system system/exit-code)
         version/utils
         "command-server.rkt"
         (only-in "image.rkt" set-use-svg?!))

(define (assert-racket-version minimum-version)
  (define actual-version (version))
  (unless (version<=? minimum-version actual-version)
    (error '|Racket Mode back end| "Need Racket ~a or newer but ~a is ~a"
           minimum-version
           (find-executable-path (find-system-path 'exec-file))
           actual-version)))

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
     (error '|Racket Mode back end|
            "Bad command-line arguments:\n~s\n" v)])

  ;; Save original current-{input output}-port to give to
  ;; command-server-loop for command I/O.
  (let ([stdin  (current-input-port)]
        [stdout (current-output-port)])
    ;; Set no-ops so e.g. rando print can't bork the command I/O.
    (parameterize ([current-input-port  (open-input-bytes #"")]
                   [current-output-port (open-output-nowhere)])
      (command-server-loop stdin stdout))))

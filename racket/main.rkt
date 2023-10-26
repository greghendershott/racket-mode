;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later.

#lang racket/base

(require racket/match
         racket/port
         version/utils
         "command-server.rkt"
         (only-in "image.rkt" set-use-svg?!))

(module+ main
  ;; Assert Racket minimum version
  (define minimum-version "6.12")
  (define actual-version (version))
  (unless (version<=? minimum-version actual-version)
    (error '|Racket Mode back end| "Need Racket ~a or newer but ~a is ~a"
           minimum-version
           (find-executable-path (find-system-path 'exec-file))
           actual-version))

  ;; Command-line flags (from Emacs front end invoking us)
  (match (current-command-line-arguments)
    [(vector "--use-svg" )       (set-use-svg?! #t)]
    [(vector "--do-not-use-svg") (set-use-svg?! #f)]
    [v
     (error '|Racket Mode back end|
            "Bad command-line arguments:\n~v\n" v)])

  ;; Save original current-{input output}-port to give to
  ;; command-server-loop for command I/O.
  (let ([stdin  (current-input-port)]
        [stdout (current-output-port)])
    ;; Set no-ops so e.g. rando print can't bork the command I/O.
    (parameterize ([current-input-port  (open-input-bytes #"")]
                   [current-output-port (open-output-nowhere)])
      (command-server-loop stdin stdout))))

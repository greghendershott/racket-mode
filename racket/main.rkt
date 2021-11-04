#lang racket/base

(require racket/match
         racket/port
         version/utils
         "command-server.rkt"
         (only-in "image.rkt" set-use-svg?!)
         "repl.rkt")

(module+ main
  ;; Assert Racket minimum version
  (define minimum-version "6.9")
  (define actual-version (version))
  (unless (version<=? minimum-version actual-version)
    (error '|Racket Mode back end| "Need Racket ~a or newer but ~a is ~a"
           minimum-version
           (find-executable-path (find-system-path 'exec-file))
           actual-version))

  ;; Command-line flags (from Emacs front end invoking us)
  (define-values (launch-token accept-host tcp-port)
    (match (current-command-line-arguments)
      [(vector "--auth"        auth
               "--accept-host" accept-host
               "--port"        port
               (or (and "--use-svg"        (app (λ _ (set-use-svg?! #t)) _))
                   (and "--do-not-use-svg" (app (λ _ (set-use-svg?! #f)) _))))
       (values auth accept-host (string->number port))]
      [v
       (error '|Racket Mode back end|
              "Bad command-line arguments:\n~v\n" v)]))

  ;; Save original current-{input output}-port to give to
  ;; command-server-loop for command I/O.
  (let ([stdin  (current-input-port)]
        [stdout (current-output-port)])
    ;; Set no-ops so e.g. rando print can't bork the command I/O.
    (parameterize ([current-input-port  (open-input-bytes #"")]
                   [current-output-port (open-output-nowhere)])
      (start-repl-session-server launch-token accept-host tcp-port)
      (command-server-loop stdin stdout))))

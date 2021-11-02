#lang racket/base

(require racket/match
         racket/port
         version/utils
         "command-server.rkt"
         (only-in "image.rkt" emacs-can-use-svg!)
         "repl.rkt")

(module+ main
  (define minimum-version "6.9")
  (define actual-version (version))
  (unless (version<=? minimum-version actual-version)
    (error '|Racket Mode back end| "Need Racket ~a or newer but ~a is ~a"
           minimum-version
           (find-executable-path (find-system-path 'exec-file))
           actual-version))

  (define-values (launch-token accept-host tcp-port)
    (match (current-command-line-arguments)
      [(vector (== "--auth")        token
               (== "--accept-host") accept-host
               (== "--port")        tcp-port
               (and (or "--use-svg" "--do-not-use-svg")
                    svg-flag-str))
       (emacs-can-use-svg! svg-flag-str)
       (values token accept-host (string->number tcp-port))]
      [v
       (eprintf "Bad command-line arguments:\n~v\n" v)
       (exit)]))

  ;; Save original current-{input output}-port to give to
  ;; command-server-loop for command I/O.
  (let ([stdin  (current-input-port)]
        [stdout (current-output-port)])
    ;; Set no-ops so e.g. rando print can't bork the command I/O.
    (parameterize ([current-input-port  (open-input-bytes #"")]
                   [current-output-port (open-output-nowhere)])
      (start-repl-session-server launch-token accept-host tcp-port)
      (command-server-loop stdin stdout))))

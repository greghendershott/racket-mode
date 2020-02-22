#lang racket/base

(require racket/match
         racket/port
         "logger.rkt"
         "command-server.rkt"
         "repl.rkt")

(module+ main
  (define-values (command-port launch-token)
    (match (current-command-line-arguments)
      [(vector port token)
       (values (string->number port) token)]
      [v
       (eprintf "Bad command-line arguments: ~v\n" v)
       (exit)]))

  ;; Save original current-{input output}-port i.e. stdin/stdout, to
  ;; give to command-server-loop.
  (define in  (current-input-port))
  (define out (current-output-port))

  ;; Set current-{input output}-port to no-ops -- so e.g. some random
  ;; println won't bork the the command protocol.
  ;;
  ;; Do so BEFORE calling any start-xxx functions, below -- so any
  ;; threads those might create will inherit these no-op values.
  (current-output-port (open-output-nowhere))
  (current-input-port  (open-input-bytes #""))

  (start-repl-session-server command-port launch-token)

  (start-logger-server (add1 command-port) launch-token)

  (command-server-loop in out))

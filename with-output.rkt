#lang racket/base

(require syntax/parse/define
         "util.rkt")

(provide current-command-output-file
         with-output-to-command-output-file
         current-debug-break-output-file
         with-output-to-debug-break-output-file
         current-debug-signal-break-file)

;; Commands intended for use programmatically by racket-mode may
;; output their results to a file whose name is the value of the
;; current-command-output-file parameter. This avoids mixing with
;; stdout and stderr, and hoping that racket-mode can regexp it back
;; out again -- which ultimately is not very reliable.
;;
;; How does racket-mode know when the file is ready to be read?
;;
;; 1. racket-mode deletes the file, give us a command, and waits for
;;    the file to exist.
;;
;; 2. We direct the command's output to a temporary file (on the same
;;    filesystem), then when the command has finished, rename the temp
;;    file to current-command-output-file. This way, racket-mode knows
;;    that as soon as the file exists again, the command is finished
;;    and its output is ready to be read from the file.
;;
;; Use the same mechanism for debugger break notifications.

(define (call-with-output-to-param-file param f)
  (cond [(param)
         (define tmp-file (path-add-suffix (param) ".tmp"))
         (with-output-to-file tmp-file #:exists 'replace f)
         (rename-file-or-directory tmp-file (param) #t)]
        [else (f)]))

(define current-command-output-file (make-parameter-ish #f))

(define-simple-macro (with-output-to-command-output-file e:expr ...+)
  (call-with-output-to-param-file current-command-output-file
                                  (λ () e ...)))

(define current-debug-break-output-file (make-parameter-ish #f))

(define-simple-macro (with-output-to-debug-break-output-file e:expr ...+)
  (call-with-output-to-param-file current-debug-break-output-file
                                  (λ () e ...)))

;; Not actually an output file, more like an input file:
(define current-debug-signal-break-file (make-parameter-ish #f))

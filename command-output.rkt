#lang racket/base

(provide current-command-output-file
         with-output-to-command-output-file)

;; Commands intended for use programmatically by racket-mode may
;; output their results to a file whose name is the value of the
;; current-command-output-file parameter. This avoids mixing with
;; stdout and stderr, which ultimately is not very reliable. How does
;; racket-mode know when the file is ready to be read? 1. racket-mode
;; deletes the file, calls us, and waits for the file to exist. 2. We
;; direct the command's output to a temporary file (on the same fs),
;; then when the command has finished, rename the temp file to
;; current-command-output-file. This way, racket-mode knows that as
;; soon as the file exists again, the command is finished and its
;; output is ready to be read from the file.
(define current-command-output-file (make-parameter #f))
(define (with-output-to-command-output-file f)
  (cond [(current-command-output-file)
         (define tmp-file (path-add-suffix (current-command-output-file) ".tmp"))
         (with-output-to-file tmp-file #:exists 'replace f)
         (rename-file-or-directory tmp-file (current-command-output-file) #t)]
        [else (f)]))

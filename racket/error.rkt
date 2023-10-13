;; Copyright (c) 2013-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/format
         racket/match
         "instrument.rkt"
         "repl-output.rkt"
         "stack-checkpoint.rkt")

(provide racket-mode-error-display-handler)

(module+ test
  (require rackunit))

(define (racket-mode-error-display-handler msg v)
  (cond
    [(exn? v)
     (let ([msg (if (member (exn-message v) (list msg ""))
                    msg
                    (string-append msg "\n" (exn-message v)))])
      (repl-output-error
       (list msg
             (list 'srclocs
                   (if (exn:srclocs? v)
                       (map srcloc->elisp-value
                            ((exn:srclocs-accessor v) v))
                       null))
             (list 'context
                   (if (or (exn:fail:syntax? v)
                           (and (exn:fail:read? v) (not (exn:fail:read:eof? v)))
                           (exn:fail:user? v))
                       null
                       (context v))))))]
    [else
     (displayln msg (current-error-port))
     (flush-output (current-error-port))]))

(define (srcloc->elisp-value x)
  (define src
    ;; Although I want to find/fix this properly upstream -- is
    ;; something a path-string? when it should be a path? -- for now
    ;; just catch here the case where the source is a string like
    ;; "\"/path/to/file.rkt\"" i.e. the string value has quotes.
    (match (srcloc-source x)
      [(pregexp "^\"(.+)\"$" (list _ unquoted)) unquoted]
      [(? path? v) (path->string v)]
      [v v]))
  (and (path-string? src)
       (srcloc-line x)
       (srcloc-column x)
       (srcloc-position x)
       (srcloc-span x)
       (list src (srcloc-line x) (srcloc-column x) (srcloc-position x) (srcloc-span x))))

(define (context e)
  (define-values (kind pairs)
    (cond [(instrumenting-enabled)
           (values 'errortrace
                 (get-error-trace e))]
          [else
           (values 'plain
                   (for/list ([_ (error-print-context-length)]
                              [v (in-list
                                  (continuation-mark-set->trimmed-context
                                   (exn-continuation-marks e)))])
                     v))]))
  (cons kind
        (for/list ([v (in-list pairs)])
          (match-define (cons label src) v)
          (cons (and label (~a label))
                (and src (srcloc->elisp-value src))))))

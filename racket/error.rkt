;; Copyright (c) 2013-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/format
         racket/match
         "instrument.rkt"
         "repl-output.rkt"
         "stack-checkpoint.rkt")

(provide racket-mode-error-display-handler)

(define default-error-display-handler (error-display-handler))

;; On the one hand, the docs say: "An error display handler can print
;; errors in different ways, but it should always print to the current
;; error port." After all, a user program might use
;; error-display-handler, as in #672.
;;
;; On the other hand, we really want to give our front end REPL
;; /structured/ error data via our special channel, not text.
;;
;; I think the solution is to check whether current-error-port is the
;; special one we use for structured REPL output, a.k.a. the original
;; value for the user program.

;; - If so it's fine to bend the rules and use our special output
;;   channel to the front end. Probably we're the one using the
;;   handler. Even if the user program is, the meaning is "use it
;;   for-effect to output to the original error port", which in this
;;   case means ultimately to the Racket Mode front end REPL. It's OK
;;   and in fact desirable to get the same structured error handling.
;;
;; - Otherwise, we're running while the user program has parameterized
;;   current-error-port, perhaps to an output-string to use for-value,
;;   or to some other port to use for-effect. In that case we defer
;;   /completely/ to the default error-display-handler. Not only does
;;   that output to current-error-port, the overall format will be the
;;   same as when the user program is run with command-line racket.
;;   (Of course some context items may differ on the "outside" edge,
;;   showing wx/queue.rkt, racket-mode's repl.rkt, etc. But the
;;   "inner" items and the overall format will be the same.)
(define (racket-mode-error-display-handler msg v)
  (cond
    [(repl-error-port? (current-error-port))
     (cond
       [(exn? v)
        (let ([msg (if (member (exn-message v) (list msg ""))
                       msg
                       (string-append msg "\n" (exn-message v)))])
          (repl-output-error (list msg (srclocs v) (context v))))]
       [else
        (displayln msg (current-error-port))
        (flush-output (current-error-port))])]
    [else
     (default-error-display-handler msg v)]))

(define (srclocs e)
  (cond [(exn:srclocs? e)
         (for*/list ([sl (in-list ((exn:srclocs-accessor e) e))]
                     [elv (in-value (srcloc->elisp-value sl))]
                     #:when elv)
           elv)]
        [else null]))

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

(define (srcloc->elisp-value loc)
  (define src
    ;; Although I want to find/fix this properly upstream -- is
    ;; something a path-string? when it should be a path? -- for now
    ;; just catch here the case where the source is a string like
    ;; "\"/path/to/file.rkt\"" i.e. the string value has quotes.
    (match (srcloc-source loc)
      [(pregexp "^\"(.+)\"$" (list _ unquoted)) unquoted]
      [(? path? v) (path->string v)]
      [v v]))
  (define str (or (srcloc->string loc)
                  (format "~a:~a:~a" src (srcloc-line loc) (srcloc-column loc))))
  (and (path-string? src)
       (srcloc-line loc)
       (srcloc-column loc)
       (srcloc-position loc)
       (srcloc-span loc)
       (list str src (srcloc-line loc) (srcloc-column loc) (srcloc-position loc) (srcloc-span loc))))

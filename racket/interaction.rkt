;; Copyright (c) 2013-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require "repl-output.rkt"
         "repl-session.rkt"
         "stack-checkpoint.rkt")

(provide get-interaction)

(define (get-interaction prompt)
  (repl-output-prompt (string-append prompt ">"))
  (let loop ()
    (sync
     (wrap-evt ((current-get-interaction-evt)) ;allow GUI yield
               (λ (thk) (thk) (loop)))
     (wrap-evt (current-submissions)
               (λ (str)
                 (define in (open-input-string str))
                 (with-stack-checkpoint
                   ((current-read-interaction) 'racket-mode-repl in)))))))

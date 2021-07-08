#lang racket/base

(require "index.rkt" "elisp.rkt")

(for
    ([e (in-list (index))])
  (elisp-writeln
   e
   (current-output-port)))



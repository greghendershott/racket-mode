#lang at-exp racket/base

(require racket/format
         "util.rkt")

(provide welcome)

(define (welcome specific-file?)
  (display (banner))
  (unless specific-file?
    (display-commented
     @~a{Racket Mode's REPL is not running any specific file. The namespace
         is just #lang racket/base. If you were expecting more things to be
         defined, try entering (require racket).

         Also, you can:
           1. Visit a .rkt file in a racket-mode buffer.
           2. Use the racket-run command.})))

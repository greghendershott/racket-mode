#lang at-exp racket/base

(require racket/format
         "util.rkt")

(provide welcome)

(define (welcome specific-file?)
  (display (banner))
  (unless specific-file?
    (display-commented
     @~a{You started Racket Mode's REPL for no specific file. The namespace
         is just #lang racket/base. If you were expecting more things to be
         defined, try entering (require racket).

         Instead of using the racket-repl command, consider using
         Racket Mode's REPL as intended:
           1. Visit a .rkt file.
           2. Use the racket-run command.})))

#lang racket/base

(require syntax/modcode
         syntax/modresolve
         racket/match)

(provide imports-gui?)

;; Although I imagined it would be sufficient to check for imports of
;; racket/gui/base, a counter-example I discovered is the plot
;; package. It requires racket/draw, but module-compiled-imports
;; doesn't reveal that it transitively requires racket/gui/base.
(define gui-paths (for/list ([x '(racket/gui/base racket/draw)])
                    (resolve-module-path x #f)))

(define imports-gui (make-hash))

;; (or/c path-string? path? module-path?) -> boolean?
;; Works like syntax/moddeps 
(define (imports-gui? v)
  (define path
    (cond [(path? v) v]
          [(module-path? v) (resolve-module-path v #f)]
          [(path-string? v) (string->path v)]
          [else (raise-argument-error 'imports-gui
                                      "(or/c path-string? path? module-path?"
                                      v)]))
  (and (path? path)
       (hash-ref imports-gui path
                 (lambda ()
                   (define code (get-module-code path))
                   (define imports (module-compiled-imports code))
                   (define result
                     (for/or ([import imports])
                       (match import
                         [(list phase mpis ...)
                          (for/or ([mpi mpis])
                            (define p (resolve-module-path-index mpi path))
                            (or (member? p gui-paths)
                                (and (not (symbol? p))
                                     (imports-gui? p))))]
                         [_ #f])))
                   (hash-set! imports-gui path result)
                   result))))

(module+ test
  (require rackunit)
  (check-false (imports-gui? "/tmp/foo.rkt"))
  (check-true (imports-gui? "/tmp/gui.rkt"))
  (check-true (imports-gui? "/tmp/plot.rkt")))

(define (member? x xs)
  (not (not (member x xs))))

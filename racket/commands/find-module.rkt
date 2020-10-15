#lang racket/base

(require racket/contract
         racket/match
         syntax/modresolve
         "../mod.rkt")

(provide find-module)

(define/contract (find-module str maybe-mod)
  (-> string? (or/c #f mod?)
      (or/c #f (list/c path-string? number? number?)))
  (define-values (dir _file maybe-rmp) (maybe-mod->dir/file/rmp maybe-mod))
  (parameterize ([current-load-relative-directory dir])
    (or (mod-loc str maybe-rmp)
        (mod-loc (string->symbol str) maybe-rmp))))

(define (mod-loc v maybe-rmp)
  (match (with-handlers ([exn:fail? (λ _ #f)])
           (resolve-module-path v maybe-rmp))
    [(? path-string? path)
     #:when (file-exists? path)
     (list (path->string path) 1 0)]
    [_ #f]))

(module+ test
  (require rackunit
           racket/runtime-path)
  (define-runtime-path here ".")
  (let* ([here             (simplify-path here)] ;nuke trailing dot
         ;; Examples of finding relative and absolute:
         [requires.rkt     (path->string (build-path here "requires.rkt"))]
         [pe-racket/string (pregexp "collects/racket/string.rkt$")])
    ;; Examples of having no current module (i.e. plain racket/base
    ;; REPL) and having one ("describe.rkt").
    (let ([mod #f])
     (parameterize ([current-directory here])
       (check-match (find-module "requires.rkt" mod)
                    (list (== requires.rkt) 1 0))
       (check-match (find-module "racket/string" mod)
                    (list pe-racket/string 1 0))))
    (let ([mod (->mod/existing (build-path here "describe.rkt"))])
      (check-match (find-module "requires.rkt" mod)
                   (list (== requires.rkt) 1 0))
      (check-match (find-module "racket/string" mod)
                   (list pe-racket/string 1 0)))))

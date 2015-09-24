#lang at-exp racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/contract/region
         racket/format
         racket/match
         racket/string
         syntax/location
         "util.rkt")

(provide relative-module-path?
         (struct-out mod)
         ->mod/existing
         maybe-mod->dir/file/rmp
         maybe-mod->prompt-string
         maybe-warn-about-submodules)

(module+ test
  (require rackunit))

;; The subset of module-path? with a relative filename
(define (relative-module-path? v)
  (define (rel-path? v) ;real predicate taking any/c, unlike relative-path?
    (and (path-string? v) (relative-path? v)))
  (and (module-path? v)
       (match v
         [(? rel-path?) #t]
         [(list 'submod (? rel-path?) (? symbol?) ..1) #t]
         [_ #f])))

(module+ test
  (check-true (relative-module-path? "f.rkt"))
  (check-true (relative-module-path? '(submod "f.rkt" a b)))
  (check-false (relative-module-path? "/path/to/f.rkt"))
  (check-false (relative-module-path? '(submod "/path/to/f.rkt" a b)))
  (check-false (relative-module-path? 'racket/base))
  (check-false (relative-module-path? '(submod 'racket/base a b))))

(define-struct/contract mod
  ([dir  absolute-path?]         ;#<path:/path/to/>
   [file relative-path?]         ;#<path:foo.rkt>
   [rmp  relative-module-path?]) ;#<path:f.rkt> or '(submod <path:f.rkt> bar)
  #:transparent)

(define/contract (->mod/simple v)
  (-> any/c (or/c #f mod?))
  (match v
    [(? symbol? s) (->mod/simple (~a s))] ;treat 'file.rkt as "file.rkt"
    [(or (? path? ap) (? path-string? ap))
     (let*-values ([(dir file _) (split-path (simplify-path ap))]
                   [(dir) (match dir ['relative (current-directory)][dir dir])])
       (mod dir file file))]
    [_ #f]))

(define/contract (->mod v)
  (-> any/c (or/c #f mod?))
  (define-match-expander mm
    (syntax-parser
      [(_ dir:id file:id rmp:id)
       #'(app ->mod/simple (mod dir file rmp))]))
  (match v
    [(list 'submod
           (mm d f _) (? symbol? ss) ..1) (mod d f (list* 'submod f ss))]
    [(list (mm d f _) (? symbol? ss) ..1) (mod d f (list* 'submod f ss))]
    [(list (mm d f mp))                   (mod d f mp)]
    [(mm d f mp)                          (mod d f mp)]
    [_                                    #f]))

(module+ test
  (define-syntax-rule (= x y) (check-equal? x y))
  (define f.rkt (string->path "f.rkt"))
  ;; rel path
  (let ([dir (current-directory)])
    (= (->mod "f.rkt") (mod dir f.rkt f.rkt))
    (= (->mod 'f.rkt)  (mod dir f.rkt f.rkt))
    (= (->mod '(submod "f.rkt" a b)) (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '(submod f.rkt a b))   (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '("f.rkt" a b)) (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '(f.rkt a b))   (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '("f.rkt")) (mod dir f.rkt f.rkt))
    (= (->mod '(f.rkt))   (mod dir f.rkt f.rkt)))
  ;; abs path
  (let ([dir (string->path "/p/t/")])
    (= (->mod "/p/t/f.rkt") (mod dir f.rkt f.rkt))
    (= (->mod '/p/t/f.rkt)  (mod dir f.rkt f.rkt))
    (= (->mod '(submod "/p/t/f.rkt" a b)) (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '(submod /p/t/f.rkt a b))   (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '("/p/t/f.rkt" a b)) (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '(/p/t/f.rkt a b))   (mod dir f.rkt `(submod ,f.rkt a b)))
    (= (->mod '("/p/t/f.rkt")) (mod dir f.rkt f.rkt))
    (= (->mod '(/p/t/f.rkt))   (mod dir f.rkt f.rkt)))
  ;; nonsense input => #f
  (= (->mod 42)                #f)
  (= (->mod '(42 'bar))        #f)
  (= (->mod '(submod 42 'bar)) #f)
  (= (->mod '(submod (submod "f.rkt" foo) bar)) #f))

(define/contract (->mod/existing v)
  (-> any/c (or/c #f mod?))
  (match (->mod v)
    [(and v (mod dir file mp))
     (define path (build-path dir file))
     (cond [(file-exists? path) v]
           [else (display-commented (format "~a does not exist" path))
                 #f])]
    [_ #f]))

(define/contract (maybe-mod->dir/file/rmp maybe-mod)
  (-> (or/c #f mod?) (values absolute-path?
                             (or/c #f relative-path?)
                             (or/c #f relative-module-path?)))
  (match maybe-mod
    [(mod d f mp) (values d f mp)]
    [#f           (values (current-directory) #f #f)]))

(define/contract (maybe-mod->prompt-string m)
  (-> (or/c #f mod?) string?)
  (match m
    [(mod _ _ (? path? file))     (~a file)]
    [(mod _ _ (list* 'submod xs)) (string-join (map ~a xs) ":")]
    [#f                           ""]))

;; Check whether Racket is new enough (newer than 6.2.1) that
;; module->namespace works with module+ and (module* _ #f __)
;; forms when errortrace is enabled.
(module+ check
  (define x 42))
(define (can-enter-module+-namespace?)
  (define mp (quote-module-path check))
  (dynamic-require mp #f)
  (with-handlers ([exn:fail? (λ _ #f)])
    (eval 'x (module->namespace mp))
    #t))

(define warned? #f)
(define/contract (maybe-warn-about-submodules mp context)
  (-> (or/c #f module-path?) symbol? any)
  (unless (or warned?
              (not (pair? mp)) ;not submodule
              (memq context '(low medium))
              (can-enter-module+-namespace?))
    (set! warned? #t)
    (display-commented
     @~a{Note: @~v[@mp] will be evaluated.
               However your Racket version is old. You will be unable to
               use the REPL to examine definitions in the body of a module+
               or (module* _ #f ___) form when errortrace is enabled. Either
               upgrade Racket, or, set the Emacs variable racket-error-context
               to 'low or 'medium.})))

#lang racket/base

(require racket/contract
         racket/match
         racket/set
         racket/string
         (only-in "../util.rkt"
                  path-has-extension?
                  path-replace-extension))

(provide module-names)

(struct collection
  (maybe-prefix ;(or/c #f string?) when a rktd link entry starts with a string
   path))       ;path?

(define (module-names)
  (define results (mutable-set))
  (for ([coll (in-set (collections))])
    (define top (collection-path coll))
    (when (safe-directory-exists? top)
      (parameterize ([current-directory top])
        (for ([raw-p (in-directory #f use?)])
          (define p (maybe-prefix-path-for-collection coll raw-p))
          (define-values (base name dir?) (split-path p))
          (define name-str (path->string name))
          (when (and (use? p)
                     (or dir?
                         (path-has-extension? p #"rkt")
                         (path-has-extension? p #"ss")))
            (define v (path->string
                       ;; path/to/main.rkt => path/to
                       (match (explode-path p)
                         [(list xs ..1 (== (build-path "main.rkt")))
                          (apply build-path xs)]
                         [_ (path-replace-extension p #"")])))
            (set-add! results v))))))
  (sort (set->list results)
        string<?))

(define (use? p)
  (define-values (base name dir?) (split-path p))
  (define name-str (path->string name))
  (and (not (string-prefix? name-str "."))
       (not (member name-str '("compiled"
                               "info.rkt"
                               "private"
                               "scribblings"
                               "tests")))))

(define (collections)
  (define results (mutable-set))
  (for ([link-file (in-list (current-library-collection-links))])
    (cond [link-file
           (when (file-exists? link-file)
             (define-values (base name dir?) (split-path link-file))
             (match (with-handlers ([exn:fail? (λ (x) '())])
                      (call-with-input-file link-file read))
               [(? list? vs)
                (for ([v (in-list vs)])
                  (when (if (and (list? v) (= 3 (length v)))
                            (and (regexp? (list-ref v 2))
                                 (regexp-match (list-ref v 2) (version)))
                            #t)
                    (define prefix (if (string? (list-ref v 0))
                                       (list-ref v 0)
                                       #f))
                    (define path (simplify-path
                                  (if (relative-path? (list-ref v 1))
                                      (build-path base (list-ref v 1))
                                      (list-ref v 1))))
                    (set-add! results
                              (collection prefix
                                          path))))]
               [_ (void)]))]
          [else
           (for ([p (in-list (current-library-collection-paths))])
             (set-add! results (collection #f
                                           (simplify-path p))))]))
  results)

(define (maybe-prefix-path-for-collection coll path)
  (if (collection-maybe-prefix coll)
      (build-path (collection-maybe-prefix coll) path)
      path))

(define/contract (safe-directory-exists? d)
  (-> path-string? boolean?)
  (with-handlers ([exn:fail? (λ (x) #f)])
    (directory-exists? d)))

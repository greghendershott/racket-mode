;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/match
         racket/path
         racket/set
         racket/string)

(provide module-names)

(struct collection
  (maybe-prefix ;(or/c #f string?) when a rktd link entry starts with a string
   path)        ;path?
  #:transparent)

(define (module-names)
  (define results (mutable-set))
  (define main.rkt (string->path "main.rkt"))
  (for ([coll (in-set (collections))])
    (define top (collection-path coll))
    (when (safe-directory-exists? top)
      (parameterize ([current-directory top])
        (for ([raw-p (in-directory #f use?)])
          (define p (maybe-prefix-path-for-collection coll raw-p))
          (define-values (_base _name dir?) (split-path p))
          (when (and (use? p)
                     (or dir?
                         (member (path-get-extension p) '(#".rkt" #".ss"))))
            (match-define (cons last-part first-parts) (reverse (explode-path p)))
            (define path-parts
              (reverse
               (cond [;; path/to/main.rkt => path/to
                      (equal? last-part main.rkt) first-parts]
                     [;; path/to/file.rkt => path/to/file
                      else (cons (path-replace-extension last-part #"")
                            first-parts)])))
            ;; Use string-join with "/" instead of build-path so that
            ;; Windows paths become Racket module paths.
            (set-add! results (string-join (map path->string path-parts)
                                           "/")))))))
  (sort (set->list results)
        string<?))

;; This is not a test submodule because, although there are a half
;; dozen false positives, they are things like
;; "web-server/default-web-root/configuration-table", for which our
;; module-names function would need to start reading info.rkt for
;; {compile test}-omit-paths -- and I just don't think it's worth the
;; effort just to exclude a half dozen bogus completion candidates
;; among thousands of correct ones.
(module+ find-false-positives
  (require rackunit)
  (for ([m (in-list (module-names))])
    (check-not-exn (λ () (dynamic-require (string->symbol m) (void)))
                   m)))

(define (use? p)
  (define-values (_base name dir?) (split-path p))
  (define name-str (path->string name))
  (and (not (string-prefix? name-str "."))
       (not (member name-str '("compiled"
                               "doc"
                               "info.rkt"
                               "private"
                               "scribblings"
                               "tests"))))  )

(define (collections)
  (define results (mutable-set))
  (for ([link-file (in-list (current-library-collection-links))])
    (cond [link-file
           (when (file-exists? link-file)
             (define-values (base _name _dir?) (split-path link-file))
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
                    (define path
                      (match (list-ref v 1)
                        [(? string? str) str]
                        [(? bytes? bstr) (bytes->path bstr)]
                        [(? list? elems) (apply build-path
                                                (for/list ([elem (in-list elems)])
                                                  (if (bytes? elem)
                                                      (bytes->path-element elem)
                                                      elem)))]))
                    (define abs-path (simplify-path
                                      (if (relative-path? path)
                                          (build-path base path)
                                          path)))
                    (set-add! results
                              (collection prefix
                                          abs-path))))]
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

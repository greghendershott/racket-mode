#lang racket/base

(require racket/path
         racket/match
         pkg/db
         pkg/name
         (only-in pkg/private/pkg-db pkg-directory) ;; private!!
         (only-in pkg/private/dirs pkg-installed-dir) ;; private!!
         (only-in pkg/lib
                  installed-pkg-table
                  pkg-info-orig-pkg
                  pkg-info-auto?
                  pkg-info-checksum))

(provide packages-summaries
         package-details
         package-mutate)

(struct installed-pkg (scope orig-pkg auto? checksum) #:transparent)
(define (installed-packages)
  (define ht (make-hash))
  (for ([scope (in-list (list 'installation 'user))])
    (for ([(name pi) (in-hash (installed-pkg-table #:scope scope))])
      (hash-set! ht name (installed-pkg scope
                                        (cleanse-orig-pkg (pkg-info-orig-pkg pi))
                                        (pkg-info-auto? pi)
                                        (or (pkg-info-checksum pi) "")))))
  ht)

(define (cleanse-orig-pkg orig-pkg)
  (case (car orig-pkg)
    [(link static-link clone)
     (list* (car orig-pkg)
            (path->string
             (simple-form-path
              (path->complete-path (cadr orig-pkg)
                                   (pkg-installed-dir))))
            (cddr orig-pkg))]
    [else orig-pkg]))

(define (packages-summaries)
  (define installed (installed-packages))
  (define catalog (for/hash ([p (in-list (get-pkgs))])
                    (values (pkg-name p) p)))
  (append
   ;; All packages from the catalogs
   (for/list ([(name p) (in-hash catalog)])
     (define ip (hash-ref installed name #f))
     (define status (if ip
                        (if (installed-pkg-auto? ip)
                            "dependency"
                            "manual")
                        "available"))
     (define checksum (if ip
                          (installed-pkg-checksum ip)
                          (pkg-checksum p)))
     (list name
           status
           checksum
           (pkg-source p)
           (cleansed-pkg-desc p)))
   ;; Installed packages not from the catalogs, i.e. that we didn't
   ;; just handle above.
   (for/list ([(name pi) (in-hash installed)]
              #:when (not (hash-has-key? catalog name)))
     (list name
           "manual"
           (installed-pkg-checksum pi)
           ""
           ""))))

(define (package-details name)
  (define-values (p catalog-only-props)
    (match (get-pkgs #:name name)
      [(cons p _)
       (values p
               (list ':author (pkg-author p)
                     ':tags (get-pkg-tags name (pkg-catalog p))
                     ':deps (get-pkg-dependencies name (pkg-catalog p) (pkg-checksum p))
                     ':description (cleansed-pkg-desc p)))]
      [(list)
       (values #f null)]))
  (define ip (hash-ref (installed-packages) name #f))
  (cond
    [ip
     (define source (if p
                        (pkg-source p)
                        (installed-pkg-orig-pkg ip)))
     (append (list ':name name
                   ':source (cons source (source-url source))
                   ':status (if (installed-pkg-auto? ip) "dependency" "manual")
                   ':checksum (installed-pkg-checksum ip)
                   ':scope (installed-pkg-scope ip)
                   ':dir (path->string
                          (simple-form-path
                           (pkg-directory name))))
             catalog-only-props)]
    [p
     (append (list ':name name
                   ':source (cons (pkg-source p) (source-url (pkg-source p)))
                   ':status "available"
                   ':checksum (pkg-checksum p))
             catalog-only-props)]
    [else #f]))

(define (cleansed-pkg-desc p)
  (regexp-replace* "[\r\n]" (pkg-desc p) " "))

;; TODO: Using the `pkg` module pkg-{install update remove}-command
;; functions is easy enough -- but do we want to show the progress
;; output in the details buffer like we do with the old design, and if
;; so, how??
(define (package-mutate name op)
  #t)

(require net/url-string)
(define (source-url s)
  (match s
    [(or (list (? symbol?) (? string? s))
         (list (? symbol?) (? string?) (? string? s)))
     (source-url s)]
    [(pregexp "^(file://.+)[?]type=.+$" (list _ s))
     s]
    ;; git flavors: Use https and simplify the path+query to just user
    ;; and repo elements.
    [(pregexp "^github://|git://|git\\+http://|git\\+https://$")
     (define u (string->url s))
     (match-define (list* user repo _) (url-path u))
     (url->string (struct-copy url u
                               [scheme "https"]
                               [path (list user repo)]
                               [query null]))]
    [(pregexp "^/[^/]")
     (string-append "file://" s)]
    [s s]))

(module+ test
  (require rackunit)
  (check-equal? (source-url '(static-link "/path/to/foo"))
                "file:///path/to/foo")
  (check-equal? (source-url '(catalog "git://github.com/user/repo/blah?x=1"))
                "https://github.com/user/repo")
  (check-equal? (source-url "file:///path/to/foo?type=static-link")
                "file:///path/to/foo")
  (check-equal? (source-url "/path/to/foo")
                "file:///path/to/foo")
  (check-equal? (source-url "git://github.com/user/repo/blah?x=1")
                "https://github.com/user/repo"))

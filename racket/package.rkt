#lang racket/base

(require (only-in racket/format ~a)
         racket/match
         racket/path
         (only-in racket/string string-join)
         (except-in pkg/lib
                    pkg-desc)
         (only-in pkg/db
                  get-pkgs
                  pkg-name
                  pkg-catalog
                  pkg-checksum
                  pkg-source
                  pkg-author
                  pkg-desc
                  get-pkg-dependencies
                  get-pkg-tags
                  get-pkg-modules)
         (only-in pkg
                  pkg-install-command
                  pkg-update-command
                  pkg-remove-command))

(provide package-list
         package-details
         package-config
         package-op
         package-notify-channel)

(define (package-list)
  (define installed (installed-packages))
  (define catalog (for/hash ([p (in-list (get-pkgs))])
                    (values (pkg-name p) p)))
  (append
   ;; All packages from the catalogs:
   (for/list ([(name p) (in-hash catalog)])
     (define ip (hash-ref installed name #f))
     (define status (cond
                      [(not ip)                 "available"]
                      [(installed-pkg-auto? ip) "dependency"]
                      [else                     "manual"]))
     (list name
           status
           (cleanse-pkg-desc p)))
   ;; Installed packages not from the catalogs, i.e. that we didn't
   ;; already handle above:
   (for/list ([name (in-hash-keys installed)]
              #:when (not (hash-has-key? catalog name)))
     (list name
           "manual"
           ""))))

(define (package-details name)
  (define-values (p catalog-only-props)
    (match (get-pkgs #:name name)
      [(cons p _)
       (values p
               (list ':author (pkg-author p)
                     ':tags (get-pkg-tags name (pkg-catalog p))
                     ':catalog (pkg-catalog p)
                     ':deps (for/list ([d (in-list (get-pkg-dependencies name (pkg-catalog p) (pkg-checksum p)))])
                              (match-define (cons name qualifiers) d)
                              (cons name (string-join (map ~a qualifiers) " ")))
                     ':modules (for/list ([p (in-list (get-pkg-modules name (pkg-catalog p) (pkg-checksum p)))])
                                 (match p
                                   [`(lib ,path) path]
                                   [other        (format "~v" other)]))
                     ':description (cleanse-pkg-desc p)))]
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
                                   (get-pkgs-dir (current-pkg-scope)
                                                 (current-pkg-scope-version)))))
            (cddr orig-pkg))]
    [else orig-pkg]))

(define (cleanse-pkg-desc p)
  (regexp-replace* "[\r\n]" (pkg-desc p) " "))

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

;;; package config; ~= "raco pkg config" output

(define (package-config)
  (list ":catalogs" (or (current-pkg-catalogs)
                        (pkg-config-catalogs))
        ":name" (current-pkg-scope-version)
        ":default-scope" (~a (default-pkg-scope))
        ":cache" (current-pkg-download-cache-dir)))

;;; package operations

(define package-notify-channel (make-channel))

(define sema (make-semaphore 1))

(define (package-op verb name)
  (call-with-semaphore sema
                       (λ () (raw-package-op verb name))))

(define (raw-package-op verb name)
  (define act! (case verb
                 ['install (λ () (pkg-install-command #:auto #t name))]
                 ['update  (λ () (pkg-update-command name))]
                 ['remove  (λ () (pkg-remove-command #:auto #t name))]
                 [else     (error 'package-op "unknown verb")]))
  (define (put v)
    (channel-put package-notify-channel
                 (cons 'pkg-op-notify v)))
  (define-values (in out) (make-pipe))
  (parameterize ([current-output-port out]
                 [current-error-port out])
    (define (pump)
      (define bstr (make-bytes 2048))
      (match (read-bytes-avail! bstr in)
        [(? exact-nonnegative-integer? n)
         (put (bytes->string/utf-8 (subbytes bstr 0 n)))
         (pump)]
        [(? eof-object?)
         (put 'done)]))
    (thread pump)
    (with-handlers ([exn:fail? (λ (exn)
                                 (list 'error (exn-message exn)))])
      (act!))
    (flush-output out)
    (close-output-port out)))

(module+ test
  (define (pump)
    (match (channel-get package-notify-channel)
      [(cons 'pkg-op-notify (? string? s)) (display s)]
      [(cons 'pkg-op-notify 'done) (displayln "<Done>.")]
      [(list 'pkg-op-notify 'error (? string? s)) (displayln s)])
    (pump))
  (thread pump)
  (package-op 'install "ansi-color")
  (package-op 'remove "ansi-color"))

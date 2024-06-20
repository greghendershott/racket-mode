#lang racket/base

(require (only-in racket/format ~a ~v ~s)
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
                  pkg-remove-command)
         pkg/name
         net/url-string)

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
     (append (list ':name name
                   ':source (if p
                                (catalog-package-source p)
                                (installed-package-source ip))
                   ':status (if (installed-pkg-auto? ip) "dependency" "manual")
                   ':checksum (installed-pkg-checksum ip)
                   ':scope (installed-pkg-scope ip)
                   ':dir (path->string
                          (simple-form-path
                           (pkg-directory name))))
             catalog-only-props)]
    [p
     (append (list ':name name
                   ':source (catalog-package-source p)
                   ':status "available"
                   ':checksum (pkg-checksum p))
             catalog-only-props)]
    [else
     (list ':name name
           ':status "Package neither installed nor available from a catalog")]))

(struct installed-pkg (scope orig-pkg auto? checksum) #:transparent)
(define (installed-packages)
  (define ht (make-hash))
  (for ([scope (in-list (list 'installation 'user))])
    (for ([(name pi) (in-hash (installed-pkg-table #:scope scope))])
      (hash-set! ht name (installed-pkg scope
                                        (pkg-info-orig-pkg pi)
                                        (pkg-info-auto? pi)
                                        (or (pkg-info-checksum pi) "")))))
  ht)

(define (cleanse-pkg-desc p)
  (regexp-replace* "[\r\n]" (pkg-desc p) " "))

;; The "source" from a package /catalog/ seems to be always a simple
;; string, whereas for /installed packages/ the pkg-info-orig-pkg
;; field is an expression as documented at
;; <https://docs.racket-lang.org/pkg/path.html>.
;;
;; For the front end we want to return:
;;
;; 1. A label to display, e.g. ~a or ~s of whatever original value.
;;
;; 2. A URL, or, a local filesystem path.
;;
;;    - For a URL, simplify it so it's likely to work in a web browser
;;      to visit the user/repo.
;;
;;    - For a path, do stuff like package-source->path and/or
;;      simple-form-path.
;;
;; 3. A flag as to which kind 2 is, so that the front end can know it
;; should do back end -> front end translation for a local filesystem
;; path when the back end is remote.

(define (installed-package-source ip)
  (define source (installed-pkg-orig-pkg ip))
  (define scope (installed-pkg-scope ip))
  (cons
   (~s source)
   (match source
     ;; pkg-info-orig-pkg values for URLs
     [(or (list (or 'catalog 'clone) _ url)
          (list (or 'catalog 'git 'url) url))
      (list 'url
            (simplify-url url))]
     ;; pkg-info-orig-pkg values for local paths
     [(list (and type (or 'file 'dir 'link 'static-link)) path)
      (list 'path
            (path->string
             (simplify-path
              (path->complete-path (package-source->path path type)
                                   (get-pkgs-dir scope
                                                 (current-pkg-scope-version))))))])))

(define git-protos-px #px"^(?:github|git|git\\+http|git\\+https)://")

(define (simplify-url s)
  (match s
    ;; git flavors: Use https and simplify the path+query to just
    ;; user and repo path elements.
    [(pregexp git-protos-px)
     (define u (string->url s))
     (match-define (list* user repo _) (url-path u))
     (url->string (struct-copy url u
                               [scheme "https"]
                               [path (list user repo)]
                               [query null]))]
    [s s]))

(define (simple-path-string ps)
  (let ([ps (simplify-path
             (path->complete-path ps
                                  (get-pkgs-dir (current-pkg-scope)
                                                (current-pkg-scope-version))))])
    (if (string? ps)
        ps
        (path->string ps))))

(define (catalog-package-source p)
  (define source (pkg-source p))
  (cons
   (~a source)
   (match source
     ;; package catalog strings for URLs
     [(and s (pregexp git-protos-px))
      (list 'url (simplify-url s))]
     [(and s (pregexp "^https?://"))
      (list 'url s)]
     ;; package catalog strings for local paths
     [(pregexp "^(file://.+)[?]type=(.+)$" (list _ path type))
      (list 'path (simple-path-string
                   (package-source->path path (string->symbol type))))]
     [(and s (pregexp "^/[^/]"))
      (list 'path (simple-path-string s))]
     ;; Unknown
     [_
      (list 'unknown "")])))

;;; package config; ~= "raco pkg config" output

(define (package-config)
  (list ':catalogs (or (current-pkg-catalogs)
                        (pkg-config-catalogs))
        ':name (current-pkg-scope-version)
        ':default-scope (~a (default-pkg-scope))
        ':cache (current-pkg-download-cache-dir)))

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

(module+ example
  (define (pump)
    (match (channel-get package-notify-channel)
      [(cons 'pkg-op-notify (? string? s)) (display s)]
      [(cons 'pkg-op-notify 'done) (displayln "<Done>.")]
      [(list 'pkg-op-notify 'error (? string? s)) (displayln s)])
    (pump))
  (thread pump)
  (package-op 'install "ansi-color")
  (package-op 'remove "ansi-color"))


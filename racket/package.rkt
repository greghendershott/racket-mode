#lang racket/base

(require (only-in racket/format ~a ~v ~s)
         (only-in racket/hash hash-union!)
         racket/match
         racket/path
         racket/promise
         (only-in racket/string string-join)
         (except-in pkg/lib
                    pkg-desc)
         (only-in pkg/db
                  pkg?
                  current-pkg-catalog-file
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
         (only-in setup/getinfo get-info/full)
         net/url
         (only-in "scribble.rkt"
                  module-doc-path
                  refresh-module-doc-path-index!))

(provide package-list
         package-details
         package-op
         catalog-package-doc-link
         package-notify-channel)

(define (package-list)
  (define installed (installed-packages))
  (define catalog (for/hash ([p (in-list (get-pkgs))])
                    (values (pkg-name p) p)))
  (append
   ;; All packages from the catalogs:
   (for/list ([(name p) (in-hash catalog)])
     (define ip (hash-ref installed name #f))
     (define pi (and ip (installed-package-pkg-info ip)))
     (define status (cond
                      [(not ip)            "available"]
                      [(pkg-info-auto? pi) "dependency"]
                      [else                "installed"]))
     (list name
           status
           (cleanse-pkg-desc p)))
   ;; Installed packages not from the catalogs, i.e. that we didn't
   ;; already handle above:
   (for/list ([name (in-hash-keys installed)]
              #:when (not (hash-has-key? catalog name)))
     (list name
           "installed"
           ""))))

(define (package-details name)
  (define props (make-hasheq))
  (define (merge! . kvs)
    (hash-union! props (apply hash kvs) #:combine (λ (_a b) b)))
  ;; Start with props assuming neither catalog nor installed.
  (merge! 'name name
          'status "unknown -- neither installed nor available from a catalog"
          'config-name (current-pkg-scope-version)
          'config-catalogs (or (current-pkg-catalogs)
                               (pkg-config-catalogs))
          'default-scope (~a (default-pkg-scope)))
  ;; When pkg available from catalog, override with those details.
  (match (get-pkgs #:name name)
    [(cons (? pkg? p) _) ;if multiple, take just first
     (merge! 'status "available"
             'source (catalog-package-source p)
             'checksum (pkg-checksum p)
             'author (pkg-author p)
             'tags (get-pkg-tags name (pkg-catalog p))
             'catalog (pkg-catalog p)
             'deps (for/list ([d (in-list (get-pkg-dependencies name (pkg-catalog p) (pkg-checksum p)))])
                     (match-define (cons name qualifiers) d)
                     (cons name (string-join (map ~a qualifiers) " ")))
             'modules (sort
                       (for/list ([p (in-list (get-pkg-modules name (pkg-catalog p) (pkg-checksum p)))])
                         (match p
                           [`(lib ,path) path]
                           [other        (format "~s" other)]))
                       string<?)
             'description (cleanse-pkg-desc p))]
    [(list) (void)])
  ;; When pkg installed, override with those details.
  (match (hash-ref (installed-packages) name #f)
    [(? installed-package? ip)
     (define pi (installed-package-pkg-info ip))
     (define single? (sc-pkg-info? pi))
     (define dir (simple-form-path (pkg-directory name)))
     (merge! 'source (installed-package-source ip)
             'status (if (pkg-info-auto? pi) "dependency" "installed")
             'checksum (pkg-info-checksum pi)
             'scope (installed-package-scope ip)
             'dir (path->string dir)
             'modules (installed-package-modules dir single?))]
    [#f (void)])
  ;; Convert hash-table to association list. Omit values that are #f,
  ;; null, or blank strings.
  (for/list ([(k v) (in-hash props)]
             #:when (match v
                      [(or #f (list) (regexp "^[ ]+$")) #f]
                      [_ #t]))
    (cons k v)))

(struct installed-package
  (scope     ;(or/c 'installation 'user)
   pkg-info) ;pkg-info? including structs derived from pkg-info
  #:transparent)
(define (installed-packages)
  (define ht (make-hash))
  (for ([scope (in-list (list 'installation 'user))])
    (for ([(name pi) (in-hash (installed-pkg-table #:scope scope))])
      (hash-set! ht name (installed-package scope pi))))
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
;; 1. A label to display, such as ~s of the original or cleansed
;;    value.
;;
;; 2. A URL, or, a local filesystem path.
;;
;;    - URL: simplify to an http(s) likely to work in a web browser to
;;      visit the web page for the user/repo.
;;
;;    - Path, do package-source->path, complete relative to the
;;      scope's pkgs dir, and simplify.
;;
;; 3. A flag as to which kind 2 is, so the front end knows when it
;;    should do racket-mode-file-name-back-to-front.

(define (installed-package-source ip)
  (define source (pkg-info-orig-pkg (installed-package-pkg-info ip)))
  (define scope (installed-package-scope ip))
  (match source
    ;; pkg-info-orig-pkg values for URLs
    [(or (list (or 'catalog 'clone) _ url)
         (list (or 'catalog 'git 'url) url))
     (list (~s source)
           'url
           (simplify-url url))]
    ;; pkg-info-orig-pkg values for local paths
    [(list (and type (or 'file 'dir 'link 'static-link)) raw-path)
     (let ([path (path->string
                  (simplify-path
                   (path->complete-path (package-source->path raw-path type)
                                        (get-pkgs-dir scope
                                                      (current-pkg-scope-version)))))])
       (list (~s (list type path))
             'path
             path))]))

;; Note: Although my instinct was also to ignore "private" and
;; "scribblings", pkg.r-l.org includes them, so I am, too. The front
;; end UX could show them dimmed.
(define ignore-dirs
  (list (build-path ".git")
        (build-path ".github")
        (build-path "compiled")))

(define (get-info pkg-dir key get-default)
  (with-handlers ([exn:fail? (λ _ (get-default))])
    (match (get-info/full pkg-dir #:bootstrap? #t)
      [(? procedure? get) (get key get-default)]
      [#f                 (get-default)])))

(define (installed-package-modules pkg-dir single-collection-package?)
  (define (parent-dir) (car (reverse (explode-path pkg-dir))))
  (define prepend-collection-name
    (and single-collection-package?
         (match (get-info pkg-dir 'collection parent-dir)
           ['use-pkg-name (parent-dir)]
           [(? path-string? name) name]
           ['multi #f]))) ;defensive
  (define (use-dir? p)
    (not (member (file-name-from-path p) ignore-dirs)))
  (for*/list ([abs (in-directory pkg-dir use-dir?)]
              #:when (member (path-get-extension abs)
                             '(#".rkt" #".ss" #".scrbl"))
              [rel (in-value (find-relative-path pkg-dir abs))]
              #:when (and rel
                          (not (equal? (file-name-from-path rel)
                                       (build-path "info.rkt"))))
              [rel (in-value (if prepend-collection-name
                                 (build-path prepend-collection-name rel)
                                 rel))])
    (define-values (mod lang?)
      (match (map path->string (explode-path rel))
        [(list dirs ..1 (or "main.rkt" "main.ss"))
         (values (apply build-path dirs) #f)]
        [(list dirs ..1 "lang" "reader.rkt")
         (values (apply build-path dirs) #t)]
        [_
         (values (path-replace-suffix rel #"") #f)]))
    (define doc-path (module-doc-path (path->string mod) lang?))
    (list rel abs doc-path)))

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

(define (catalog-package-doc-link name)
  (with-handlers ([exn:fail? (λ _ null)])
    (match (call/input-url (string->url
                            (format "https://pkgs.racket-lang.org/pkg/~a"
                                    name))
                           get-pure-port
                           read)
      [(hash-table ('build
                    (hash-table
                     ('docs docs))))
       (for/list ([doc (in-list docs)])
         (match-define (list _ name path) doc)
         (list name
               (string-append "https://pkg-build.racket-lang.org/" path)))]
      [_ #f])))

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
                 ['refresh (λ () (pkg-catalog-update-local))]
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
    (close-output-port out)
    (refresh-module-doc-path-index!)))

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


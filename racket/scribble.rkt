#lang racket/base

(require (only-in html
                  read-html-as-xml)
         racket/contract
         racket/file
         racket/format
         racket/function
         racket/match
         racket/path
         racket/promise
         racket/string
         (only-in scribble/core
                  tag?)
         scribble/blueboxes
         scribble/manual-struct
         scribble/xref
         setup/xref
         (only-in xml
                  xml->xexpr
                  element
                  xexpr->string))

(provide binding->path+anchor
         path+anchor->html
         identifier->bluebox
         libs-exporting-documented)

(module+ test
  (require rackunit))

(define xref-promise (delay/thread (load-collections-xref)))

(define/contract (binding->path+anchor stx)
  (-> identifier? (or/c #f (cons/c path-string? (or/c #f string?))))
  (let* ([xref (force xref-promise)]
         [tag  (xref-binding->definition-tag xref stx 0)]
         [p+a  (and tag (tag->path+anchor xref tag))])
    p+a))

(define (tag->path+anchor xref tag)
  (define-values (path anchor) (xref-tag->path+anchor xref tag))
  (and path anchor (cons path anchor)))

;;; Scribble docs as HTML suitable for Emacs' shr renderer

(define/contract (path+anchor->html path+anchor)
  (-> (or/c #f (cons/c path-string? (or/c #f string?)))
      (or/c #f string?))
  (match path+anchor
    [(cons path anchor)
     (let* ([xexpr (get-raw-xexpr path anchor)]
            [xexpr (and xexpr (massage-xexpr path xexpr))]
            [html  (and xexpr (xexpr->string xexpr))])
       html)]
    [_ #f]))

(define (get-raw-xexpr path anchor)
  (define (heading-element? x)
    (match x
      [(cons (or 'h1 'h2 'h3 'h4 'h5 'h6) _) #t]
      [_ #f]))
  (match (let loop ([es (main-elements (html-file->xexpr path))])
           (match es
             [(list) (list)]
             [(cons (? (curryr anchored-element anchor) this) more)
              ;; Accumulate until another intrapara with an anchor, or
              ;; until a heading element indicating a new subsection.
              (cons this
                    (let get ([es more])
                      (match es
                        [(list) (list)]
                        [(cons (? heading-element?) _) (list)] ;stop
                        [(cons (? anchored-element) _) (list)] ;stop
                        [(cons this more) (cons this (get more))])))]
             [(cons _ more) (loop more)]))
    [(list) #f]
    [xs     `(div () ,@xs)]))

(module+ test
  (test-case "procedure"
    (check-not-false (path+anchor->html (binding->path+anchor #'print))))
  (test-case "syntax"
    (check-not-false (path+anchor->html (binding->path+anchor #'match))))
  (test-case "parameter"
    (check-not-false (path+anchor->html (binding->path+anchor #'current-eval))))
  (test-case "indented sub-item"
    (check-not-false (path+anchor->html (binding->path+anchor #'struct-out))))
  (test-case "deftogether"
    (test-case "1 of 2"
      (check-not-false (path+anchor->html (binding->path+anchor #'lambda))))
    (test-case "2 of 2"
      (check-not-false (path+anchor->html (binding->path+anchor #'λ)))))
  (check-not-false (path+anchor->html (binding->path+anchor #'xref-binding->definition-tag))))

(define (main-elements x)
  (match x
    [`(x () "\n"
       (html ()
             (head ,_ . ,_)
             (body ,_
                   (div ([class "tocset"]) . ,_)
                   (div ([class "maincolumn"])
                        (div ([class "main"]) . ,es))
                   . ,_)))
     es]
    [_ '()]))

;; anchored-element : xexpr? (or/c #f string?) -> (or/c #f string?)
;; When `name` is #f, return the first anchor having any name.
;; Otherwise, return the first anchor having `name`.
(define (anchored-element x [name #f])
  (define (anchor xs)
    (for/or ([x (in-list xs)])
      (match x
        [`(a ((name ,a)) . ,_) (or (not name) (equal? name a))]
        [`(,tag ,attrs . ,es)  (anchor es)]
        [_                     #f])))
  (match x
    [`(div ((class "SIntrapara"))
           (blockquote ((class "SVInsetFlow"))
                       (table ,(list-no-order `(class "boxed RBoxed") _ ...)
                              . ,es)))
     ;; That's likely sufficient to say we're in HTML resulting from a
     ;; Scribble defXXX form. From here on out, there can be some
     ;; variation, so just look recursively for anchors within `es'.
     (anchor es)]
    [`(blockquote ((class "leftindent"))
                  (p ())
                  (div ((class "SIntrapara"))
                       (blockquote ((class "SVInsetFlow"))
                                   (table ,(list-no-order `(class "boxed RBoxed") _ ...)
                                          . ,es)))
                  ,_ ...)
     (anchor es)]
    [_ #f]))

(define (html-file->xexpr pathstr)
  (xml->xexpr
   (element #f #f 'x '()
           (read-html-as-xml (open-input-string (file->string pathstr))))))

;; This is a big ole pile of poo, attempting to simplify and massage
;; the HTML so that Emacs shr renders it in the least-worst way.
;;
;; Note: Emacs shr renderer removes leading spaces and nbsp from <td>
;; elements -- which messes up the alignment of s-expressions
;; including contracts. But actually, the best place to address that
;; is up in Elisp, not here -- replace &nbsp; in the HTML with some
;; temporary character, then replace that character in the shr output.
(define (massage-xexpr html-pathname xexpr)
  ;; In addition to the main x-expression value handled by `walk`, we
  ;; have a couple annoying side values. Rather than "thread" them
  ;; through `walk` as additional values -- literally or using some
  ;; monadic hand-wavery -- I'm just going to set! them. Won't even
  ;; try to hide my sin by using make-parameter. I hereby accept the
  ;; deduction of Functional Experience Points.
  (define kind-xexprs '())
  (define provide-xexprs '())
  (define (walk x)
    (match x
      ;; The "Provided" title/tooltip. Set aside for later.
      [`(span ([title ,(and s (pregexp "^Provided from:"))]) . ,xs)
       (set! provide-xexprs (list s))
       `(span () ,@(map walk xs))]
      ;; The HTML for the "kind" (e.g. procedure or syntax or
      ;; parameter) comes before the rest of the bluebox. Simple HTML
      ;; renderers like shr don't handle this well. Set aside for
      ;; later.
      [`(div ([class "RBackgroundLabel SIEHidden"])
         (div ([class "RBackgroundLabelInner"]) (p () . ,xs)))
       (set! kind-xexprs `((i () ,@xs)))
       ""]
      ;; Bold RktValDef, which is the name of the thing.
      [`(a ([class ,(pregexp "RktValDef|RktStxDef")] . ,_) . ,xs)
       `(b () ,@(map walk xs))]
      ;; Kill links. (Often these won't work anyway -- e.g. due to
      ;; problems with "open" and file: links on macOS.)
      [`(a ,_ . ,xs)
       `(span () ,@(map walk xs))]
      ;; Kill "see also" notes, since they're N/A w/o links.
      [`(div ([class "SIntrapara"])
         (blockquote ([class "refpara"]) . ,_))
       `(span ())]
      ;; Delete some things that produce unwanted blank lines and/or
      ;; indents in simple rendering engines like Emacs' shr.
      [`(blockquote ([class ,(or "SVInsetFlow" "SubFlow")]) . ,xs)
       `(span () ,@(map walk xs))]
      [`(p ([class "RForeground"]) . ,xs)
       `(div () ,@(map walk xs))]
      ;; Let's italicize all RktXXX classes except RktPn.
      [`(span ([class ,(pregexp "^Rkt(?!Pn)")]) . ,xs)
       `(i () ,@(map walk xs))]
      ;; Image sources need path prepended.
      [`(img ,(list-no-order `[src ,src] more ...))
       `(img ([src ,(~a "file://" (path-only html-pathname) src)] . ,more))]
      ;; Misc element: Just walk kids.
      [`(,tag ,attrs . ,xs)
       `(,tag ,attrs ,@(map walk xs))]
      [x x]))
  (match (walk xexpr)
    [`(div () . ,xs)
     (define hs
       (match* [kind-xexprs provide-xexprs]
         [[`() `()] `()]
         [[ks   ps] `((span () ,@ks 'nbsp ,@ps))]))
     `(div () ,@hs ,@xs)]))

(module+ test
  (check-equal? ;issue 410
   (massage-xexpr (string->path "/path/to/file.html")
                  `(div ()
                    (img ([x "x"] [src "foo.png"] [y "y"]))))
   `(div ()
     (img ([src "file:///path/to/foo.png"] [x "x"] [y "y"])))))

;;; Blueboxes

(define racket-version-6.10? (equal? (version) "6.10"))

(define bluebox-cache (delay (make-blueboxes-cache #t)))

(define/contract (identifier->bluebox stx)
  (-> identifier? (or/c #f string?))
  (match (and (not racket-version-6.10?)
              (xref-binding->definition-tag (force xref-promise) stx 0))
    [(? tag? tag)
     (match (fetch-blueboxes-strs tag #:blueboxes-cache (force bluebox-cache))
       [(list* _kind strs)
        (string-replace (string-join strs "\n")
                        "\u00A0"
                        " ")]
       [_ #f])]
    [_ #f]))

(module+ test
  ;; This test succeeds on all Racket versions before and after 6.10.
  ;; I spent an hour installing 6.10 locally and exploring the problem
  ;; but so far have no clue. As neither 6.10 nor I are getting any
  ;; younger, I am choosing to ignore this, for now.
  ;;
  ;; Probably https://github.com/racket/drracket/issues/118
  (unless racket-version-6.10?
    (check-equal? (identifier->bluebox #'list)
                  "(list v ...) -> list?\n  v : any/c"))
  (check-false (identifier->bluebox (datum->syntax #f (gensym)))))

;;; Documented exports of a symbol by zero or more libraries

(define (libs-exporting-documented sym-as-str)
  ;; (-> string? (listof (list/c sym:string? tag:string?)))
  (define sym (string->symbol sym-as-str))
  (define xref (force xref-promise))
  (define idx (xref-index xref))
  (define libs
    (for*/list ([entry (in-list idx)]
                [desc  (in-value (entry-desc entry))]
                #:when (and (exported-index-desc? desc)
                            (eq? sym (exported-index-desc-name desc)))
                [lib   (in-value (car (exported-index-desc-from-libs desc)))]) ;*
      (symbol->string lib)))
  (sort libs
        string<?
        #:cache-keys? #t
        #:key (match-lambda
                [(and (pregexp "^racket/") v)       (string-append "0_" v)]
                [(and (pregexp "^typed/racket/") v) (string-append "1_" v)]
                [v v])))

;; *: (exported-index-desc-from-libs desc) is a list. Empirically -- I
;; haven't yet found the docs or spec -- the list is usually a single
;; lib. Sometimes it is multiple, e.g. '(racket/base racket) or
;; '(typed/racket/base typed/racket). Generalizing from those two
;; examples (!), the most-specific one is first. The most-specific one
;; is what we'd prefer in a feature that automatically adds a require.
;; TL;DR: Always just use the car of this list.

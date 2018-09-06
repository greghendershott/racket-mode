#lang racket/base

(require (only-in html
                  read-html-as-xml)
         racket/file
         racket/function
         racket/match
         scribble/xref
         setup/xref
         (only-in xml
                  xml->xexpr
                  element
                  xexpr->string))

(provide scribble-doc/html
         binding->path+anchor)

;;; Extract Scribble documentation as modified HTML suitable for
;;; Emacs' shr renderer.

(define (scribble-doc/html stx)
  (define xexpr (scribble-doc/xexpr stx))
  (and xexpr (xexpr->string xexpr)))

(define (scribble-doc/xexpr stx)
  (define xexpr (scribble-doc/xexpr-raw stx))
  (and xexpr (massage-xexpr xexpr)))

(define (scribble-doc/xexpr-raw stx)
  (define-values (path anchor) (binding->path+anchor stx))
  (and path anchor (scribble-get-xexpr path anchor)))

(define (binding->path+anchor stx)
  (define xref (load-collections-xref))
  (define tag (and (identifier? stx)
                   (xref-binding->definition-tag xref stx 0)))
  (cond [tag (xref-tag->path+anchor xref tag)]
        [else (values #f #f)]))

(define (scribble-get-xexpr path anchor)
  (match (let loop ([es (main-elements (html-file->xexpr path))])
           (match es
             [(list) (list)]
             [(cons (? (curryr anchored-element anchor) this) more)
              ;; Accumulate until another intrapara with an anchor
              (cons this
                    (let get ([es more])
                      (match es
                        [(list) (list)]
                        [(cons (? anchored-element) _) (list)] ;stop
                        [(cons this more) (cons this (get more))])))]
             [(cons _ more) (loop more)]))
    [(list) #f]
    [xs     `(div () ,@xs)]))

(module+ test
  (require rackunit)
  (test-case "procedure"
   (check-not-false (scribble-doc/xexpr #'print)))
  (test-case "syntax"
    (check-not-false (scribble-doc/xexpr #'match)))
  (test-case "parameter"
    (check-not-false (scribble-doc/xexpr #'current-eval)))
  (test-case "indented sub-item"
    (check-not-false (scribble-doc/xexpr #'struct-out)))
  (test-case "deftogether"
    (test-case "1 of 2"
      (check-not-false (scribble-doc/xexpr #'lambda)))
    (test-case "2 of 2"
      (check-not-false (scribble-doc/xexpr #'λ))))
  (check-not-false (scribble-doc/xexpr #'xref-binding->definition-tag)))

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
(define (massage-xexpr x)
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
       (set! kind-xexprs xs)
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
      ;; Misc element: Just walk kids.
      [`(,tag ,attrs . ,xs)
       `(,tag ,attrs ,@(map walk xs))]
      [x x]))
  (match (walk x)
    [`(div () . ,xs)
     `(div ()
       (span ([style "color: #C0C0C0"])
             (i () ,@kind-xexprs)
             'nbsp
             ,@provide-xexprs)
       ,@xs)]))

#lang racket/base

(require racket/file
         racket/match
         scribble/xref
         setup/xref
         (only-in xml xml->xexpr element xexpr->string)
         (only-in html read-html-as-xml))

(provide scribble-doc/text
         scribble-doc/html)

;;; Extract Scribble documentation as plain text or as modified HTML
;;; suitable for Emacs' shr renderer.

(define (scribble-doc/text stx)
  (define xexpr (scribble-doc/xexpr stx))
  (and xexpr (xexpr->text xexpr)))

(define (scribble-doc/html stx)
  (define xexpr (scribble-doc/xexpr stx))
  (and xexpr (xexpr->string (xexpr->html xexpr))))

(define (scribble-doc/xexpr stx)
  (define-values (path anchor) (binding->path+anchor stx))
  (and path anchor (scribble-get-xexpr path anchor)))

(define (binding->path+anchor stx)
  (define xref (load-collections-xref))
  (define tag (and (identifier? stx)
                   (xref-binding->definition-tag xref stx 0)))
  (cond [tag (xref-tag->path+anchor xref tag)]
        [else (values #f #f)]))

(define (scribble-get-xexpr path anchor)
  (match (html-file->xexpr path)
    [`(x () "\n"
       (html ()
             (head ,_ . ,_)
             (body ,_
                   (div ([class "tocset"]) . ,_)
                   (div ([class "maincolumn"])
                        (div ([class "main"]) ,es ...))
                   ,_ ...)))
     (define xs
       (let loop ([es es])
         (match es
           [(list) (list)]
           [(cons this more)
            (cond [(equal? anchor (intrapara-anchor this))
                   ;; Accumulate until another intrapara with an anchor
                   (cons this (let get ([es more])
                                (match es
                                  [(list) (list)]
                                  [(cons (? intrapara-anchor) _) (list)] ;stop
                                  [(cons this more) (cons this (get more))])))]
                  [else (loop more)])])))
     `(div () ,@xs)]
    [_ #f]))

(define (intrapara-anchor x)
  (define (anchor xs)
    (for/or ([x (in-list xs)])
      (match x
        [`(a ((name ,anchor)) ,_ ...) anchor]
        [`(,tag ,attrs ,es ...) (anchor es)]
        [_ #f])))
  (match x
    [`(div ((class "SIntrapara"))
       (blockquote
        ((class "SVInsetFlow"))
        (table
         ((cellpadding "0") (cellspacing "0") (class "boxed RBoxed"))
         (tbody
          ()
          (tr
           ()
           (td
            ()
            (blockquote
             ((class "SubFlow"))
             (div
              ((class "RBackgroundLabel SIEHidden"))
              (div
               ((class "RBackgroundLabelInner"))
               (p () ,_ ...)))
             ;; That should be enough to say we're in a help item.
             ;; From here on out, there can be some variation, so just
             ;; look recursively for the first anchor.
             ,es ...)))
          ,_ ...))))
     (anchor es)]
    [_ #f]))

(define (html-file->xexpr pathstr)
  (xml->xexpr
   (element #f #f 'x '()
           (read-html-as-xml (open-input-string (file->string pathstr))))))

;; FIXME: Not sure the newline strategy is correct here, wrt nested
;; elements. e.g. (td () (p () ___)) should probably be 1 not 2
;; newlines.
(define (xexpr->text x)
  (define o (open-output-string))
  (let loop ([x x]
             [pars '()])
    (match x
      [`(div ([class "SIntrapara"]) ;ignore "see also" boxes
         (blockquote ([class "refpara"]) ,_ ...))
       (void)]
      [`(div ([class "SIntrapara"]) ,xs ...)
       (for ([x xs]) (loop x (cons 'div pars)))
       (newline o)
       (newline o)]
      [(list 'p _ xs ...)
       (for ([x xs]) (loop x (cons 'p pars)))
       (newline o)
       (unless (memq 'td pars) (newline o))]
      [(list (and tag (or 'tr 'th 'br)) _ xs ...)
       (for ([x xs]) (loop x (cons tag pars)))
       (newline o)]
      [(list tag _ xs ...) (for ([x xs]) (loop x (cons tag pars)))]
      [(? string?) (display x o)]
      [(? char?) (display x o)]
      [(? symbol?) (display (translate-symbol x) o)]
      ;;[_ (error 'xexpr->text "Unexpected value: ~s" x)]
      [x "<??????????>"]))
  (get-output-string o))

(define (translate-symbol x)
  (case x
    [(nbsp) " "]
    [(rarr) "->"]
    [(larr) "<-"]
    [(rsquo) "'"]
    [(ldquo) "``"]
    [(rdquo) "''"]
    [(ndash) "-"]
    [(mdash) "--"]
    [(amp) "&"]
    ;;[else (error 'translate-symbol "Don't know how to translate ~s" x)]
    [else "<??>"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a big ole pile of poo, attempting to simplify and massage
;; the HTML so that Emacs shr renders it in the least-worst way.
(define (xexpr->html x)
  (match x
    ;; Emacs shr renderer removes leading spaces and nbsp from <td>
    ;; elements -- which messes up the alignment of s-expressions
    ;; including contracts. But actually, the best place to address
    ;; that is up in Elisp, not here: Replace nbsp in the HTML with
    ;; some temporary character, then replace that character in the
    ;; shr output.

    ;; Bold RktValDef, which is the name of the thing.
    [`(a ([class ,(pregexp "RktValDef")] ,_ ...) ,xs ...)
     `(b () ,@(map xexpr->html xs))]
    ;; Kill links. Due the problem with "open" and file: links on OSX,
    ;; these won't work.
    [`(a ,_ ,xs ...)
     `(span () ,@(map xexpr->html xs))]
    ;; Kill "see also" notes, since they're N/A w/o links.
    [`(div ([class "SIntrapara"])
       (blockquote ([class "refpara"]) ,_ ...))
     `(span ())]
    ;; Delete some things that produce unwanted blank lines and/or
    ;; indents in simple rendering engines like Emacs' shr.
    [`(blockquote ([class ,(or "SVInsetFlow" "SubFlow")]) ,xs ...)
     `(div () ,@(map xexpr->html xs))]
    [`(p ([class "RForeground"]) ,xs ...)
     `(div () ,@(map xexpr->html xs))]
    ;; The HTML for the "kind" (e.g. procedure or syntax or parameter)
    ;; comes before the rest of the bluebox. Simple HTML renderers
    ;; like shr don't handle this well. Kill it.
    [`(div ([class "RBackgroundLabel SIEHidden"]) ,xs ...)
     ""]
    ;; Let's italicize all RktXXX classes.
    [`(span ([class ,(pregexp "^Rkt")]) ,xs ...)
     `(i () ,@(map xexpr->html xs))]
    [`(,tag ,attrs ,xs ...)
     `(,tag ,attrs ,@(map xexpr->html xs))]
    [x x]))

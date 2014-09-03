#lang racket/base

(require racket/match
         racket/string
         racket/file
         (only-in html read-html-as-xml)
         (only-in xml xml->xexpr element))

(require scribble/xref
         setup/xref)

(provide scribble-doc/text)

;;; Extract Scribble documentation as plain text

(define (scribble-doc/text stx)
  (define xexpr (scribble-doc/xexpr stx))
  (and xexpr (xexpr->text xexpr)))

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
     `(span () ,@xs)]
    [_ #f]))

(define (intrapara-anchor x)
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
             (p
              ((class "RForeground"))
              (span ((class "RktPn")) "(")
              (a ((name ,anchor)))
              ,_ ...))))
          ,_ ...))))
     anchor]
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

;; (displayln
;; (scribble-doc/text (string->path "/Users/greg/src/plt/racket/racket/doc/reference/Writing.html")
;;                      "(def._((quote._~23~25kernel)._display))"))

(require racket/pretty)

;;(pretty-print (scribble-doc/xexpr #'display))
;;(displayln (scribble-doc/text #'display))

;; (pretty-print (scribble-doc/xexpr #'match-define))
;; (displayln (scribble-doc/text #'match-define))

#lang racket/base

(require
 racket/function
 racket/format
 racket/contract
 scribble/xref
 scribble/manual-struct
 (for-syntax
  racket/syntax
  racket/string
  racket/base
  syntax/parse)
 "help.rkt")

(define entry/c
  (list/c
   (non-empty-listof string?)
   (or/c
    (cons/c
     symbol?
     (listof any/c))
    #f)
   (cons/c path-string?
           (or/c string? #f))))

(define-syntax index-desc
  (syntax-parser
    [(_ id:id)
     (with-syntax
       ([pred? (format-id #'id "~a?"
                          (string-trim
                           (symbol->string
                            (syntax->datum #'id))
                           "-index-desc?" #:left? #f))])
       #'(cons pred?
               (const (list (quote id)))))]))

(define index-desc->list
  (list
   (index-desc module-path-index-desc)
   (index-desc thing-index-desc)
   (index-desc form-index-desc)
   (index-desc reader-index-desc)
   (index-desc class-index-desc)
   (index-desc language-index-desc)
   (index-desc interface-index-desc)
   (index-desc mixin-index-desc)
   (index-desc struct-index-desc)
   (cons exported-index-desc?
         (λ (%)
           (list
            'exported
            (exported-index-desc-from-libs %))))))

(define (entry->list entry)
  (define desc (entry-desc entry))
  (define-values (path anchor)
    (xref-tag->path+anchor xref (entry-tag entry)))
  (define desc->list
    (findf
     (λ (%)
       ((car %) desc))
     index-desc->list))
  (list
   (entry-words entry)
   (if desc->list
       ((cdr desc->list) desc)
       #f)
   (cons (~a path) anchor)))

(define/contract index
  (listof entry/c)
  (map
   entry->list
   (xref-index xref)))

(provide index)

(module+ test
  (require rackunit)
  (check-true (pair? index)))

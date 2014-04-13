#lang racket/base

(require racket/string
         racket/pretty
         racket/lazy-require
         "defn.rkt"
         "util.rkt")

(provide (all-defined-out))

(define (def sym)
  (display-definition (symbol->string sym)))

(define (doc str)
  (eval `(begin
          (require racket/help)
          (help ,(string-trim str))
          (newline))))

(define (cd s)
  (let ([old-wd (current-directory)])
    (current-directory s)
    (unless (directory-exists? (current-directory))
      (display-commented (format "~v doesn't exist." (current-directory)))
      (current-directory old-wd))
    (display-commented (format "In ~v" (current-directory)))))

;;; syntax expansion

(define last-stx #f)

(define (exp1)
  (set! last-stx (expand-once (read)))
  (pp-stx last-stx))

(define (exp+)
  (when last-stx
    (define this-stx (expand-once last-stx))
    (cond [(equal? (syntax->datum last-stx) (syntax->datum this-stx))
           (display-commented "Already fully expanded.")
           (set! last-stx #f)]
          [else
           (pp-stx this-stx)
           (set! last-stx this-stx)])))

(define (exp!)
  (set! last-stx #f)
  (pp-stx (expand (read))))

(define (pp-stx stx)
  (newline)
  (pretty-print (syntax->datum stx)))

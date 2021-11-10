#lang racket/base

(require racket/class
         racket/contract
         "core.rkt"
         (submod "core.rkt" private)
         (only-in "../indent-at-exp.rkt"
                  [indent-amount sexp:indent-amount]))

(provide indenter
         line-amount)

(define/contract (line-amount info-indenter tm gen pos auto?)
  (-> (-> object? position/c boolean? any) token-map? generation/c position/c boolean? integer?)
  (block-until-updated-thru tm gen pos)
  (or (info-indenter (token-map-like-text% tm) pos auto?)
      (sexp:indent-amount tm gen pos)))

(define (indenter get-info)
  (let ([line-indent  (get-info 'drracket:indentation #f)]
        [range-indent (get-info 'drracket:range-indentation #f)])
    (cond
      [(or line-indent range-indent)
       (λ (t pos auto?)
         (cond
           [(and auto? line-indent)
            (line-indent t pos)]
           [else
            (define r (and range-indent
                           (range-indent t pos pos)))
            (if r
                (if (null? r)
                    (list 0 "")
                    (car r))
                (and line-indent
                     (line-indent t pos)))]))]
      [else default-indenter])))

(define (default-indenter _like-text _pos _auto?) #f)

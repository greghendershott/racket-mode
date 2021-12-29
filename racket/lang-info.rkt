#lang racket/base

(provide (struct-out lang-info)
         lang-info-grouping-position-is-racket?)

;; This is its own file really just so that hash-lang.bridge.rkt can
;; require it normally and not need to do more dynamic-requires.

(struct lang-info
  (lexer
   paren-matches
   quote-matches
   grouping-position
   line-indenter
   range-indenter
   submit-predicate)
  #:transparent #:authentic)

(define racket-grouping-position
  (with-handlers ([exn:fail? (λ _ #f)])
    (dynamic-require 'syntax-color/racket-navigation 'racket-grouping-position)))

(define (lang-info-grouping-position-is-racket? li)
  (equal? (lang-info-grouping-position li) racket-grouping-position))


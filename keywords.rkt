#lang typed/racket/no-check

(require racket/syntax)

;; Generate lists for Racket keywords, builtins, and types.
;;
;; The question of what is a "keyword" and a "builtin" is not so
;; simple in Racket:
;;
;; 1. The distinction between the two is squishy, and from one point
;; of view Racket has 1400+ "primitives" (!).
;;
;; 2. As for "builtins", there are many, many "batteries included"
;; libraries in the main distribution. Where to draw the line?
;;
;; 3. More fundamentally, Racket is a language for making languages.
;; Ultimately the only way to be 100% correct is to do something
;; "live" with namespace-mapped-symbols. But I don't see that as
;; performant for Emacs font-lock.
;;
;; Here I'm saying that:
;;
;; (a) "keywords" are syntax (only) from racket/base
;;
;; (b) "builtins" are everything else provided by #lang racket and
;; #lang typed/racket (except the capitalized Types from typed/racket
;; go into their own list). Plus for modern macros, racket/syntax and
;; a few items from syntax/parse (but not its the syntax classes,
;; because `id` and `str` are too "generic" and too likely to be user
;; program identifiers).
;;
;; Is that somewhat arbitrary? Hell yes. It's my least-worst,
;; practical idea for now. Also, IMHO it's an improvement over getting
;; pull requests to add people's favorites, a few at a time. At least
;; this way is consistent, and can be regenerated programatically as
;; Racket evolves.

(define (symbol<=? a b)
  (string<=? (symbol->string a) (symbol->string b)))

(define (exports mod #:only-stx? [only-stx? #f])
  (define (ids phases)
    (for*/list ([phase phases]
                [item (cdr phase)])
      (car item)))
  (define-values (vars stxs) (module->exports mod))
  (sort (remove-duplicates (append (ids stxs)
                                   (if only-stx? '() (ids vars)))
                           eq?)
        symbol<=?))

(define (subtract xs ys)
  (for*/list ([x xs] #:when (not (memq x ys))) x))

(define base-stx (exports 'racket/base #:only-stx? #t))

(define rkt      (append (exports 'racket)
                         (exports 'racket/syntax)
                         '(syntax-parse syntax-parser define-simple-macro)))
(define rkt+     (subtract rkt base-stx))

(define tr       (exports 'typed/racket))
(define tr+      (subtract tr rkt)) ;This includes Types, too

(define Types    (for/list ([x tr+]
                            #:when (char-upper-case? (string-ref (symbol->string x) 0)))
                   x))

;;; The final lists

(define keywords base-stx)

(define builtins
  (sort (subtract (remove-duplicates (append rkt+
                                             (subtract tr+ Types))
                                     eq?)
                  base-stx)
        symbol<=?))
;; So many builtins, Emacs gives "regexp too long" error, so split into two:
(define-values (builtins1 builtins2)
  (let ([mid (/ (length builtins) 2)])
    (for/fold ([xs '()]
               [ys '()])
              ([x builtins]
               [i (in-naturals)])
      (cond [(< i mid) (values (cons x xs) ys)]
            [else      (values xs (cons x ys))]))))

(define types Types)

(define (prn xs)
  (pretty-print (map symbol->string (sort xs symbol<=?))))

;; Run these to print, copy and paste into racket-keywords-and-builtins.el
;; (prn keywords)
;; (prn builtins1)
;; (prn builtins2)
;; (prn types)

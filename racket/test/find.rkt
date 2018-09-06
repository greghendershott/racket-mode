#lang at-exp racket/base

(require racket/format
         racket/match
         racket/runtime-path
         rackunit
         syntax/modread
         "../find.rkt"
         "find-examples.rkt")

(define-runtime-path dot-dot "..")

(define-namespace-anchor nsa)
(parameterize ([current-namespace (namespace-anchor->namespace nsa)])
  (define (not-0 v) (not (= 0 v)))
  (define (not-1 v) (not (= 1 v)))

  (check-equal? (find-definition "display")
                'kernel)
  (check-equal? (find-signature "display")
                '("defined in #%kernel, signature unavailable"))

  (check-match (find-definition "displayln")
               (list (pregexp "/racket/private/misc\\.rkt$")
                     (? not-1)
                     (? not-0)))
  (check-equal? (find-signature "displayln")
                '((displayln v) (displayln v p))) ;case-lambda defn

  ;; Test a definer macro that (as of Racket 6.7) does not properly
  ;; set srcloc: Can we at least return a specfic location for its
  ;; parent syntax (as opposed to line 1 column 0)?
  (check-match (find-definition "in-hash")
               (list (pregexp "/racket/private/for.rkt$")
                     (? not-1)
                     (? not-0)))

  ;; Tests for specific locations in find-examples.rkt

  (check-match (find-definition "plain")
               (list (pregexp "find-examples.rkt$") 7 9))
  (check-equal? (find-signature "plain")
                '(plain x))

  (check-match (find-definition "renamed")
               (list (pregexp "find-examples.rkt$") 7 9))
  (check-equal? (find-signature "renamed")
                '(plain x))

  (check-match (find-definition "contracted1")
               (list (pregexp "find-examples.rkt$") 11 9))
  (check-equal? (find-signature "contracted1")
                '(contracted1 x))

  (check-match (find-definition "contracted2")
               (list (pregexp "find-examples.rkt$") 13 9))
  (check-equal? (find-signature "contracted2")
                '(contracted2 x))

  (check-match (find-definition "contracted/renamed")
               (list (pregexp "find-examples.rkt$") 16 9))
  (check-equal? (find-signature "contracted/renamed")
                '(c/r x))

  (check-match (find-definition "plain-by-macro")
               (list (pregexp "find-examples.rkt$") 23 15))
  (check-false (find-signature "plain-by-macro"))

  (check-match (find-definition "contracted-by-macro")
               (list (pregexp "find-examples.rkt$") 29 20))
  (check-false (find-signature "contracted-by-macro"))

  (check-match (find-definition "sub")
               (list (pregexp "find-examples.rkt$") 38 11))
  (check-equal? (find-signature "sub")
                '(sub x))

  (check-match (find-definition "sub/renamed")
               (list (pregexp "find-examples.rkt$") 38 11))
  (check-equal? (find-signature "sub/renamed")
                '(sub x))

  (check-match (find-definition "foo")
               (list (pregexp "find-examples.rkt$") 48 9))
  (check-equal? (find-signature "foo")
                '(foo x))

  (check-match (find-definition "a-number")
               (list (pregexp "find-examples.rkt$") 52 8))

  (check-match (find-definition "a-parameter")
               (list (pregexp "find-examples.rkt$") 54 8))

  ;; This is (roughly) a test of opening a Racket source file and
  ;; doing M-. on every non-list sexpr: Call find-definition on each
  ;; sexpr. Not-found (#f) is fine. But fail test for (list _ 1 0) --
  ;; i.e. the source file was found, but not the location within.
  (define (check-non-bof-location file)
    (define ht (make-hash))
    (define (find k) ;memoized find-definition
      (hash-ref ht k
                (λ ()
                  (define v (find-definition (format "~a" k)))
                  (hash-set! ht k v)
                  v)))
    (define (walk v)
      (if (list? v)
          (for-each walk v)
          (match (find v)
            [(list where 1 0)
             (fail @~a{can't find definition of `@|v|` in @where})]
            [_ (void)])))
    (walk
     (with-module-reading-parameterization
       ;; Why read not read-syntax? Because we only care about the
       ;; sexprs as text: `find-definition` takes a string, because
       ;; `racket-visit-definition` takes text from an Emacs buffer.
       (λ () (with-input-from-file file read)))))
  (for ([file '("commands/requires.rkt"
                "run.rkt")])
    (check-non-bof-location (build-path dot-dot file))))

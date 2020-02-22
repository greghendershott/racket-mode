#lang at-exp racket/base

(require racket/format
         racket/match
         racket/runtime-path
         rackunit
         syntax/modread
         "../find.rkt"
         "find-examples.rkt")

(define-runtime-path here "..")

(define-namespace-anchor nsa)
(parameterize ([current-namespace (namespace-anchor->namespace nsa)])
  (define (not-0 v) (not (= 0 v)))
  (define (not-1 v) (not (= 1 v)))

  (check-equal? (find-definition 'namespace "display")
                'kernel)
  (check-equal? (find-signature 'namespace "display")
                '("defined in #%kernel, signature unavailable"))

  (check-match (find-definition 'namespace "displayln")
               (list (pregexp "/racket/private/misc\\.rkt$")
                     (? not-1)
                     (? not-0)))
  (check-equal? (find-signature 'namespace "displayln")
                '((displayln v) (displayln v p))) ;case-lambda defn

  ;; Test a definer macro that (as of Racket 6.7) does not properly
  ;; set srcloc: Can we at least return a specfic location for its
  ;; parent syntax (as opposed to line 1 column 0)?
  (check-match (find-definition 'namespace "in-hash")
               (list (pregexp "/racket/private/for.rkt$")
                     (? not-1)
                     (? not-0)))

  ;; Tests for specific locations in find-examples.rkt

  (check-match (find-definition 'namespace "plain")
               (list (pregexp "find-examples.rkt$") 7 9))
  (check-equal? (find-signature 'namespace "plain")
                '(plain x))

  (check-match (find-definition 'namespace "renamed")
               (list (pregexp "find-examples.rkt$") 7 9))
  (check-equal? (find-signature 'namespace "renamed")
                '(plain x))

  (check-match (find-definition 'namespace "contracted1")
               (list (pregexp "find-examples.rkt$") 11 9))
  (check-equal? (find-signature 'namespace "contracted1")
                '(contracted1 x))

  (check-match (find-definition 'namespace "contracted2")
               (list (pregexp "find-examples.rkt$") 13 9))
  (check-equal? (find-signature 'namespace "contracted2")
                '(contracted2 x))

  (check-match (find-definition 'namespace "contracted/renamed")
               (list (pregexp "find-examples.rkt$") 16 9))
  (check-equal? (find-signature 'namespace "contracted/renamed")
                '(c/r x))

  (check-match (find-definition 'namespace "plain-by-macro")
               (list (pregexp "find-examples.rkt$") 23 15))
  (check-false (find-signature 'namespace "plain-by-macro"))

  (check-match (find-definition 'namespace "contracted-by-macro")
               (list (pregexp "find-examples.rkt$") 29 20))
  (check-false (find-signature 'namespace "contracted-by-macro"))

  (check-match (find-definition 'namespace "sub")
               (list (pregexp "find-examples.rkt$") 38 11))
  (check-equal? (find-signature 'namespace "sub")
                '(sub x))

  (check-match (find-definition 'namespace "sub/renamed")
               (list (pregexp "find-examples.rkt$") 38 11))
  (check-equal? (find-signature 'namespace "sub/renamed")
                '(sub x))

  (check-match (find-definition 'namespace "foo")
               (list (pregexp "find-examples.rkt$") 48 9))
  (check-equal? (find-signature 'namespace "foo")
                '(foo x))

  (check-match (find-definition 'namespace "a-number")
               (list (pregexp "find-examples.rkt$") 52 8))

  (check-match (find-definition 'namespace "a-parameter")
               (list (pregexp "find-examples.rkt$") 54 8))

  ;; This is (roughly) a test of opening a Racket source file and
  ;; doing M-. on every non-list sexpr: Call find-definition on each
  ;; sexpr. Not-found (#f) is fine. But fail test for (list _ 1 0) --
  ;; i.e. the source file was found, but not the location within.
  (define (check-non-bof-location file)
    (define ht (make-hash))
    (define (find k) ;memoized find-definition 'namespace
      (hash-ref ht k
                (λ ()
                  (define v (find-definition 'namespace (format "~a" k)))
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
                "repl.rkt")])
    (check-non-bof-location (build-path here file))))

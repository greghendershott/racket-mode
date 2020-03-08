#lang at-exp racket/base

(require racket/format
         racket/match
         racket/runtime-path
         rackunit
         syntax/modread
         "../find.rkt"
         "../syntax.rkt"
         "find-examples.rkt")

(define (not-0 v) (not (= 0 v)))
(define (not-1 v) (not (= 1 v)))

(define-runtime-path parent-dir "..")

(define (test how)
  (check-equal? (find-definition how "display")
                'kernel)
  (check-equal? (find-signature how "display")
                '("defined in #%kernel, signature unavailable"))

  (check-match (find-definition how "displayln")
               (list (pregexp "/racket/private/misc\\.rkt$")
                     (? not-1)
                     (? not-0)))
  (check-equal? (find-signature how "displayln")
                '((displayln v) (displayln v p))) ;case-lambda defn

  ;; Test a definer macro that (as of Racket 6.7) does not properly
  ;; set srcloc: Can we at least return a specfic location for its
  ;; parent syntax (as opposed to line 1 column 0)?
  (check-match (find-definition how "in-hash")
               (list (pregexp "/racket/private/for.rkt$")
                     (? not-1)
                     (? not-0)))

  ;; Tests for specific locations in find-examples.rkt

  (check-match (find-definition how "plain")
               (list (pregexp "find-examples.rkt$") 7 9))
  (check-equal? (find-signature how "plain")
                '(plain x))

  (check-match (find-definition how "renamed")
               (list (pregexp "find-examples.rkt$") 7 9))
  (check-equal? (find-signature how "renamed")
                '(plain x))

  (check-match (find-definition how "contracted1")
               (list (pregexp "find-examples.rkt$") 11 9))
  (check-equal? (find-signature how "contracted1")
                '(contracted1 x))

  (check-match (find-definition how "contracted2")
               (list (pregexp "find-examples.rkt$") 13 9))
  (check-equal? (find-signature how "contracted2")
                '(contracted2 x))

  (check-match (find-definition how "contracted/renamed")
               (list (pregexp "find-examples.rkt$") 16 9))
  (check-equal? (find-signature how "contracted/renamed")
                '(c/r x))

  (check-match (find-definition how "plain-by-macro")
               (list (pregexp "find-examples.rkt$") 23 15))
  (check-false (find-signature how "plain-by-macro"))

  (check-match (find-definition how "contracted-by-macro")
               (list (pregexp "find-examples.rkt$") 29 20))
  (check-false (find-signature how "contracted-by-macro"))

  (check-match (find-definition how "sub")
               (list (pregexp "find-examples.rkt$") 38 11))
  (check-equal? (find-signature how "sub")
                '(sub x))

  (check-match (find-definition how "sub/renamed")
               (list (pregexp "find-examples.rkt$") 38 11))
  (check-equal? (find-signature how "sub/renamed")
                '(sub x))

  (check-match (find-definition how "foo")
               (list (pregexp "find-examples.rkt$") 48 9))
  (check-equal? (find-signature how "foo")
                '(foo x))

  (check-match (find-definition how "a-number")
               (list (pregexp "find-examples.rkt$") 52 8))

  (check-match (find-definition how "a-parameter")
               (list (pregexp "find-examples.rkt$") 54 8))

  (check-match (find-definition how "from-m")
               (list (pregexp "find-examples.rkt$") 58 10))

  ;; This is (roughly) a test of opening a Racket source file and
  ;; doing M-. on every non-list sexpr: Call find-definition on each
  ;; sexpr. Not-found (#f) is fine. But fail test for (list _ 1 0) --
  ;; i.e. the source file was found, but not the location within.
  (define (check-non-bof-location file)
    (define ht (make-hash))
    (define (find k) ;memoized find-definition how
      (hash-ref ht k
                (λ ()
                  (define v (find-definition how (format "~a" k)))
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
    (check-non-bof-location (build-path parent-dir file))))


;; Exercise "how" = 'namespace
(define-namespace-anchor nsa)
(parameterize ([current-namespace (namespace-anchor->namespace nsa)])
  (test 'namespace))

;; Exercise "how" = a specific file
(define this-file (path->string (syntax-source #'here)))
(file->expanded-syntax this-file
                       (λ (_stx)
                         (test this-file)))

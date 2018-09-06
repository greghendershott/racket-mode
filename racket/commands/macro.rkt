#lang racket/base

(require racket/contract
         racket/file
         racket/format
         racket/match
         racket/pretty
         racket/system
         "../elisp.rkt"
         "../syntax.rkt"
         "../util.rkt")

(provide macro-stepper
         macro-stepper/next)

(define step-thunk/c (-> (cons/c (or/c 'original string? 'final) string?)))
(define step-thunk #f)

(define/contract (make-expr-stepper str)
  (-> string? step-thunk/c)
  (define step-num #f)
  (define last-stx (string->namespace-syntax str))
  (define (step)
    (cond [(not step-num)
           (set! step-num 0)
           (cons 'original (pretty-format-syntax last-stx))]
          [else
           (define this-stx (expand-once last-stx))
           (cond [(not (equal? (syntax->datum last-stx)
                               (syntax->datum this-stx)))
                  (begin0
                      (cons (~a step-num ": expand-once")
                            (diff-text (pretty-format-syntax last-stx)
                                       (pretty-format-syntax this-stx)
                                       #:unified 3))
                    (set! last-stx this-stx))]
                 [else
                  (cons 'final (pretty-format-syntax this-stx))])]))
  step)

(define/contract (make-file-stepper path into-base?)
  (-> (and/c path-string? absolute-path?) boolean? step-thunk/c)
  ;; If the dynamic-require fails, just let it bubble up.
  (define stepper-text (dynamic-require 'macro-debugger/stepper-text 'stepper-text))
  (define stx (file->syntax path))
  (define-values (dir _name _dir) (split-path path))
  (define raw-step (parameterize ([current-load-relative-directory dir])
                     (stepper-text stx
                                   (if into-base? (λ _ #t) (not-in-base)))))
  (define step-num #f)
  (define step-last-after "")
  (define/contract (step) step-thunk/c
    (cond [(not step-num)
           (set! step-num 0)
           (cons 'original
                 (pretty-format-syntax stx))]
          [else
           (define out (open-output-string))
           (parameterize ([current-output-port out])
             (cond [(raw-step 'next)
                    (set! step-num (add1 step-num))
                    (match-define (list title before after)
                      (step-parts (get-output-string out)))
                    (set! step-last-after after)
                    (cons (~a step-num ": " title)
                          (diff-text before after #:unified 3))]
                   [else
                    (cons 'final step-last-after)]))]))
  step)

(define/contract (macro-stepper what into-base?)
  (-> (or/c (cons/c 'expr string?) (cons/c 'file path-string?)) elisp-bool/c
      (cons/c 'original string?))
  (set! step-thunk
        (match what
          [(cons 'expr str)  (make-expr-stepper str)]
          [(cons 'file path) (make-file-stepper path (as-racket-bool into-base?))]))
  (macro-stepper/next))

(define/contract (macro-stepper/next)
  (-> (cons/c (or/c 'original 'final string?) string?))
  (unless step-thunk
    (error 'macro-stepper "Nothing to expand"))
  (define v (step-thunk))
  (when (eq? 'final (car v))
    (set! step-thunk #f))
  v)

;; Borrowed from xrepl.
(define not-in-base
  (λ () (let ([base-stxs #f])
          (unless base-stxs
            (set! base-stxs ; all ids that are bound to a syntax in racket/base
                  (parameterize ([current-namespace (make-base-namespace)])
                    (let-values ([(vals stxs) (module->exports 'racket/base)])
                      (map (λ (s) (namespace-symbol->identifier (car s)))
                           (cdr (assq 0 stxs)))))))
          (λ (id) (not (ormap (λ (s) (free-identifier=? id s)) base-stxs))))))

(define (step-parts str)
  (match str
    [(pregexp "^(.+?)\n(.+?)\n +==>\n(.+?)\n+$"
              (list _ title before after))
     (list title before after)]))

(define (diff-text before-text after-text #:unified [-U 3])
  (define template "racket-mode-syntax-diff-~a")
  (define (make-temporary-file-with-text str)
    (define file (make-temporary-file template))
    (with-output-to-file file #:mode 'text #:exists 'replace
      (λ () (displayln str)))
    file)
  (define before-file (make-temporary-file-with-text before-text))
  (define after-file  (make-temporary-file-with-text after-text))
  (define out (open-output-string))
  (begin0 (parameterize ([current-output-port out])
            (system (format "diff -U ~a ~a ~a" -U before-file after-file))
            (match (get-output-string out)
              ["" " <empty diff>\n"]
              [(pregexp "\n(@@.+@@\n.+)$" (list _ v)) v]))
    (delete-file before-file)
    (delete-file after-file)))

(define (pretty-format-syntax stx)
  (pretty-format #:mode 'write (syntax->datum stx)))

#lang racket/base

(require (only-in macro-debugger/stepper-text
                  stepper-text)
         racket/contract
         racket/file
         racket/format
         racket/match
         (only-in racket/path
                  path-only)
         racket/pretty
         racket/system
         "../elisp.rkt"
         "../repl-session.rkt"
         "../syntax.rkt"
         "../util.rkt")

(provide macro-stepper
         macro-stepper/next)

(define step/c (cons/c (or/c 'original string? 'final) string?))
(define step-proc/c (-> (or/c 'next 'all) (listof step/c)))
(define step-proc #f)

(define/contract (make-expr-stepper str)
  (-> string? step-proc/c)
  (unless (current-session-id)
    (error 'make-expr-stepper "Does not work without a running REPL"))
  (define step-num #f)
  (define last-stx (string->namespace-syntax str))
  (define/contract (step what) step-proc/c
    (cond [(not step-num)
           (set! step-num 0)
           (list (cons 'original
                       (pretty-format-syntax last-stx)))]
          [else
           (define result
             (let loop ()
               (define this-stx (expand-once last-stx))
               (cond [(equal? (syntax->datum last-stx)
                              (syntax->datum this-stx))
                      (cond [(eq? what 'all)
                             (list (cons 'final
                                         (pretty-format-syntax this-stx)))]
                            [else (list)])]
                     [else
                      (set! step-num (add1 step-num))
                      (define step
                        (cons (~a step-num ": expand-once")
                              (diff-text (pretty-format-syntax last-stx)
                                         (pretty-format-syntax this-stx)
                                         #:unified 3)))
                      (set! last-stx this-stx)
                      (cond [(eq? what 'all) (cons step (loop))]
                            [else (list step)])])))
           (match result
             [(list) (list (cons 'final
                                 (pretty-format-syntax last-stx)))]
             [v v])]))
  step)

(define/contract (make-file-stepper path into-base?)
  (-> (and/c path-string? absolute-path?) boolean? step-proc/c)
  (assert-file-stepper-works)
  (define stx (file->syntax path))
  (define dir (path-only path))
  (define ns (make-base-namespace))
  (define raw-step (parameterize ([current-load-relative-directory dir]
                                  [current-namespace               ns])
                     (stepper-text stx
                                   (if into-base? (λ _ #t) (not-in-base)))))
  (define step-num #f)
  (define step-last-after "")
  (log-racket-mode-debug "~v ~v ~v" path into-base? raw-step)
  (define/contract (step what) step-proc/c
    (cond [(not step-num)
           (set! step-num 0)
           (list (cons 'original
                       (pretty-format-syntax stx)))]
          [else
           (define out (open-output-string))
           (cond [(parameterize ([current-output-port out])
                    (raw-step what))
                  (log-racket-mode-debug "~v" (get-output-string out))
                  (define in (open-input-string (get-output-string out)))
                  (let loop ()
                    (match (parameterize ([current-input-port in])
                             (read-step))
                      [(? eof-object?)
                       (cond [(eq? what 'all)
                              (list (cons 'final step-last-after))]
                             [else (list)])]
                      [(list title before after)
                       (set! step-num (add1 step-num))
                       (set! step-last-after after)
                       (cons (cons (~a step-num ": " title)
                                   (diff-text before after #:unified 3))
                             (loop))]))]
                 [else
                  (list (cons 'final step-last-after))])]))
  step)

(define (read-step)
  (define title (read-line))
  (define before (read))
  (define _arrow (read)) ; '==>
  (define after (read))
  (read-line)
  (match (read-line)
    [(? eof-object? e) e]
    [_ (list title
            (pretty-format #:mode 'write before)
            (pretty-format #:mode 'write  after))]))

(define (assert-file-stepper-works)
  (define step (stepper-text #'(module example racket/base 42)))
  (unless (step 'next)
    (error 'macro-debugger/stepper-text
           "does not work in your version of Racket.\nPlease try an older or newer version.")))

(define/contract (macro-stepper what into-base?)
  (-> (or/c (cons/c 'expr string?) (cons/c 'file path-string?)) elisp-bool/c
      (list/c step/c))
  (set! step-proc
        (match what
          [(cons 'expr str)  (make-expr-stepper str)]
          [(cons 'file path) (make-file-stepper path (as-racket-bool into-base?))]))
  (macro-stepper/next 'next))

(define/contract (macro-stepper/next what) step-proc/c
  (unless step-proc
    (error 'macro-stepper "Nothing to expand"))
  (define v (step-proc what))
  (match v
    [(list (cons 'final _)) (set! step-proc #f)]
    [_ (void)])
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

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (only-in macro-debugger/stepper-text
                  stepper-text)
         (only-in macro-debugger/model/hiding-policies
                  policy->predicate)
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

(define (nothing-step-proc _) null)

(define step-proc nothing-step-proc)

(define/contract (macro-stepper path expression-str hiding-policy)
  (-> (and/c path-string? complete-path?) any/c any/c
      (list/c step/c))
  (assert-macro-debugger-stepper-works)
  (define-values (stx ns)
    (cond
      [(string? expression-str)
       (unless (current-session-id)
         (error 'macro-stepper "Does not work without a running REPL"))
       (values (string->namespace-syntax expression-str)
               (current-namespace))]
      [else
       (values (file->syntax path)
               (make-base-namespace))]))
  (set! step-proc
        (make-stepper path stx ns hiding-policy))
  (macro-stepper/next 'next))

(define/contract (macro-stepper/next what) step-proc/c
  (define v (step-proc what))
  (match v
    [(list (cons 'final _)) (set! step-proc nothing-step-proc)]
    [_ (void)])
  v)

(define/contract (make-stepper path stx ns elisp-hiding-policy)
  (-> (and/c path-string? complete-path?) syntax? namespace? any/c
      step-proc/c)
  (define dir (path-only path))
  (define policy (elisp-policy->policy elisp-hiding-policy))
  (define predicate (policy->predicate policy))
  (define raw-step (parameterize ([current-load-relative-directory dir]
                                  [current-namespace               ns])
                     (stepper-text stx predicate)))
  (define step-num #f)
  (define step-last-after (pretty-format-syntax stx))
  (log-racket-mode-debug "~v ~v ~v" path policy raw-step)
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

(define (elisp-policy->policy e)
  ;; See macro-debugger/model/hiding-policies.rkt):
  ;;
  ;; A Policy is one of
  ;;   'disable
  ;;   'standard
  ;;   (list 'custom boolean boolean boolean boolean (listof Entry))
  ;;
  ;; Of the Entry rules, although the free=? one can't work because it
  ;; needs a live syntax object identifier, I think most of the rest
  ;; should be fine.
  (match e
    [(or 'disable 'standard) e]
    [(list (app as-racket-bool hide-racket?)
           (app as-racket-bool hide-libs?)
           (app as-racket-bool hide-contracts?)
           (app as-racket-bool hide-phase1?)
           rules)
     (list 'custom hide-racket? hide-libs? hide-contracts? hide-phase1? rules)]))

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
  (dynamic-wind
    void
    (λ ()
      (parameterize ([current-output-port out])
        (system (format "diff -U ~a ~a ~a" -U before-file after-file))
        (match (regexp-replace* #rx"\r\n" ;#598
                                (get-output-string out)
                                "\n")
          ["" " <empty diff>\n"]
          [(pregexp "\n(@@.+@@\n.+)$" (list _ v)) v])))
    (λ ()
      (delete-file before-file)
      (delete-file after-file))))

(define (pretty-format-syntax stx)
  (pretty-format #:mode 'write (syntax->datum stx)))

(define (assert-macro-debugger-stepper-works)
  (define step (stepper-text #'(module example racket/base 42)))
  (unless (step 'next)
    (error 'macro-debugger/stepper-text
           "does not work in your version of Racket.\nPlease try an older or newer version.")))

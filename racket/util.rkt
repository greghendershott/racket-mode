#lang racket/base

(require (for-syntax racket/base)
         syntax/stx
         syntax/parse/define
         racket/format
         (only-in racket/path
                  filename-extension
                  some-system-path->string))

(provide fresh-line
         zero-column!
         display-commented
         string->namespace-syntax
         syntax-or-sexpr->syntax
         syntax-or-sexpr->sexpr
         nat/c
         pos/c
         inc!
         memq?
         in-syntax
         log-racket-mode-debug
         log-racket-mode-info
         log-racket-mode-warning
         log-racket-mode-error
         log-racket-mode-fatal
         time-apply/log
         with-time/log
         define-polyfill
         path-has-extension?
         path-replace-extension
         some-system-path->string)

;; Issue a newline unless already in column zero. Assumes
;; port-count-lines! already applied to current-output-port.
(define (fresh-line)
  (flush-output)
  (define-values [_line col _pos] (port-next-location (current-output-port)))
  (unless (eq? col 0) (newline)))

(define (zero-column!)
  (define-values [line col pos] (port-next-location (current-output-port)))
  (set-port-next-location! (current-output-port) line 0 (- pos col)))

(define (display-commented str)
  (fresh-line)
  (printf "; ~a\n"
          (regexp-replace* "\n" str "\n; ")))

(define (string->namespace-syntax str)
  (namespace-syntax-introduce
   (read-syntax #f (open-input-string str))))

(define (syntax-or-sexpr->syntax v)
  (if (syntax? v)
      v
      (namespace-syntax-introduce (datum->syntax #f v))))

(define (syntax-or-sexpr->sexpr v)
  (if (syntax? v)
      (syntax-e v)
      v))

(define nat/c exact-nonnegative-integer?)
(define pos/c exact-positive-integer?)

(define-simple-macro (inc! v:id)
  (set! v (add1 v)))

(define (memq? x xs)
  (and (memq x xs) #t))

;;; in-syntax: Not defined until Racket 6.3

(define-sequence-syntax in-syntax
  (λ () #'in-syntax/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(id) (_ arg)]
       #'[(id) (in-list (in-syntax/proc arg))]])))

(define (in-syntax/proc stx)
  (or (stx->list stx)
      (raise-type-error 'in-syntax "stx-list" stx)))

;;; logger / timing

(define-logger racket-mode)

(define (time-apply/log what proc args)
  (define-values (vs cpu real gc) (time-apply proc args))
  (define (fmt n) (~v #:align 'right #:min-width 4 n))
  (log-racket-mode-debug "~a cpu | ~a real | ~a gc <= ~a"
                         (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

;;; Path extension for Racket versions < 6.6

(define-simple-macro (define-polyfill (id:id arg:expr ...)
                       #:module mod:id
                       body:expr ...+)
  (define id
    (with-handlers ([exn:fail? (λ (_exn)
                                 (λ (arg ...) body ...))])
      (dynamic-require 'mod 'id))))

(define-polyfill (path-has-extension? path ext)
  #:module racket/path
  (let ([ext (if (string? ext) (string->bytes/utf-8 ext) ext)])
    (equal? (filename-extension path) ext)))

(define-polyfill (path-replace-extension path ext)
  #:module racket/path
  (path-replace-suffix path ext))

(module+ test
  (require rackunit)
  (check-true (path-has-extension? "/path/to/foo.EXT" "EXT"))
  (check-true (path-has-extension? (build-path "/path/to/foo.EXT") "EXT"))
  (check-equal? (path-replace-extension "/path/to/foo.OLD" ".NEW")
                (build-path "/path/to/foo.NEW"))
  (check-equal? (path-replace-extension (build-path "/path/to/foo.OLD") ".NEW")
                (build-path "/path/to/foo.NEW")))

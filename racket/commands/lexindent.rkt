#lang racket/base

(require racket/match
         racket/pretty
         (rename-in "../sexp-indent.rkt"
                    [indent-amount se:indent-amount])
         (rename-in "../token-map.rkt"
                    [create tm:create]
                    [update tm:update]
                    [tokens tm:tokens]
                    [classify tm:classify])
         "../util.rkt")

;; Just a shim to use token-maps from Emacs Lisp.

(provide lexindent)

(define next-id 0)
(define ht (make-hash)) ;id => token-map?

(define (lexindent . args)
  (begin0
      (match args
        [`(create ,s) (create s)]
        [`(delete ,id) (delete id)]
        [`(update ,id ,pos ,old-len ,str) (update id pos old-len str)]
        [`(indent-amount ,id ,pos) (indent-amount id pos)]
        [`(classify ,id ,pos) (classify id pos)]
        ;; really just for logging/debugging `update`s
        [`(show ,id) (show id)])
    #;
    (match args
      [(list* (or 'create 'delete) _) (void)]
      [(list* _ id _) (log-racket-mode-debug "~v" (hash-ref ht id))])))

(define (create s)
  (set! next-id (add1 next-id))
  (define tm (tm:create s))
  (hash-set! ht next-id tm)
  (cons next-id (tokens-as-elisp tm 1 +inf.0)))

(define (delete id)
  (hash-remove! ht id))

(define (update id pos old-len str)
  (define tm (hash-ref ht id))
  (begin ;with-time/log "tm:update"
    (map token->elisp (tm:update tm pos old-len str))))

(define (indent-amount id pos)
  (define tm (hash-ref ht id))
  (se:indent-amount tm pos))

(define (classify id pos)
  (define tm (hash-ref ht id))
  (token->elisp (tm:classify tm pos)))

;; provided really just for logging/debugging `update`s
(define (show id)
  (define tm (hash-ref ht id))
  (log-racket-mode-debug "~a" (pretty-format tm))
  #f)

(define (tokens-as-elisp tm beg end)
  (tm:tokens tm beg end token->elisp))

(define (token->elisp b+t)
  (match-define (bounds+token beg end t) b+t)
  (match t
    [(? token:expr:open? t)  (list beg end 'open               (token:expr-close t))]
    [(? token:expr:close? t) (list beg end 'close              (token:expr-open t))]
    [(? token:misc? t)       (list beg end (token:misc-kind t) #f)]))

(module+ example-0
  (define str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar")
  (match-define (cons id vs) (lexindent 'create str))
  vs
  (lexindent 'update id 14 2 "9999")
  (lexindent 'classify id 14)
  (lexindent 'update id 14 4 "")
  (lexindent 'classify id 14)
  (lexindent 'classify id 15))

(module+ example-1
  (define str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar")
  (match-define (cons id vs) (lexindent 'create str))
  vs
  (lexindent 'classify id (sub1 (string-length str))))

(module+ example-2
  (define str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword")
  (match-define (cons id vs) (lexindent 'create str))
  vs
  (lexindent 'classify id (sub1 (string-length str))))

(module+ example-3
  (define str "#lang racket\n(λ () #t)")
  (match-define (cons id vs) (lexindent 'create str))
  vs
  (lexindent 'classify id 14)
  (lexindent 'classify id (sub1 (string-length str))))

(module+ example-4
  (define str "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")
  (match-define (cons id vs) (lexindent 'create str))
  vs)

(module+ example-5
  (define str "#lang racket\n123\n(print 123)\n")
  ;;           1234567890123 4567 890123456789 0
  ;;                    1           2          3
  (match-define (cons id _vs) (lexindent 'create str))
  (hash-ref ht id)
  (indent-amount id 18)
  (update id 28 0 "\n")
  (hash-ref ht id)
  (indent-amount id 29))

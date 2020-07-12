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

;; Just a shim to use lexers, token-maps, and indenters, from Emacs
;; Lisp.

(provide lexindent)

(define (lexindent . args)
  (begin0
      (match args
        [`(create ,id ,s) (create id s)]
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

(struct lexindenter (tm indent) #:transparent)
(define ht (make-hash)) ;id => lexindenter?

(define (create id s)
  (define tm (tm:create s))

  (define get-info (or (read-language (open-input-string s)
                                      (λ () #f))
                       (λ (_key default) default)))
  (define indent (get-info 'indent-amount se:indent-amount))

  (hash-set! ht id (lexindenter tm indent))
  (tokens-as-elisp tm 1 +inf.0))

(define (delete id)
  (hash-remove! ht id))

(define (update id pos old-len str)
  (match-define (lexindenter tm _) (hash-ref ht id))
  (begin ;with-time/log "tm:update"
    (map token->elisp (tm:update tm pos old-len str))))

(define (indent-amount id pos)
  (match-define (lexindenter tm proc) (hash-ref ht id))
  (proc tm pos))

(define (classify id pos)
  (match-define (lexindenter tm _) (hash-ref ht id))
  (token->elisp (tm:classify tm pos)))

;; provided really just for logging/debugging `update`s
(define (show id)
  (match-define (lexindenter tm _) (hash-ref ht id))
  (local-require (submod "../token-map.rkt" test))
  (check-valid? tm)
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
  (define id 0)
  (define str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar")
  (lexindent 'create id str)
  (lexindent 'update id 14 2 "9999")
  (lexindent 'classify id 14)
  (lexindent 'update id 14 4 "")
  (lexindent 'classify id 14)
  (lexindent 'classify id 15))

(module+ example-1
  (define id 0)
  (define str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar")
  (lexindent 'create id str)
  (lexindent 'classify id (sub1 (string-length str))))

(module+ example-2
  (define id 0)
  (define str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword")
  (lexindent 'create id str)
  (lexindent 'classify id (sub1 (string-length str))))

(module+ example-3
  (define id 0)
  (define str "#lang racket\n(λ () #t)")
  (lexindent 'create id str)
  (lexindent 'classify id 14)
  (lexindent 'classify id (sub1 (string-length str))))

(module+ example-4
  (define id 0)
  (define str "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")
  (lexindent 'create id str))

(module+ example-5
  (define id 0)
  (define str "#lang racket\n123\n(print 123)\n")
  ;;           1234567890123 4567 890123456789 0
  ;;                    1           2          3
  (lexindent 'create id str)
  (hash-ref ht id)
  (indent-amount id 18)
  (update id 28 0 "\n")
  (hash-ref ht id)
  (indent-amount id 29))
#lang racket/base

(require racket/match
         (rename-in "../sexp-indent.rkt"
                    [indent-line se:indent-line])
         (rename-in "../token-map.rkt"
                    [create tm:create]
                    [update tm:update]
                    [tokens tm:tokens]
                    [classify tm:classify]))

;; Just a shim to use token-maps from Emacs Lisp.

(provide lexindent)

(define next-id 0)
(define ht (make-hash)) ;id => token-map?

(define (lexindent . args)
  (match args
    [`(create ,s) (create s)]
    [`(delete ,id) (delete id)]
    [`(update ,id ,(app sub1 pos) ,old-len ,after) (update id pos old-len after)]
    [`(classify ,id ,pos) (classify id pos)]
    [`(indent-line ,id ,pos) (indent-line id pos)]))

(define (create s)
  (set! next-id (add1 next-id))
  (define tm (tm:create s))
  (hash-set! ht next-id tm)
  (cons next-id (tokens-as-elisp tm 1)))

(define (delete id)
  (hash-remove! ht id))

(define (update id pos old-len after)
  (define tm (hash-ref ht id))
  (define start (tm:update tm pos old-len after))
  (tokens-as-elisp tm start))

(define (classify id pos)
  (define tm (hash-ref ht id))
  (token->elisp (tm:classify tm pos)))

(define (indent-line id pos)
  (define tm (hash-ref ht id))
  (se:indent-line tm pos))

(define (tokens-as-elisp tm pos)
  (tm:tokens tm pos token->elisp))

(define (token->elisp t)
  (match t
    [(token:expr:open beg end _backup _open close)  (list beg end 'open close)]
    [(token:expr:close beg end _backup open _close) (list beg end 'close open)]
    [(token:misc beg end _backup kind)              (list beg end kind #f)]))

(module+ example-0
  (require racket/pretty)
  (define str "#lang racket
               42
               (print \"hello\")
               @print{Hello}
               'foo #:bar")
  (match-define (cons id vs) (lexindent 'create str))
  (pretty-print vs)
  (pretty-print (lexindent 'update id 25 1 "J"))
  (pretty-print (lexindent 'classify id (sub1 (string-length str))))
  ;(lexindent 'delete id)
  )

(module+ example-1
  (require racket/pretty)
  (define str "#lang at-exp racket
               42
               (print \"hello\")
               @print{Hello (there)}
               'foo #:bar")
  (match-define (cons id vs) (lexindent 'create str))
  (pretty-print vs)
  (pretty-print (lexindent 'classify id (sub1 (string-length str))))
  ;(lexindent 'delete id)
  )

(module+ example-2
  (require racket/pretty)
  (define str "#lang scribble/text
               Hello
               @(print \"hello\")
               @print{Hello (there)}
               #:not-a-keyword")
  (match-define (cons id vs) (lexindent 'create str))
  (pretty-print vs)
  (pretty-print (lexindent 'classify id (sub1 (string-length str))))
  ;(lexindent 'delete id)
  )

(module+ example-3
  (require racket/pretty)
  (define str "#lang racket\n(λ () #t)")
  (match-define (cons id vs) (lexindent 'create str))
  (pretty-print vs)
  (pretty-print (lexindent 'classify id 14))
  (pretty-print (lexindent 'classify id (sub1 (string-length str))))
  ;(lexindent 'delete id)
  )

(module+ example-4
  (require racket/pretty)
  (define str "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")
  (match-define (cons id vs) (lexindent 'create str))
  (pretty-print vs)
  ;(lexindent 'delete id)
  )

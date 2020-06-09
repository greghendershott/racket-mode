#lang racket/base

(require data/interval-map
         racket/format
         racket/match
         racket/pretty
         syntax-color/module-lexer
         "../util.rkt")

(provide lexindent)

(define next-id 0)
(struct lexindenter (str im) #:transparent)
(define ht (make-hash)) ;id => lexindenter

;; The command uses Emacs 1-based positions, but internally we use
;; 0-based.
(define (lexindent . args)
  (match args
    [`(create ,s)
     (set! next-id (add1 next-id))
     (define im (make-interval-map))
     (hash-set! ht next-id (lexindenter s im))
     (tokenize! im s 0 #f)
     (cons next-id (lexemes im 1))]
    [`(delete ,id)
     (hash-remove! ht id)]
    [`(update ,id ,(app sub1 pos) ,old-len ,after)
     (match-define (lexindenter old-str im) (hash-ref ht id))
     (define str (string-append (substring old-str 0 pos)
                                after
                                (substring old-str
                                           (+ pos old-len)
                                           (string-length old-str))))
     (hash-set! ht id (lexindenter str im))
     (define start (get-start-maybe-backup im (add1 pos)))
     (interval-map-remove! im start +inf.0)
     (tokenize! im str start #f)
     (lexemes im start)]
    [`(classify ,id ,pos)
     (match-define (lexindenter _str im) (hash-ref ht id))
     (define-values (beg end vs) (interval-map-ref/bounds im pos #f))
     (cond [(and beg end vs) (list* beg end (cdr vs))]
           [else (list pos pos 'error #f)])]))

(define (get-start-maybe-backup im pos)
  (define-values (beg _end vs) (interval-map-ref/bounds im pos #f))
  (cond [(and beg vs)
         (define backup (car vs))
         (- beg backup)]
        [else pos]))

(define (lexemes im pos)
  (define-values (beg end vs) (interval-map-ref/bounds im pos #f))
  (cond [(and beg end vs)
         (match-define (list _backup kind opposite) vs)
         (cons (list beg end kind opposite)
               (lexemes im end))]
        [else '()]))

(define (tokenize! im str offset [mode #f])
  (define in (open-input-string str))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (-tokenize! im in offset mode))

(define (-tokenize! im in offset mode)
  (define-values (lexeme kind delimit beg end backup new-mode)
    (module-lexer in offset mode))
  (cond [(eof-object? lexeme) im]
        [else
         (define-values (adjusted-kind opposite)
           (if (eq? kind 'parenthesis)
               (match delimit
                 ['|(| (values 'open ")")]
                 ['|[| (values 'open "]")]
                 ['|{| (values 'open "}")]
                 ['|)| (values 'close "(")]
                 ['|]| (values 'close "[")]
                 ['|}| (values 'close "{")]
                 [_ (match lexeme
                      ["(" (values 'open ")")]
                      [")" (values 'close "(")]
                      ;; I have seen this with e.g. scribble/text for #\@
                      [_ (values 'symbol #f)])])
               (values kind #f)))
         (interval-map-set! im beg end (list backup adjusted-kind opposite))
         (-tokenize! im in end new-mode)]))

(module+ example-0
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
  (define str "#lang racket\n(λ () #t)")
  (match-define (cons id vs) (lexindent 'create str))
  (pretty-print vs)
  (pretty-print (lexindent 'classify id (sub1 (string-length str))))
  ;(lexindent 'delete id)
  )

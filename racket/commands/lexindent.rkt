#lang racket/base

(require (only-in data/interval-map
                  make-interval-map
                  interval-map-set!
                  interval-map-ref
                  interval-map-remove!)
         racket/match
         syntax-color/module-lexer
         "../util.rkt")

(provide lexindent)

;; Because interval-map-ref/bounds is not available in older Racket,
;; we redundantly store the interval in the value as the beg end
;; members.
(struct token (beg end backup) #:transparent)
(struct misc-token token (kind) #:transparent)
(struct delim-token token (open close) #:transparent)
(struct open-token delim-token () #:transparent)
(struct close-token delim-token () #:transparent)

(struct lexindenter (str im) #:transparent)

(define next-id 0)
(define ht (make-hash)) ;id => lexindenter

(define (lexindent . args)
  (match args
    [`(create ,s) (create s)]
    [`(delete ,id) (delete id)]
    [`(update ,id ,(app sub1 pos) ,old-len ,after) (update id pos old-len after)]
    [`(classify ,id ,pos) (classify id pos)]))

(define (create s)
  (set! next-id (add1 next-id))
  (define im (make-interval-map))
  (hash-set! ht next-id (lexindenter s im))
  (tokenize-string im s 0 #f)
  (cons next-id (tokens-as-elisp im 1)))

(define (delete id)
  (hash-remove! ht id))

(define (update id pos old-len after)
  (match-define (lexindenter old-str im) (hash-ref ht id))
  (define str (string-append (substring old-str 0 pos)
                             after
                             (substring old-str
                                        (+ pos old-len)
                                        (string-length old-str))))
  (hash-set! ht id (lexindenter str im))
  (define start (match (interval-map-ref im pos #f)
                  [(token beg _end backup) (- beg backup)]
                  [_                       pos]))
  (interval-map-remove! im start +inf.0)
  (tokenize-string im str start #f)
  (tokens-as-elisp im start))

(define (classify id pos)
  (match-define (lexindenter _str im) (hash-ref ht id))
  (token->elisp (interval-map-ref im pos)))

(define (tokens-as-elisp im pos)
  (match (interval-map-ref im pos #f)
    [(? token? t) (cons (token->elisp t)
                        (tokens-as-elisp im (token-end t)))]
    [_ '()]))

(define (token->elisp t)
  (match t
    [(misc-token beg end _backup kind)         (list beg end kind #f)]
    [(open-token beg end _backup _open close)  (list beg end 'open close)]
    [(close-token beg end _backup open _close) (list beg end 'close open)]))

(define (tokenize-string im str offset [mode #f])
  (define in (open-input-string str))
  (port-count-lines! in) ;important for Unicode e.g. λ
  (tokenize-port im in offset mode))

(define (tokenize-port im in offset mode)
  (define-values (lexeme kind delimit beg end backup new-mode)
    (module-lexer in offset mode))
  (cond [(eof-object? lexeme) im]
        [(eq? kind 'parenthesis)
         ;; Handling "parentheses" well helps in various ways:
         ;;
         ;; 1. Editor commands to navigate and modify sub-expressions
         ;; or "blocks". Things like backward-up-list, forward-sexp,
         ;; and things depending on those like paredit. This is
         ;; obvious in a sexpr lang, but can also work in C-like langs
         ;; that use {}, and even langs that use indent (either as a
         ;; shorthand for {} like Haskell, or not like Python).
         ;;
         ;; 2. A lang-provided indenter can use this information.
         ;;
         ;; I find the status quo lexer/c handling of this to be not
         ;; quite right: `delimit` is one of the chars "()[]{}", and,
         ;; it simply "restates" information from the lexeme. I think
         ;; the justification for this is that e.g. racket-lexer
         ;; handles "#(" as a single 2-char token, parenthesis "(".
         ;; [However it handles "#'(" as 2 tokens -- a 2-char "#'"
         ;; constant followed by a 1-char parenthesis "(". So...???]
         ;;
         ;; INSTEAD: I suggest instead of "parenthesis" tokens, the
         ;; lexer should return "open" and "close" tokens. Instead of
         ;; 'delimit' as in the status quo, there is a field stating
         ;; the opposite, matching close/open token's expected text.
         ;; That way a lang could specify expression/block delimiters
         ;; like "begin" and "end", the lexer could emit tokens like
         ;; "begin" "end") and "(close "end" "begin"), and an indenter
         ;; or editor can use these effectively. (I think this will
         ;; work even if multiple open tokens use the same end token
         ;; -- like when "then" and "else" both use "end", as opposed
         ;; to "elif" and "end" -- right?) (For off-side rule lexers,
         ;; presumably indent = open and dedent = close, and the
         ;; "opposite" value is N/A?)
         ;;
         ;; ---
         ;;
         ;; In Emacs, the standard char-syntax stuff is just that --
         ;; single chars. As a result it's helpful here to return
         ;; tokens for parens that are indeed single chars. Any
         ;; "excess" chars should be constant or perhaps Emacs 'prefix
         ;; syntax-class. The latter would be ideal for Racket Mode
         ;; and sexpr langs, but I'm not sure how clever to get here
         ;; b/c it could bake in some assumption that is /not/ good
         ;; for non-sepxr langs. So for now just accept "constant" for
         ;; "'", "#", or "#'".
         (define tok
           (match delimit
             ['|(| (open-token beg end backup "(" ")")]
             ['|[| (open-token beg end backup "[" "]")]
             ['|{| (open-token beg end backup "{" "}")]
             ['|)| (close-token beg end backup "(" ")")]
             ['|]| (close-token beg end backup "[" "]")]
             ['|}| (close-token beg end backup "{" "}")]
             [_ (match lexeme
                  ["(" (open-token beg end backup "(" ")")]
                  [")" (close-token beg end backup "(" ")")]
                  ;; I have seen this with e.g. scribble/text for #\@
                  [_ (misc-token beg end backup 'symbol)])]))
         ;; Weirdly, racket-lexer treats "#(" as a 2-char open paren
         ;; token -- but "#'(" as a 2-char constant followed by a
         ;; 1-char open paren. Handle the former like the latter, here
         ;; (so that Emacs front end can rely on the
         ;; syntax-propertization).
         (cond [(and (open-token? tok)
                     (< 1 (- end beg)))
                (interval-map-set! im beg (sub1 end)
                                   (misc-token beg (sub1 end) backup 'constant))
                (interval-map-set! im (sub1 end) end
                                   (open-token (sub1 end) end backup
                                               (delim-token-open tok)
                                               (delim-token-close tok)))]
               [else
                (interval-map-set! im beg end tok)])
         (tokenize-port im in end new-mode)]
        [else
         (interval-map-set! im beg end (misc-token beg end backup kind))
         (tokenize-port im in end new-mode)]))

(define (backward-up im pos count)
  (with-handlers ([exn:fail? (λ _ #f)])
    (let loop ([pos (match (interval-map-ref im pos)
                      ;; When already exactly on an open, back up
                      [(? open-token? t)
                       #:when (= pos (token-beg t))
                       (sub1 pos)]
                      [_ pos])]
               [count count])
      (match (interval-map-ref im pos)
        [(? open-token? t)
         (if (= count 1)
             (token-beg t)
             (loop (sub1 (token-beg t))
                   (sub1 count)))]
        [(? token? t)
         (loop (sub1 (token-beg t))
               count)]))))

(define (forward-whitespace/comment im pos)
  (match (interval-map-ref im pos)
    [(misc-token _beg end _backup (or 'white-space 'comment 'sexp-comment))
     (forward-whitespace/comment im end)]
    [_ pos]))

(define (forward-sexp im pos)
  (let ([pos (forward-whitespace/comment im pos)])
    (match (interval-map-ref im pos)
      ;; Open token: Scan for matching close token.
      [(? open-token? open-t)
       (let loop ([pos pos]
                  [depth 0])
         ;;(println (list pos depth (interval-map-ref im pos)))
         (match (interval-map-ref im pos)
           [(? open-token? t)
            #:when (equal? (delim-token-open open-t) (delim-token-open t))
            (loop (token-end t)
                  (add1 depth))]
           [(? close-token? t)
            #:when (equal? (delim-token-open open-t) (delim-token-open t))
            (if (= depth 1)
                (sub1 (token-end t))
                (loop (token-end t)
                      (sub1 depth)))]
           [(? token? t)
            (loop (token-end t)
                  depth)]))]
      ;; Some other token, e.g. symbol or string: Simply use last char pos.
      [(? token? t)
       (sub1 (token-end t))])))

(module+ test
  (require rackunit)
  (require racket/pretty)
  (define str "#lang racket\n(a (b (c  foo)))")
  ;;           1234567890123 4567890123456789
  ;;                    1          2
  (match-define (cons id vs) (lexindent 'create str))
  ht
  (match-define (lexindenter _str im) (hash-ref ht id))
  (check-equal? (backward-up im 22 1) 20)
  (check-equal? (backward-up im 22 3) 14)
  (check-false  (backward-up im 22 4))
  (check-equal? (backward-up im 20 1) 17)
  (check-equal? (forward-whitespace/comment im 22) 24)
  (check-equal? (forward-sexp im 24) 26)
  (check-equal? (forward-sexp im 14) 29))

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

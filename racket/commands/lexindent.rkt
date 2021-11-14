#lang racket/base

;; Bridge to Emacs Lisp to use lexers, token-maps, and indenters.

(require racket/async-channel
         racket/class
         racket/match
         "../token-map/private/core.rkt"
         "../util.rkt")

(provide lexindent
         token-notify-channel)

;; TODO:
;;
;; - Use drracket:quote-matches (?).
;;
;; - Default to module-lexer* not module-lexer?? IIUC the main difference
;;   is that token type can be a hash-table instead of a symbol.

(define (lexindent . args)
  (log-racket-mode-debug "~v" args)
  (match args
    [`(create ,id ,s)                             (create id s)]
    [`(delete ,id)                                (delete id)]
    [`(update ,id ,gen ,pos ,old-len ,str)        (update id gen pos old-len str)]
    [`(indent-amount ,id ,gen ,pos)               (indent-amount id gen pos)]
    [`(classify ,id ,gen ,pos)                    (classify id gen pos)]
    [`(grouping ,id ,gen ,pos ,dir ,limit ,count) (grouping id gen pos dir limit count)]))

(define token-notify-channel (make-async-channel))

(struct lexindenter (obj notify-rx-chan) #:transparent)
(define ht (make-hash)) ;id => lexindenter?
(define (get-object id) (lexindenter-obj (hash-ref ht id)))

(define (create id s) ;any/c string? -> void
  ;; We supply an async-channel to create that we receive here to
  ;; transform the values to Elisp, as well as attaching the `id` so
  ;; they can be distributed to the appropriate buffer, before sending
  ;; them on the token-notify-processor that the command server can
  ;; sync on just like it does for notify channels for logger and
  ;; debug.
  (define ch (make-async-channel))
  (thread
   (λ ()
     (let loop ()
       (match (async-channel-get ch)
         ['begin (loop)] ;ignore
         [(? bounds+token? b+t)
          (async-channel-put token-notify-channel
                             (list 'token id (token->elisp b+t)))
          (loop)]
         ['end (loop)] ;ignore
         ['quit (void)]))))
  (define obj (new hash-lang% [notify-chan ch]))
  (send obj update! 1 1 0 s)
  (hash-set! ht id (lexindenter obj ch)))

(define (delete id)
  (match (hash-ref ht id #f)
    [(lexindenter obj ch)
     (send obj delete)
     (async-channel-put ch 'quit) ;kill thread
     (hash-remove! ht id)]
    [#f (log-racket-mode-warning "delete lexindenter ~v: not found" id)]))

(define (update id gen pos old-len str)
  (with-time/log "tm:update"
    (send (get-object id) update! gen pos old-len str)))

(define (indent-amount id gen pos)
  (send (get-object id) indent-line-amount gen pos))

(define (classify id gen pos)
  (token->elisp (send (get-object id) classify gen pos)))

(define (grouping id gen pos dir limit count)
  (send (get-object id) grouping gen pos dir limit count))

(define (token->elisp b+t)
  (match-define (bounds+token beg end t) b+t)
  (list beg end (token-type t) (token-paren t)))

(module+ example-0
  (define id 0)
  (define str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar")
  (lexindent 'create id str)
  (lexindent 'update id 2 14 2 "9999")
  (lexindent 'classify id 2 14)
  (lexindent 'update id 3 14 4 "")
  (lexindent 'classify id 3 14)
  (lexindent 'classify id 3 15)
  (lexindent 'grouping id 3 15 'forward 0 1))

(module+ example-1
  (define id 0)
  (define str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar")
  (lexindent 'create id str)
  (lexindent 'classify id 1 (sub1 (string-length str))))

(module+ example-1.5
  (define id 0)
  (define str "#lang scribble/manual\n(print \"hello\")\n@print[#:kw 12]{Hello (there) #:not-a-keyword}\n")
  (lexindent 'create id str))

(module+ example-2
  (define id 0)
  (define str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword")
  (lexindent 'create id str)
  (lexindent 'classify id (sub1 (string-length str))))

(module+ example-3
  (define id 0)
  (define str "#lang racket\n(λ () #t)")
  (lexindent 'create id str)
  (lexindent 'classify id 1 14)
  (lexindent 'classify id 1 (sub1 (string-length str))))

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
  (indent-amount id 1 18)
  (update id 2 28 0 "\n")
  (indent-amount id 2 29))

#lang racket/base

;; Bridge to Emacs Lisp to use lexers, token-maps, and indenters.

(require racket/async-channel
         racket/match
         "../token-map/private/indent.rkt"
         (rename-in "../token-map/main.rkt"
                    [create tm:create]
                    [delete tm:delete]
                    [update! tm:update!]
                    [tokens tm:tokens]
                    [classify tm:classify]
                    [forward-sexp tm:forward-sexp]
                    [backward-sexp tm:backward-sexp])
         "../util.rkt")

(provide lexindent
         token-notify-channel)

;; TODO:
;;
;; - Consider the case where the #lang line changed -- that will
;; require re-reading things we got from get-info!
;;
;; - Separate indent-line and indent-region commands. Read docs
;; carefully wrt to case where both drracket:range-indent and :indent
;; are available.
;;
;; - Use drracket:quote-matches.
;;
;; - Use drracket:grouping-position.
;;
;; - Default to module-lexer* not module-lexer ?

(define (lexindent . args)
  (log-racket-mode-debug "~v" args)
  (match args
    [`(create ,id ,s)                      (create id s)]
    [`(delete ,id)                         (delete id)]
    [`(update ,id ,gen ,pos ,old-len ,str) (update id gen pos old-len str)]
    [`(indent-amount ,id ,gen ,pos)        (indent-amount id gen pos)]
    [`(classify ,id ,gen ,pos)             (classify id gen pos)]
    [`(forward-sexp ,id ,gen ,pos ,arg)    (forward-sexp id gen pos arg)]))

(define token-notify-channel (make-async-channel))

(struct lexindenter (token-map indent notify-rx-chan) #:transparent)
(define ht (make-hash)) ;id => lexindenter?

(define (create id s)
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
  (define tm (tm:create s ch))
  (define indenter (choose-indenter s tm))
  (hash-set! ht id (lexindenter tm indenter ch)))

(define (delete id)
  (match (hash-ref ht id #f)
    [(lexindenter tm _ ch)
     (tm:delete tm)
     (async-channel-put ch 'quit) ;kill thread
     (hash-remove! ht id)]
    [#f (log-racket-mode-warning "delete lexindenter ~v: not found" id)]))

(define (update id gen pos old-len str)
  (match-define (lexindenter tm _ _) (hash-ref ht id))
  (with-time/log "tm:update" (tm:update! tm gen pos old-len str)))

(define (indent-amount id gen pos)
  (match-define (lexindenter tm proc _) (hash-ref ht id))
  (line-amount proc tm gen pos #f))

(define (classify id gen pos)
  (match-define (lexindenter tm _ _) (hash-ref ht id))
  (token->elisp (tm:classify tm gen pos)))

(define (forward-sexp id gen pos arg)
  (match-define (lexindenter tm _ _) (hash-ref ht id))
  (define (fail pos) (list pos pos)) ;for signal scan-error
  (let loop ([pos pos]
             [arg arg])
    (cond [(zero? arg) pos]
          [(positive? arg)
           (match (tm:forward-sexp tm gen pos fail)
             [(? number? v) (loop v (sub1 arg))]
             [v v])]
          [(negative? arg)
           (match (tm:backward-sexp tm gen pos fail)
             [(? number? v) (loop v (add1 arg))]
             [v v])])))

(define (token->elisp b+t)
  (match-define (bounds+token beg end t) b+t)
  (match t
    [(? token:open? t)  (list beg end 'open               (token:open-close t))]
    [(? token:close? t) (list beg end 'close              (token:close-open t))]
    [(? token:misc? t)  (list beg end (token:misc-kind t) #f)]))


(define (choose-indenter string tm)
  (define get-info (or (with-handlers ([values (λ _ #f)])
                         (read-language (open-input-string string)
                                        (λ _ #f)))
                       (λ (_key default) default)))
  (indenter get-info))

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

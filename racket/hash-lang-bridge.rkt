#lang racket/base

;; Bridge to Emacs Lisp to use lexers, token-maps, and indenters.

(require racket/async-channel
         racket/class
         racket/match
         "hash-lang.rkt"
         "util.rkt")

(provide hash-lang
         token-notify-channel)

;; TODO:
;;
;; - Use drracket:quote-matches (?).
;;
;; - Default to module-lexer* not module-lexer?? IIUC the main difference
;;   is that token type can be a hash-table instead of a symbol.

(define (hash-lang . args)
  (unless hash-lang%
    (error "syntax-color/color-textoid not available; you need a newer version of Racket and/or syntax-color-lib"))
  (log-racket-mode-debug "~v" args)
  (match args
    [`(create ,id ,s)                              (create id s)]
    [`(delete ,id)                                 (delete id)]
    [`(update ,id ,gen ,pos ,old-len ,str)         (update id gen pos old-len str)]
    [`(indent-amount ,id ,gen ,pos)                (indent-amount id gen pos)]
    [`(indent-region-amounts ,id ,gen ,from ,upto) (indent-region-amounts id gen from upto)]
    [`(classify ,id ,gen ,pos)                     (classify id gen pos)]
    [`(grouping ,id ,gen ,pos ,dir ,limit ,count)  (grouping id gen pos dir limit count)]))

(define token-notify-channel (make-async-channel))

(struct obj+chan (obj notify-rx-chan) #:transparent)
(define ht (make-hash)) ;id => lexindenter?
(define (get-object id) (obj+chan-obj (hash-ref ht id)))

(define (create id s) ;any/c string? -> void
  ;; We supply an async-channel to create that we receive here to
  ;; transform the values to Elisp, as well as attaching the `id` so
  ;; they can be distributed to the appropriate buffer, before sending
  ;; them on the token-notify-processor that the command server can
  ;; sync on just like it does for notify channels for logger and
  ;; debug.
  (define ch (make-async-channel))
  (define (on-notify . args)
    (match args
      [(or 'begin 'end) (void)] ;ignore
      ['quit (async-channel-put ch 'quit)]
      ;; Produce a value convenient for Emacs to use as a notification.
      ;; Tokens of type 'parenthesis get extra data -- an open? flag
      ;; and the symbol for the matching open or close.
      ;; (or/c (list/c position/c position/c token?)
      ;;       (list/c position/c position/c token? boolean? string?))
      [(list paren-matches beg end token)
       (define ht-or-type (token-type token))
       (define type (if (symbol? ht-or-type)
                        ht-or-type
                        (hash-ref ht-or-type 'type 'unknown)))
       (define paren (token-paren token))
       (async-channel-put
        ch
        (list* beg
               end
               type
               (if paren
                   (or (for/or ([pm (in-list paren-matches)])
                         (match-define (list open close) pm)
                         (cond [(eq? paren open)
                                (list #t (symbol->string close))]
                               [(eq? paren close)
                                (list #f (symbol->string open))]
                               [else #f]))
                       null)
                   null)))]))
  (thread
   (λ ()
     (let loop ()
       (match (async-channel-get ch)
         [(? list? v)
          (log-racket-mode-debug "~v" v)
          (async-channel-put token-notify-channel
                             (list 'token id v))
          (loop)]
         ['quit (void)]))))
  (define obj (new hash-lang% [on-notify on-notify]))
  (hash-set! ht id (obj+chan obj ch))
  (send obj update! 1 1 0 s))

(define (delete id)
  (match (hash-ref ht id #f)
    [(obj+chan obj ch)
     (send obj delete)
     (async-channel-put ch 'quit) ;kill thread
     (hash-remove! ht id)]
    [#f (log-racket-mode-warning "delete lexindenter ~v: not found" id)]))

(define (update id gen pos old-len str)
  (with-time/log "tm:update"
    (send (get-object id) update! gen pos old-len str)))

(define (indent-amount id gen pos)
  (send (get-object id) indent-line-amount gen pos))

(define (indent-region-amounts id gen from upto)
  (send (get-object id) indent-region-amounts gen from upto))

(define (classify id gen pos)
  (match-define (list beg end tok) (send (get-object id) classify gen pos))
  (list beg end (token-type tok) (token-paren tok)))

(define (grouping id gen pos dir limit count)
  (send (get-object id) grouping gen pos dir limit count))

(module+ example-0
  (define id 0)
  (define str "#lang racket\n42 (print \"hello\") @print{Hello} 'foo #:bar")
  (hash-lang 'create id str)
  (hash-lang 'update id 2 14 2 "9999")
  (hash-lang 'classify id 2 14)
  (hash-lang 'update id 3 14 4 "")
  (hash-lang 'classify id 3 14)
  (hash-lang 'classify id 3 15)
  (hash-lang 'grouping id 3 15 'forward 0 1))

(module+ example-1
  (define id 0)
  (define str "#lang at-exp racket\n42 (print \"hello\") @print{Hello (there)} 'foo #:bar")
  (hash-lang 'create id str)
  (hash-lang 'classify id 1 (sub1 (string-length str))))

(module+ example-1.5
  (define id 0)
  (define str "#lang scribble/manual\n(print \"hello\")\n@print[#:kw 12]{Hello (there) #:not-a-keyword}\n")
  (hash-lang 'create id str))

(module+ example-2
  (define id 0)
  (define str "#lang scribble/text\nHello @(print \"hello\") @print{Hello (there)} #:not-a-keyword")
  (hash-lang 'create id str)
  (hash-lang 'classify id (sub1 (string-length str))))

(module+ example-3
  (define id 0)
  (define str "#lang racket\n(λ () #t)")
  (hash-lang 'create id str)
  (hash-lang 'classify id 1 14)
  (hash-lang 'classify id 1 (sub1 (string-length str))))

(module+ example-4
  (define id 0)
  (define str "#lang racket\n#rx\"1234\"\n#(1 2 3)\n#'(1 2 3)")
  (hash-lang 'create id str))

(module+ example-5
  (define id 0)
  (define str "#lang racket\n123\n(print 123)\n")
  ;;           1234567890123 4567 890123456789 0
  ;;                    1           2          3
  (hash-lang 'create id str)
  (indent-amount id 1 18)
  (update id 2 28 0 "\n")
  (indent-amount id 2 29))

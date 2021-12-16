#lang racket/base

(require racket/async-channel
         racket/class
         racket/match
         "util.rkt")

(provide hash-lang
         hash-lang-notify-channel)

;; Bridge for Emacs to use hash-lang%
;;
;; - Reference hash-lang% objects by string ID.
;;
;; - Adjust Emacs 1-based positions to/from hash-lang% 0-based.
;;
;; - Handle notifications about changed language and token spans,
;;   putting to an async channel handled in command-server and up in
;;   Emacs much like the channels used for logging and debugging.

(define hash-lang-class-or-error-message
  (with-handlers ([exn:fail? exn-message])
    (dynamic-require "hash-lang.rkt" 'hash-lang%)))

(define our-hash-lang%
  (when (class? hash-lang-class-or-error-message)
    (class hash-lang-class-or-error-message
      (super-new)
      (init-field id)
      (define/override (on-changed-lang-values)
        (async-channel-put hash-lang-notify-channel
                           (list 'hash-lang id
                                 'lang
                                 'racket-grouping (send this racket-grouping-position?)
                                 'range-indenter  (send this range-indenter?))))
      (define/override (on-changed-tokens gen beg end)
        (async-channel-put hash-lang-notify-channel
                           (list 'hash-lang id
                                 'update gen (add1 beg) (add1 end)))))))

(define (hash-lang . args)
  (unless (class? hash-lang-class-or-error-message)
    (error 'hash-lang
           (string-append "This feature needs a newer version of syntax-color-lib.\n"
                          hash-lang-class-or-error-message)))
  (match args
    [`(create ,id ,s)                              (create id s)]
    [`(delete ,id)                                 (delete id)]
    [`(update ,id ,gen ,pos ,old-len ,str)         (update id gen pos old-len str)]
    [`(indent-amount ,id ,gen ,pos)                (indent-amount id gen pos)]
    [`(indent-region-amounts ,id ,gen ,from ,upto) (indent-region-amounts id gen from upto)]
    [`(classify ,id ,gen ,pos)                     (classify id gen pos)]
    [`(grouping ,id ,gen ,pos ,dir ,limit ,count)  (grouping id gen pos dir limit count)]
    [`(get-tokens ,id ,gen ,from ,upto)            (get-tokens id gen from upto)]))

(define hash-lang-notify-channel (make-async-channel))

(define ht (make-hash)) ;id => hash-lang%
(define (get-object id) (hash-ref ht id))

(define (create id s) ;any/c string? -> void
  (define obj (new our-hash-lang% [id id]))
  (hash-set! ht id obj)
  (send obj update! 1 0 0 s))

(define (delete id)
  (match (hash-ref ht id #f)
    [#f (log-racket-mode-warning "hash-lang delete ~v: not found" id)]
    [obj (send obj delete)
         (hash-remove! ht id)]))

(define (update id gen pos old-len str)
  (send (get-object id) update! gen (sub1 pos) old-len str))

(define (indent-amount id gen pos)
  (with-time/log "hash-lang indent-amount"
    (send (get-object id) indent-line-amount gen (sub1 pos))))

(define (indent-region-amounts id gen from upto)
  (with-time/log "hash-lang indent-region-amounts"
    (match (send (get-object id) indent-region-amounts gen (sub1 from) (sub1 upto))
      [#f 'false] ;avoid Elisp nil/`() punning problem
      [v v])))

(define (classify id gen pos)
  (match-define (list beg end attribs) (send (get-object id) classify gen (sub1 pos)))
  (list (add1 beg) (add1 end) attribs))

(define (grouping id gen pos dir limit count)
  (match (send (get-object id) grouping gen (sub1 pos) dir limit count)
    [(? number? n) (add1 n)]
    [v v]))

(define (get-tokens id gen from upto)
  (for/list ([tok (in-list (send (get-object id) get-tokens gen (sub1 from) (sub1 upto)))])
    (match-define (list (app add1 beg) (app add1 end) (app attribs->types types)) tok)
    (list beg end types)))

(define (attribs->types attribs)
  (match attribs
    [(? symbol? s) (list s)]
    [(? hash? ht)  (cons (hash-ref ht 'type 'unknown)
                         (if (hash-ref ht 'comment? #f)
                             '(sexp-comment-body)
                             null))]))

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

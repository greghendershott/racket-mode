;; Copyright (c) 2020-2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/async-channel
         racket/class
         racket/match
         racket/runtime-path
         "elisp.rkt"
         "lang-info.rkt"
         "util.rkt")

(provide hash-lang
         hash-lang-notify-channel)

;; Bridge for Emacs front end to use hash-lang%
;;
;; - Reference hash-lang% objects by a serializable ID supplied by the
;;   front end.
;;
;; - Adjust Emacs 1-based positions to/from hash-lang% 0-based.
;;
;; - Handle notifications about changed languages and tokens, by
;;   putting values to an async channel that is handled in
;;   command-server.rkt, and then and up in Emacs, similar to
;;   notifications used for logging and debugging.

(define-runtime-path hash-lang.rkt "hash-lang.rkt")

(define hash-lang-class-or-error-message
  (with-handlers ([exn:fail? exn-message])
    (dynamic-require hash-lang.rkt 'hash-lang%)))

(define our-hash-lang%
  (when (class? hash-lang-class-or-error-message)
    (class hash-lang-class-or-error-message
      (super-new)
      (init-field id)
      (define/override (on-changed-lang-info _gen li)
        (async-channel-put
         hash-lang-notify-channel
         (list
          'hash-lang id
          'lang
          'module-language    (lang-info-module-language li)
          'racket-grouping    (lang-info-grouping-position-is-racket? li)
          'range-indenter     (and (lang-info-range-indenter li) #t)
          'submit-predicate   (and (lang-info-submit-predicate li) #t)
          ;; String-ize paren-matches and quotes-matches data to avoid
          ;; discrepancies with Emacs Lisp allowed symbols and char
          ;; reader syntax.
          'paren-matches      (for/list ([o/c (in-list (lang-info-paren-matches li))])
                                (match-define (list o c) o/c)
                                (cons (symbol->string o) (symbol->string c)))
          'quote-matches      (for/list ([c (in-list (lang-info-quote-matches li))])
                                (make-string 1 c))
          'comment-delimiters (lang-info-comment-delimiters li))))
      (define/override (on-changed-tokens gen beg end)
        (when (< beg end)
          (async-channel-put hash-lang-notify-channel
                             (list 'hash-lang id
                                   'update
                                   gen (add1 beg) (add1 end))))))))

(define (hash-lang . args)
  (cond
    [(class? hash-lang-class-or-error-message) (apply hash-lang* args)]
    [(eq? 'create (car args)) #f]
    [else (error 'hash-lang hash-lang-class-or-error-message)]))

(define (hash-lang* . args)
  (match args
    [`(create ,id ,ols ,str)
     (create id ols str)]
    [`(delete ,id)
     (delete id)]
    [`(update ,id ,gen ,pos ,old-len ,str)
     (update id gen pos old-len str)]
    [`(indent-amount ,id ,gen ,pos)
     (indent-amount id gen pos)]
    [`(indent-range-amounts ,id ,gen ,from ,upto ,rev?)
     (indent-range-amounts id gen from upto (as-racket-bool rev?))]
    [`(classify ,id ,gen ,pos)
     (classify id gen pos)]
    [`(grouping ,id ,gen ,pos ,dir ,limit ,count)
     (grouping id gen pos dir limit count)]
    [`(get-tokens ,id ,gen ,from ,upto)
     (get-tokens id gen from upto)]
    [`(submit-predicate ,id ,str ,eos?)
     (submit-predicate id str eos?)]))

(define hash-lang-notify-channel (make-async-channel))

(define ht (make-hash)) ;id => hash-lang%
(define (get-object id)
  (hash-ref ht id
            (λ () (error 'hash-lang-bridge
                         "No hash-lang exists with ID ~v" id))))

(define (create id ols str) ;any/c (or/c #f string?) string? -> void
  (define obj (new our-hash-lang%
                   [id id]
                   [other-lang-source (and ols (not (null? ols)) ols)]))
  (hash-set! ht id obj)
  (send obj update! 1 0 0 str)
  id)

(define (delete id)
  (hash-remove! ht id))

(define (update id gen pos old-len str)
  (send (get-object id) update! gen (sub1 pos) old-len str))

(define (indent-amount id gen pos)
  (with-time/log "hash-lang indent-amount"
    (send (get-object id) indent-line-amount gen (sub1 pos))))

(define (indent-range-amounts id gen from upto reverse?)
  (with-time/log "hash-lang indent-range-amounts"
    (match (send (get-object id) indent-range-amounts gen (sub1 from) (sub1 upto) reverse?)
      [#f 'false] ;avoid Elisp nil/`() punning problem
      [v v])))

(define (classify id gen pos)
  (match-define (list beg end attribs) (send (get-object id) classify gen (sub1 pos)))
  (list (add1 beg) (add1 end) (attribs->types attribs)))

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
    [(? hash? ht)  (cons (or (hash-ref ht 'semantic-type-guess #f)
                             (hash-ref ht 'type 'unknown))
                         (if (hash-ref ht 'comment? #f)
                             '(sexp-comment-body)
                             null))]))

(define (submit-predicate id str -eos?)
  (define in (open-input-string str))
  (define eos (as-racket-bool -eos?))
  (send (get-object id) submit-predicate in eos))

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

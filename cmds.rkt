#lang at-exp racket/base

(require macro-debugger/analysis/check-requires
         racket/contract/base
         racket/contract/region
         racket/format
         racket/function
         racket/list
         racket/match
         racket/path
         racket/port
         racket/pretty
         racket/set
         racket/string
         racket/tcp
         syntax/modresolve
         (only-in xml xexpr->string)
         "channel.rkt"
         "defn.rkt"
         "fresh-line.rkt"
         "help.rkt"
         "instrument.rkt"
         "mod.rkt"
         "scribble.rkt"
         "try-catch.rkt"
         "util.rkt")

(provide start-command-server
         attach-command-server
         make-prompt-read
         display-prompt)

(module+ test
  (require rackunit))

;; Emacs Lisp needs to send us commands and get responses.
;;
;; There are a few ways to do this.
;;
;; 0. Vanilla "inferior-mode" stdin/stdout. Commands are sent to stdin
;;    -- "typed invisibly at the REPL prompt" -- and responses go to
;;    stdout. Mixing command I/O with the user's Racket program I/O
;;    works better than you might expect -- but worse than you want.
;;
;;    Unmixing output is the biggest challenge. Traditionally a comint
;;    filter proc will try to extract everything up to a sentinel like
;;    the next REPL prompt. But it can accidentally match user program
;;    output that resembles the sentinel. (Real example: The ,describe
;;    command returns HTML that happens to contain `\ntag>`.)
;;
;;    Input is also a problem. If the user's program reads from stdin,
;;    it might eat a command. Or if it runs for awhile; commands are
;;    blocked.
;;
;;    TL;DR: Command traffic should be out of band not mixed in stdin
;;    and stdout.
;;
;; 1. Use files. Originally I addressed the mixed-output side by
;;    having commands output responses to a file. (Stdout only
;;    contains regular Racket output and appears directly in REPL
;;    buffer as usual.) This didn't address mixed input. Although
;;    using a command input file could have worked (and did work in
;;    experiments), instead...
;;
;; 2. Use sockets. Now the status quo. Note that this is _not_ a
;;    "network REPL". The socket server is solely for command input
;;    and output. There is no redirection of user's Racket program
;;    I/O, and it is still handled by Emacs' comint-mode in the usual
;;    manner.

(define command-server-ns (make-base-namespace))
(define command-server-path #f)

(define (attach-command-server ns path)
  (set! command-server-ns ns)
  (set! command-server-path path))

(define (start-command-server port)
  (void
   (thread
    (λ ()
      (define listener (tcp-listen port 4 #t))
      (let connect ()
        (define-values (in out) (tcp-accept listener))
        (parameterize ([current-input-port in]
                       [current-output-port out])
          (define fail (λ _ (elisp-println #f)))
          (let loop ()
            (match (read-syntax)
              [(? eof-object?) (void)]
              [stx (with-handlers ([exn:fail? fail])
                     (parameterize ([current-namespace command-server-ns])
                       (handle-command stx command-server-path fail)))
                   (flush-output)
                   (loop)])))
        (close-input-port in)
        (close-output-port out)
        (connect))))))

(define at-prompt (box 0))
(define (at-prompt?) (positive? (unbox at-prompt)))

(define/contract ((make-prompt-read m))
  (-> (or/c #f mod?) (-> any))
  (display-prompt (maybe-mod->prompt-string m))
  (define in ((current-get-interaction-input-port)))
  (define stx (dynamic-wind
                (λ _ (box-swap! at-prompt add1))
                (λ _ ((current-read-interaction) (object-name in) in))
                (λ _ (box-swap! at-prompt sub1))))
  (syntax-case stx ()
    [(uq cmd)
     (eq? 'unquote (syntax-e #'uq))
     (begin (handle-command #'cmd m usage)
            #'(void))] ;avoid Typed Racket printing a type
    [_ stx]))

(define (display-prompt str)
  (flush-output (current-error-port))
  (fresh-line)
  (display str)
  ;; Use a character unlikely to appear in normal output. Makes it
  ;; easier for Emacs comint-regexp-prompt to avoid matching program
  ;; output by mistake. (This used to be very important: We mixed
  ;; command output with stdout and a comint filter proc had to un-mix
  ;; it. Today it mainly just helps comint-{previous next}-prompt.)
  (display #\uFEFF) ;ZERO WIDTH NON-BREAKING SPACE
  (display "> ")
  (flush-output)
  (zero-column!))

(define (elisp-read)
  ;; Elisp prints '() as 'nil. Reverse that. (Assumption: Although
  ;; some Elisp code puns nil/() also to mean "false" -- _our_ Elisp
  ;; code _won't_ do that when sending us commands.)
  (match (read)
    ['nil '()]
    [x x]))

(define/contract (handle-command cmd-stx m unknown-command)
  (-> syntax? (or/c #f mod?) (-> any) any)
  (define-values (dir file mod-path) (maybe-mod->dir/file/rmp m))
  (define path (and file (build-path dir file)))
  (let ([read elisp-read])
    (case (syntax-e cmd-stx)
      ;; These commands are intended to be used by either the user or
      ;; racket-mode.
      [(run)             (run-or-top 'run)]
      [(top)             (run-or-top 'top)]
      [(doc)             (doc (read-syntax))]
      [(exp)             (exp1 (read))]
      [(exp+)            (exp+)]
      [(exp!)            (exp! (read))]
      [(pwd)             (display-commented (~v (current-directory)))]
      [(cd)              (cd (~a (read)))]
      [(exit)            (exit)]
      [(info)            (info)]
      ;; These remaining commands are intended to be used by
      ;; racket-mode, only.
      [(path)            (elisp-println path)]
      [(prompt)          (elisp-println (and (at-prompt?) (or path 'top)))]
      [(syms)            (syms)]
      [(def)             (def-loc (read))]
      [(describe)        (describe (read-syntax))]
      [(mod)             (mod-loc (read) mod-path)]
      [(type)            (type (read))]
      [(requires/tidy)   (requires/tidy (read))]
      [(requires/trim)   (requires/trim (read) (read))]
      [(requires/base)   (requires/base (read) (read))]
      [(find-collection) (find-collection (read))]
      [(get-profile)     (get-profile)]
      [(get-uncovered)   (get-uncovered path)]
      [(check-syntax)    (check-syntax (string->path (read)))]
      ;; Obsolete
      [(log)             (display-commented "Use M-x racket-logger instead")]
      [else              (unknown-command)])))

(define (usage)
  (display-commented
   @~a{Commands:
       ,run <module> [<mem-limit-MB> [<pretty-print?> [<error-context> [<cmd-line>]]]]
       ,top          [<mem-limit-MB> [<pretty-print?> [<error-context> [<cmd-line>]]]]
         <module> = <file>
                  | (<file> <submodule-id> ...)
         <file> = file.rkt
                | /path/to/file.rkt
                | "file.rkt" | "/path/to/file.rkt"
         <error-context> = low
                         | medium
                         | high
         <cmd-line> = (listof string?) e.g. '("-f" "foo" "--bar" "baz")
       ,exit
       ,doc <identifier>|<string>
       ,exp <stx>
       ,exp+
       ,exp! <stx>
       ,pwd
       ,cd <path>}))

;;; run, top, info

;; Parameter-like interface, but we don't want thread-local. We do
;; want to call collect-garbage IFF the new limit is less than the old
;; one or less than the current actual usage.
(define current-mem
  (let ([old 0])
    (case-lambda
      [() old]
      [(new)
       (and old new
            (or (< new old)
                (< (* new 1024 1024) (current-memory-use)))
            (collect-garbage))
       (set! old new)])))

;; Likewise: Want parameter signature but NOT thread-local.
(define-syntax-rule (make-parameter-ish init)
  (let ([old init])
    (case-lambda
      [() old]
      [(new) (set! old new)])))

(define current-pp? (make-parameter-ish #t))
(define current-ctx-lvl (make-parameter-ish 'low)) ;context-level?
(define current-args (make-parameter-ish (vector)))

(define (run-or-top which)
  ;; Support both the ,run and ,top commands. Latter has no first path
  ;; arg, but otherwise they share subsequent optional args. (Note:
  ;; The complexity here is from the desire to let user type simply
  ;; e.g. ",top" or ",run file" and use the existing values for the
  ;; omitted args. We're intended mainly to be used from Emacs, which
  ;; can/does always supply all the args. But, may as well make it
  ;; convenient for human users, too.)
  (define (go what)
    (define maybe-mod (->mod/existing what))
    (when (or maybe-mod (eq? 'top which))
      (put/stop (rerun maybe-mod
                       (current-mem)
                       (current-pp?)
                       (current-ctx-lvl)
                       (current-args)))))
  (match (match which
           ['run (read-line->reads)]
           ['top (cons #f (read-line->reads))]) ;i.e. what = #f
    [(list what (? number? mem) (? boolean? pp?) (? context-level? ctx)
           (? (or/c #f (listof string?)) args))
     (current-mem mem)
     (current-pp? pp?)
     (current-ctx-lvl ctx)
     (current-args (list->vector (or args (list)))) ;Elisp () = nil => #f
     (go what)]
    [(list what (? number? mem) (? boolean? pp?) (? context-level? ctx))
     (current-mem mem)
     (current-pp? pp?)
     (current-ctx-lvl ctx)
     (go what)]
    [(list what (? number? mem) (? boolean? pp?))
     (current-mem mem)
     (current-pp? pp?)
     (go what)]
    [(list what (? number? mem))
     (current-mem mem)
     (go what)]
    [(list what)
     (go what)]
    [_ (usage)]))

(define (read-line->reads)
  (reads-from-string (read-line)))

(define (reads-from-string s)
  (with-input-from-string s reads))

(define (reads)
  (match (read)
    [(? eof-object?) (list)]
    ['t              (cons #t (reads))] ;in case from elisp
    ['nil            (cons #f (reads))] ;in case from elisp
    [x               (cons x (reads))]))

;; This really just for my own use debugging. Not documented.
(define (info)
  (displayln @~a{Memory Limit:   @(current-mem)
                 Pretty Print:   @(current-pp?)
                 Error Context:  @(current-ctx-lvl)
                 Command Line:   @(current-args)}))

;;; misc other commands

(define (syms)
  (elisp-println (sort (map symbol->string (namespace-mapped-symbols))
                       string<?)))

(define (def-loc sym)
  (elisp-println (find-definition (symbol->string sym))))

(define (mod-loc v rel)
  (define (mod-loc* mod rel)
    (define path (with-handlers ([exn:fail? (λ _ #f)])
                   (resolve-module-path mod rel)))
    (and path
         (file-exists? path)
         (list (path->string path) 1 0)))
  (elisp-println (cond [(module-path? v) (mod-loc* v rel)]
                       [(symbol? v)      (mod-loc* (symbol->string v) rel)]
                       [else             #f])))

(define (type v) ;; the ,type command.  rename this??
  (elisp-println (type-or-sig v)))

(define (type-or-sig v)
  (or (type-or-contract v)
      (sig v)
      ""))

(define (sig v) ;any/c -> (or/c #f string?)
  (and (symbol? v)
       (match (find-signature (symbol->string v))
         [#f #f]
         [x (~a x)])))

(define (type-or-contract v) ;any/c -> (or/c #f string?)
  ;; 1. Try using Typed Racket's REPL simplified type.
  (try (match (with-output-to-string
                (λ ()
                  ((current-eval)
                   (cons '#%top-interaction v))))
         [(pregexp "^- : (.*) \\.\\.\\..*\n" (list _ t)) t]
         [(pregexp "^- : (.*)\n$"            (list _ t)) t])
       #:catch exn:fail? _
       ;; 2. Try to find a contract.
       (try (parameterize ([error-display-handler (λ _ (void))])
              ((current-eval)
               (cons '#%top-interaction
                     `(if (has-contract? ,v)
                       (~a (contract-name (value-contract ,v)))
                       (error "")))))
            #:catch exn:fail? _
            #f)))

(define (sig-and/or-type stx)
  (define dat (syntax->datum stx))
  (define s (sig dat))
  (define t (type-or-contract stx))
  (xexpr->string
   `(div ()
     (h1 () ,(or s (~a dat)))
     ,(cond [(not (or s t))
             `(p ()
               (em ()  "(Found no documentation, signature, type, or contract.)"))]
            [t `(pre () ,t)]
            [else ""])
     (br ()))))

;;; describe

;; If a symbol has installed documentation, display it.
;;
;; Otherwise, walk the source to find the signature of its definition
;; (because the argument names have explanatory value), and also look
;; for Typed Racket type or a contract, if any.

(define (describe stx)
  (write (describe* stx))
  (newline))

(define (describe* _stx)
  (define stx (namespace-syntax-introduce _stx))
  (or (scribble-doc/html stx)
      (sig-and/or-type stx)))

;;; print elisp values

(define (elisp-println v)
  (elisp-print v)
  (newline))

(define (elisp-print v)
  (print (->elisp v)))

(define (->elisp v)
  (match v
    [(or #f (list)) 'nil]
    [#t             't]
    [(? list? xs)   (map ->elisp xs)]
    [(cons x y)     (cons (->elisp x) (->elisp y))]
    [(? path? v)    (path->string v)]
    [v              v]))

(module+ test
  (check-equal? (with-output-to-string
                  (λ () (elisp-print '(1 #t nil () (a . b)))))
                "'(1 t nil nil (a . b))"))

;;; doc / help

(define (doc stx)
  (or (find-help (namespace-syntax-introduce stx))
      (perform-search (~a (syntax->datum stx))))
  ;; Need some command response
  (elisp-println "Sent to web browser"))

;; cd

(define (cd s)
  (let ([old-wd (current-directory)])
    (current-directory s)
    (unless (directory-exists? (current-directory))
      (display-commented (format "~v doesn't exist." (current-directory)))
      (current-directory old-wd))
    (display-commented (format "In ~v" (current-directory)))))

;;; syntax expansion

(define last-stx #f)

(define (exp1 stx)
  (set! last-stx (expand-once stx))
  (pp-stx last-stx))

(define (exp+)
  (when last-stx
    (define this-stx (expand-once last-stx))
    (cond [(equal? (syntax->datum last-stx) (syntax->datum this-stx))
           (display-commented "Already fully expanded.")
           (set! last-stx #f)]
          [else
           (pp-stx this-stx)
           (set! last-stx this-stx)])))

(define (exp! stx)
  (set! last-stx #f)
  (pp-stx (expand stx)))

(define (pp-stx stx)
  (newline)
  (pretty-print (syntax->datum stx)
                (current-output-port)
                1))

;;; requires

;; requires/tidy : (listof require-sexpr) -> require-sexpr
(define (requires/tidy reqs)
  (let* ([reqs (combine-requires reqs)]
         [reqs (group-requires reqs)])
    (elisp-println (require-pretty-format reqs))))

;; requires/trim : path-string? (listof require-sexpr) -> require-sexpr
;;
;; Note: Why pass in a list of the existing require forms -- why not
;; just use the "keep" list from show-requires? Because the keep list
;; only states the module name, not the original form. Therefore if
;; the original require has a subform like `(only-in mod f)` (or
;; rename-in, except-in, &c), we won't know how to preserve that
;; unless we're given it. That's why our strategy must be to look for
;; things to drop, as opposed to things to keep.
(define (requires/trim path-str reqs)
  (let* ([reqs (combine-requires reqs)]
         [sr (show-requires* path-str)]
         [drops (filter-map (λ (x)
                              (match x
                                [(list 'drop mod lvl) (list mod lvl)]
                                [_ #f]))
                            sr)]
         [reqs (filter-map (λ (req)
                             (cond [(member req drops) #f]
                                   [else req]))
                           reqs)]
         [reqs (group-requires reqs)])
    (elisp-println (require-pretty-format reqs))))

;; Use `bypass` to help convert from `#lang racket` to `#lang
;; racket/base` plus explicit requires.
;;
;; Note: Currently this is hardcoded to `#lang racket`, only.
(define (requires/base path-str reqs)
  (let* ([reqs (combine-requires reqs)]
         [sr (show-requires* path-str)]
         [drops (filter-map (λ (x)
                              (match x
                                [(list 'drop mod lvl) (list mod lvl)]
                                [_ #f]))
                            sr)]
         [adds (append*
                (filter-map (λ (x)
                              (match x
                                [(list 'bypass 'racket 0
                                       (list (list mod lvl _) ...))
                                 (filter (λ (x)
                                           (match x
                                             [(list 'racket/base 0) #f]
                                             [_ #t]))
                                         (map list mod lvl))]
                                [_ #f]))
                            sr))]
         [reqs (filter-map (λ (req)
                             (cond [(member req drops) #f]
                                   [else req]))
                           reqs)]
         [reqs (append reqs adds)]
         [reqs (group-requires reqs)])
    (elisp-println (require-pretty-format reqs))))

;; show-requires* : Like show-requires but accepts a path-string? that
;; need not already be a module path.
(define (show-requires* path-str)
  (define-values (base name _) (split-path (string->path path-str)))
  (parameterize ([current-load-relative-directory base]
                 [current-directory base])
    (show-requires name)))

(define (combine-requires reqs)
  (remove-duplicates
   (append* (for/list ([req reqs])
              (match req
                [(list* 'require vs)
                 (append*
                  (for/list ([v vs])
                    ;; Use (list mod level), like `show-requires` uses.
                    (match v
                      [(list* 'for-meta level vs) (map (curryr list level) vs)]
                      [(list* 'for-syntax vs)     (map (curryr list 1) vs)]
                      [(list* 'for-template vs)   (map (curryr list -1) vs)]
                      [(list* 'for-label vs)      (map (curryr list #f) vs)]
                      [v                          (list (list v 0))])))])))))

(module+ test
  (require rackunit)
  (check-equal?
   (combine-requires '((require a b c)
                       (require d e)
                       (require a f)
                       (require (for-syntax s t u) (for-label l0 l1 l2))
                       (require (for-meta 1 m1a m1b)
                                (for-meta 2 m2a m2b))))
   '((a 0) (b 0) (c 0) (d 0) (e 0) (f 0)
     (s 1) (t 1) (u 1)
     (l0 #f) (l1 #f) (l2 #f)
     (m1a 1) (m1b 1) (m2a 2) (m2b 2))))

;; Given a list of requires -- each in the (list module level) form
;; used by `show-requires` -- group them by level and convert them to
;; a Racket `require` form. Also, sort the subforms by phase level:
;; for-syntax, for-template, for-label, for-meta, and plain (0).
;; Within each such group, sort them first by module paths then
;; relative requires. Within each such group, sort alphabetically.
(define (group-requires reqs)
  ;; Put the requires into a hash of sets.
  (define ht (make-hasheq)) ;(hash/c <level> (set <mod>))
  (for ([req reqs]) (match req
                      [(list mod lvl) (hash-update! ht lvl
                                                    (lambda (s) (set-add s mod))
                                                    (set mod))]))
  (define (mod-set->mod-list mod-set)
    (sort (set->list mod-set) mod<?))
  (define (for-level level k)
    (define mods (hash-ref ht level #f))
    (cond [mods (k (mod-set->mod-list mods))]
          [else '()]))
  (define (preface . pres)
    (λ (mods) `((,@pres ,@mods))))
  (define (meta-levels)
    (sort (for/list ([x (hash-keys ht)] #:when (not (member x '(-1 0 1 #f)))) x)
          <))
  `(require
    ,@(for-level  1 (preface 'for-syntax))
    ,@(for-level -1 (preface 'for-template))
    ,@(for-level #f (preface 'for-label))
    ,@(append* (for/list ([level (in-list (meta-levels))])
                 (for-level level (preface 'for-meta level))))
    ,@(for-level 0 values)))

(module+ test
  (check-equal? (group-requires
                 (combine-requires
                  '((require z c b a)
                    (require (for-meta 4 m41 m40))
                    (require (for-meta -4 m-41 m-40))
                    (require (for-label l1 l0))
                    (require (for-template t1 t0))
                    (require (for-syntax s1 s0))
                    (require "a.rkt" "b.rkt" "c.rkt" "z.rkt"
                             (only-in "mod.rkt" oi)
                             (only-in mod oi)))))
                '(require
                  (for-syntax s0 s1)
                  (for-template t0 t1)
                  (for-label l0 l1)
                  (for-meta -4 m-40 m-41)
                  (for-meta 4 m40 m41)
                  a b c (only-in mod oi) z
                  "a.rkt" "b.rkt" "c.rkt" (only-in "mod.rkt" oi) "z.rkt")))

(define (mod<? a b)
  (define (key x)
    (match x
      [(list 'only-in   m _ ...)     (key m)]
      [(list 'except-in m _ ...)     (key m)]
      [(list 'prefix-in _ m)         (key m)]
      [(list 'relative-in _ m _ ...) (key m)]
      [m                             m]))
  (let ([a (key a)]
        [b (key b)])
    (or (and (symbol? a) (not (symbol? b)))
        (and (list? a) (not (list? b)))
        (and (not (string? a)) (string? a))
        (and (string? a) (string? b)
             (string<? a b))
        (and (symbol? a) (symbol? b)
             (string<? (symbol->string a) (symbol->string b))))))

(module+ test
  (check-true (mod<? 'a 'b))
  (check-false (mod<? 'b 'a))
  (check-true (mod<? 'a '(only-in b)))
  (check-true (mod<? '(only-in a) 'b))
  (check-true (mod<? 'a '(except-in b)))
  (check-true (mod<? '(except-in a) 'b))
  (check-true (mod<? 'a '(prefix-in p 'b)))
  (check-true (mod<? '(prefix-in p 'a) 'b))
  (check-true (mod<? 'a '(relative-in p 'b)))
  (check-true (mod<? '(relative-in p 'a) 'b))
  (check-true (mod<? 'a '(prefix-in p (only-in b))))
  (check-true (mod<? '(prefix-in p (only-in a)) 'b)))

;; require-pretty-format : list? -> string?
(define (require-pretty-format x)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (require-pretty-print x))
  (get-output-string out))

(module+ test
  (check-equal? (require-pretty-format
                 '(require a))
                @~a{(require a)

                    })
  (check-equal? (require-pretty-format
                 '(require a b))
                @~a{(require a
                             b)

                    })
  (check-equal? (require-pretty-format
                 '(require (for-syntax a b) (for-meta 2 c d) e f))
                @~a{(require (for-syntax a
                                         b)
                             (for-meta 2 c
                                         d)
                             e
                             f)

                    })
  (check-equal? (require-pretty-format
                 `(require (only-in m a b) (except-in m a b)))
                @~a{(require (only-in m
                                      a
                                      b)
                             (except-in m
                                        a
                                        b))

                    }))

;; Pretty print a require form with one module per line and with
;; indentation for the `for-X` subforms. Example:
;;
;; (require (for-syntax racket/base
;;                      syntax/parse)
;;          (for-meta 3 racket/a
;;                      racket/b)
;;          racket/format
;;          racket/string
;;          "a.rkt"
;;          "b.rkt")
(define (require-pretty-print x)
  (define (prn x first? indent)
    (define (indent-string)
      (if first? "" (make-string indent #\space)))
    (define (prn-form pre this more)
      (define new-indent (+ indent (+ 2 (string-length pre))))
      (printf "~a(~a " (indent-string) pre)
      (prn this #t new-indent)
      (for ([x more])
        (newline)
        (prn x #f new-indent))
      (display ")"))
    (match x
      [(list 'require)
       (void)]
      [(list* (and pre (or 'require 'for-syntax 'for-template 'for-label
                           'only-in 'except-in))
              this more)
       (prn-form (format "~s" pre) this more)
       (when (eq? pre 'require)
         (newline))]
      [(list* 'for-meta level this more)
       (prn-form (format "for-meta ~a" level) this more)]
      [this
       (printf "~a~s" (indent-string) this)]))
  (prn x #t 0))

;;; find-collection

(define (do-find-collection str)
  (match (with-handlers ([exn:fail? (λ _ #f)])
           (and ;;#f ;<-- un-comment to exercise fallback path
            (dynamic-require 'find-collection/find-collection
                             'find-collection-dir)))
    [#f 'find-collection-not-installed]
    [f  (map path->string (f str))]))

(define find-collection (compose elisp-println do-find-collection))

;;; profile

(define (get-profile)
  (elisp-println
   ;; TODO: Filter files from racket-mode itself, b/c just noise?
   (for/list ([x (in-list (get-profile-info))])
     (match-define (list count msec name stx _ ...) x)
     (list count
           msec
           (and name (symbol->string name))
           (and (syntax-source stx) (path? (syntax-source stx))
                (path->string (syntax-source stx)))
           (syntax-position stx)
           (and (syntax-position stx) (syntax-span stx)
                (+ (syntax-position stx) (syntax-span stx)))))))

;;; coverage

(define (get-uncovered file)
  (elisp-println
   (consolidate-coverage-ranges
    (for*/list ([x (in-list (get-test-coverage-info))]
                [covered? (in-value (first x))]
                #:when (not covered?)
                [src (in-value (second x))]
                #:when (equal? file src)
                [pos (in-value (third x))]
                [span (in-value (fourth x))])
      (cons pos (+ pos span))))))

(define (consolidate-coverage-ranges xs)
  (remove-duplicates (sort xs < #:key car)
                     same?))

(define (same? x y)
  ;; Is x a subset of y or vice versa?
  (match-define (cons x/beg x/end) x)
  (match-define (cons y/beg y/end) y)
  (or (and (<= x/beg y/beg) (<= y/end x/end))
      (and (<= y/beg x/beg) (<= x/end y/end))))

(module+ test
  (check-true (same? '(0 . 9) '(0 . 9)))
  (check-true (same? '(0 . 9) '(4 . 5)))
  (check-true (same? '(4 . 5) '(0 . 9)))
  (check-false (same? '(0 . 1) '(1 . 2)))
  (check-equal? (consolidate-coverage-ranges
                 '((10 . 20) (10 . 11) (19 . 20) (10 . 20)
                   (20 . 30) (20 . 21) (29 . 30) (20 . 30)))
                '((10 . 20)
                  (20 . 30)))
  ;; This is a test of actual coverage data I got from one example,
  ;; where the maximal subsets were (164 . 197) and (214. 247).
  (check-equal?
   (consolidate-coverage-ranges
    '((164 . 197) (164 . 197) (164 . 197)
      (173 . 180) (173 . 180) (173 . 180) (173 . 180) (173 . 180) (187 . 196)
      (214 . 247) (214 . 247) (214 . 247)
      (223 . 230) (223 . 230) (223 . 230) (223 . 230) (223 . 230) (237 . 246)))
   '((164 . 197) (214 . 247))))

;;; check-syntax

(define check-syntax
  (let ([show-content (try (let ([f (dynamic-require 'drracket/check-syntax
                                                     'show-content)])
                             ;; Ensure correct position info for
                             ;; Unicode like λ. show-content probably
                             ;; ought to do this itself, but work
                             ;; around that.
                             (λ (path)
                               (parameterize ([port-count-lines-enabled #t])
                                 (f path))))
                           #:catch exn:fail? _ (λ _ (elisp-println 'not-supported)))])
    ;; Note: Adjust all positions to 1-based Emacs `point' values.
    (λ (path)
      (parameterize ([current-load-relative-directory (path-only path)])
        ;; Get all the data.
        (define xs (remove-duplicates (show-content path)))
        ;; Extract the add-mouse-over-status items into a list.
        (define infos
          (remove-duplicates
           (filter values
                   (for/list ([x (in-list xs)])
                     (match x
                       [(vector 'syncheck:add-mouse-over-status beg end str)
                        (list 'info (add1 beg) (add1 end) str)]
                       [_ #f])))))
        ;; Consolidate the add-arrow/name-dup items into a hash table
        ;; with one item per definition. The key is the definition
        ;; position. The value is the set of its uses.
        (define ht-defs/uses (make-hash))
        (for ([x (in-list xs)])
          (match x
            [(or (vector 'syncheck:add-arrow/name-dup
                         def-beg def-end
                         use-beg use-end
                         _ _ _ _)
                 (vector 'syncheck:add-arrow/name-dup/pxpy
                         def-beg def-end _ _
                         use-beg use-end _ _
                         _ _ _ _))
             (hash-update! ht-defs/uses
                           (list (add1 def-beg) (add1 def-end))
                           (λ (v) (set-add v (list (add1 use-beg) (add1 use-end))))
                           (set))]
            [_ #f]))
        ;; Convert the hash table into a list, sorting the usage positions.
        (define defs/uses
          (for/list ([(def uses) (in-hash ht-defs/uses)])
            (match-define (list def-beg def-end) def)
            (define tweaked-uses (sort (set->list uses) < #:key car))
            (list 'def/uses def-beg def-end tweaked-uses)))
        ;; Append both lists and print as Elisp values.
        (elisp-println (append infos defs/uses))))))

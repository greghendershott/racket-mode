#lang at-exp racket/base

(require macro-debugger/analysis/check-requires
         racket/contract/base
         racket/contract/region
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/path
         racket/port
         racket/pretty
         racket/set
         racket/string
         racket/system
         racket/tcp
         syntax/modresolve
         (only-in xml xexpr->string)
         "channel.rkt"
         "defn.rkt"
         "fresh-line.rkt"
         "help.rkt"
         "instrument.rkt"
         "md5.rkt"
         "mod.rkt"
         "scribble.rkt"
         "syntax.rkt"
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

(define drracket:submit-predicate/c (-> input-port? boolean? boolean?))

(define-struct/contract context
  ([ns          namespace?]
   [maybe-mod   (or/c #f mod?)]
   [md5         string?]
   [submit-pred (or/c #f drracket:submit-predicate/c)]))

(define command-server-context (context (make-base-namespace) #f "" #f))

(define/contract (attach-command-server ns maybe-mod)
  (-> namespace? (or/c #f mod?) any)
  (set! command-server-context
        (context ns
                 maybe-mod
                 (maybe-mod->md5 maybe-mod)
                 (get-repl-submit-predicate maybe-mod))))

(define (maybe-mod->md5 m)
  (define-values (dir file _) (maybe-mod->dir/file/rmp m))
  (if (and dir file)
      (file->md5 (build-path dir file))
      ""))

;; <https://docs.racket-lang.org/tools/lang-languages-customization.html#(part._.R.E.P.L_.Submit_.Predicate)>
(define/contract (get-repl-submit-predicate m)
  (-> (or/c #f mod?) (or/c #f drracket:submit-predicate/c))
  (define-values (dir file rmp) (maybe-mod->dir/file/rmp m))
  (define path (and dir file (build-path dir file)))
  (and path rmp
       (or (with-handlers ([exn:fail? (λ _ #f)])
            (match (with-input-from-file (build-path dir file) read-language)
              [(? procedure? get-info)
               (match (get-info 'drracket:submit-predicate #f)
                 [#f #f]
                 [v  v])]
              [_ #f]))
           (with-handlers ([exn:fail? (λ _ #f)])
             (match (module->language-info rmp #t)
               [(vector mp name val)
                (define get-info ((dynamic-require mp name) val))
                (get-info 'drracket:submit-predicate #f)]
               [_ #f])))))

;; The command server accepts a single TCP connection at a time.
;;
;; Normally Emacs will make only one connection. If the user exits the
;; REPL, then our entire Racket process exits. (Just in case, we have
;; an accept-a-connection loop below. It handles any exns -- like
;; exn:network -- not handled during command processing. It uses a
;; custodian to clean up.)
;;
;; Command requests and responses "on the wire" are a subset of valid
;; Emacs Lisp s-expressions: See elisp-read and elisp-write.
;;
;; Command requests are (nonce command param ...).
;;
;; A thread is spun off to handle each request, so that a long-running
;; command won't block others. The nonce supplied with the request is
;; returned with the response, so that the client can match the
;; response with the request. The nonce needn't be random, just
;; unique; an increasing integer is fine.
;;
;; Command responses are either (nonce 'ok sexp ...+) or (nonce 'error
;; "message"). The latter normally can and should be displayed to the
;; user in Emacs via error or message. We handle exn:fail? up here;
;; generally we're fine letting Racket exceptions percolate up and be
;; shown to the user
(define (start-command-server port)
  (thread
   (thunk
    (define listener (tcp-listen port 4 #t))
    (let accept-a-connection ()
      (define custodian (make-custodian))
      (parameterize ([current-custodian custodian])
        (with-handlers ([exn:fail? void])
          (define-values (in out) (tcp-accept listener))
          (define response-channel (make-channel))
          (define ((do-command/put-response nonce sexp))
            (channel-put
             response-channel
             (cons
              nonce
              (with-handlers ([exn:fail? (λ (e) `(error ,(exn-message e)))])
                (parameterize ([current-namespace
                                (context-ns command-server-context)])
                  `(ok ,(command sexp command-server-context)))))))
          (define (get/write-response)
            (elisp-writeln (channel-get response-channel) out)
            (flush-output out)
            (get/write-response))
          ;; With all the pieces defined, let's go:
          (thread get/write-response)
          (let read-a-command ()
            (match (elisp-read in)
              [(cons nonce sexp) (thread (do-command/put-response nonce sexp))
                                 (read-a-command)]
              [(? eof-object?)   (void)])))
        (custodian-shutdown-all custodian))
      (accept-a-connection))))
  (void))

(define/contract (make-prompt-read m)
  (-> (or/c #f mod?) (-> any))
  ;; A channel to which a thread puts interactions that it reads using
  ;; the current-read-interaction handler. This can be set by a lang
  ;; from its configure-runtime, so, this should be compatible with
  ;; any lang, even non-sexpr langs.
  (define chan (make-channel))
  (define in ((current-get-interaction-input-port)))
  (define (read-interaction/put-channel)
    (define (read-interaction)
      (with-handlers ([exn:fail? values])
        ((current-read-interaction) (object-name in) in))) ;[^1]
    (match (read-interaction)
      [(? eof-object?) (sync in)] ;[^2]
      [(? exn:fail? e) (channel-put chan e)] ;raise in other thread
      [v (channel-put chan v)])
    (read-interaction/put-channel))
  (thread read-interaction/put-channel)
  ;; The prompt-read handler.
  (define (prompt-read)
    (define v
      ;; Only display prompt when an interaction isn't available very
      ;; soon. A tiny non-zero timeout gives the read-interaction
      ;; thread a chance to run, increasing chance we needn't display
      ;; prompt. See issue #311.
      (match (sync/timeout 0.01 chan)
        [#f
         (display-prompt (maybe-mod->prompt-string m))
         (channel-get chan)]
        [v v]))
    (when (exn:fail? v)
      (raise v))
    v)
  prompt-read)
;; "Footnote" comments about make-prompt-read and many attempts to fix
;; issue #305.
;;
;; [^1]: datalog/lang expects each interaction to be EOF terminated.
;;       This seems to be a DrRacket convention (?). We could make
;;       that work here if we composed open-input-string with
;;       read-line. But that would fail for valid multi-line
;;       expressions in langs like racket/base e.g. "(+ 1\n2)". We
;;       could have Emacs racket-repl-submit append some marker that
;;       lets us know to combine multiple lines here -- but we'd have
;;       to be careful to eat the marker and avoid combining lines
;;       when the user is entering input for their own program that
;;       uses `read-line` etc. Trying to be clever here is maybe not
;;       smart. I _think_ the safest thing is for each lang like
;;       datalog to implement current-read-interaction like it says on
;;       the tin -- it can parse just one expression/statement from a
;;       normal, "infinite" input port; if that means the lang parser
;;       has to be tweaked for a single-expression/statement mode of
;;       usage, so be it.
;;
;; [^2]: The eof-object? clause is here only for datalog/lang
;;       configure-runtime.rkt. Its `the-read` returns eof if
;;       char-ready? is false. WAT. Why doesn't it just block like a
;;       normal read-interaction handler? Catch this and wait for more
;;       input to be available before calling it again.

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

(define/contract (command sexpr the-context)
  (-> pair? context? any/c)
  (match-define (context _ns maybe-mod md5 submit-pred) the-context)
  (define-values (dir file mod-path) (maybe-mod->dir/file/rmp maybe-mod))
  (define path (and dir file (build-path dir file)))
  ;; Note: Intentionally no "else" match clause -- let caller handle
  ;; exn and supply a consistent exn response format.
  (match sexpr
    [`(run ,what ,mem ,pp? ,ctx ,args) (run what mem pp? ctx args)]
    [`(path+md5)                       (cons (or path 'top) md5)]
    [`(syms)                           (syms)]
    [`(def ,str)                       (find-definition str)]
    [`(mod ,sym)                       (find-module sym maybe-mod)]
    [`(describe ,str)                  (describe str)]
    [`(doc ,str)                       (doc str)]
    [`(type ,v)                        (type v)]
    [`(macro-stepper ,str ,into-base?) (macro-stepper str into-base?)]
    [`(macro-stepper/next)             (macro-stepper/next)]
    [`(requires/tidy ,reqs)            (requires/tidy reqs)]
    [`(requires/trim ,path-str ,reqs)  (requires/trim path-str reqs)]
    [`(requires/base ,path-str ,reqs)  (requires/base path-str reqs)]
    [`(find-collection ,str)           (find-collection str)]
    [`(get-profile)                    (get-profile)]
    [`(get-uncovered)                  (get-uncovered path)]
    [`(check-syntax ,path-str)         (check-syntax path-str)]
    [`(eval ,v)                        (eval-command v)]
    [`(repl-submit? ,str ,eos?)        (repl-submit? submit-pred str eos?)]
    [`(exit)                           (exit)]))

;;; read/write Emacs Lisp values

(define (elisp-read in)
  (elisp->racket (read in)))

(define (elisp-writeln v out)
  (elisp-write v out)
  (newline out))

(define (elisp-write v out)
  (write (racket->elisp v) out))

(define elisp-bool/c (or/c #t '()))
(define (as-racket-bool v)
  ;; elisp->racket "de-puns" 'nil as '() -- not #f. Use this helper to
  ;; treat as a boolean.
  (and v (not (null? v))))

(define (elisp->racket v)
  (match v
    ['nil             '()] ;not #f -- see as-racket-bool
    ['t               #t]
    [(? list? xs)     (map elisp->racket xs)]
    [(cons x y)       (cons (elisp->racket x) (elisp->racket y))]
    [(vector s _ ...) s] ;Emacs strings can be #("string" . properties)
    [v                v]))

(define (racket->elisp v)
  (match v
    [(or #f (list)) 'nil]
    [#t             't]
    [(? list? xs)   (map racket->elisp xs)]
    [(cons x y)     (cons (racket->elisp x) (racket->elisp y))]
    [(? path? v)    (path->string v)]
    [v              v]))

(module+ test
  (check-equal? (with-output-to-string
                  (λ () (elisp-write '(1 #t nil () (a . b))
                                     (current-output-port))))
                "(1 t nil nil (a . b))"))

;;; commands

(define/contract (run what mem pp ctx args)
  (-> list? number? elisp-bool/c context-level? list?
      list?)
  (define ready-channel (make-channel))
  (channel-put message-to-main-thread-channel
               (rerun (->mod/existing what)
                      mem
                      (as-racket-bool pp)
                      ctx
                      (list->vector args)
                      (λ () (channel-put ready-channel what))))
  ;; Waiting for this allows the command response to be used as the
  ;; all-clear for additional commands that need the module load to be
  ;; done and entering a REPL for that module. For example, to compose
  ;; run with get-profile or get-uncovered.
  (sync ready-channel))

(define/contract (repl-submit? submit-pred text eos)
  (-> (or/c #f drracket:submit-predicate/c) string? elisp-bool/c (or/c 'default #t #f))
  (if submit-pred
      (submit-pred (open-input-string text) (as-racket-bool eos))
      'default))

(define (syms)
  (sort (map symbol->string (namespace-mapped-symbols))
        string<?))

(define/contract (find-module str maybe-mod)
  (-> string? (or/c #f mod?)
      (or/c #f (list/c path-string? number? number?)))
  (define-values (dir _file maybe-rmp) (maybe-mod->dir/file/rmp maybe-mod))
  (parameterize ([current-load-relative-directory dir])
    (or (mod-loc str maybe-rmp)
        (mod-loc (string->symbol str) maybe-rmp))))

(define (mod-loc v maybe-rmp)
  (match (with-handlers ([exn:fail? (λ _ #f)])
           (resolve-module-path v maybe-rmp))
    [(? path-string? path)
     #:when (file-exists? path)
     (list (path->string path) 1 0)]
    [_ #f]))

(module+ test
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (let* ([here             (simplify-path here)] ;nuke trailing dot
         ;; Examples of finding relative and absolute:
         [run.rkt          (path->string (build-path here "run.rkt"))]
         [pe-racket/string (pregexp "collects/racket/string.rkt$")])
    ;; Examples of having no current module (i.e. plain racket/base
    ;; REPL) and having one ("cmds.rkt").
    (let ([mod #f])
     (parameterize ([current-directory here])
       (check-match (find-module "run.rkt" mod)
                    (list (== run.rkt) 1 0))
       (check-match (find-module "racket/string" mod)
                    (list pe-racket/string 1 0))))
    (let ([mod (->mod/existing (build-path here "cmds.rkt"))])
      (check-match (find-module "run.rkt" mod)
                   (list (== run.rkt) 1 0))
      (check-match (find-module "racket/string" mod)
                   (list pe-racket/string 1 0)))))

(define (type v)
  (type-or-sig v))

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

(define/contract (describe str)
  (-> string? string?)
  (define stx (namespace-symbol->identifier (string->symbol str)))
  (or (scribble-doc/html stx)
      (sig-and/or-type stx)))

;;; doc / help

(define/contract (doc str)
  (-> string? #t)
  (or (find-help (namespace-symbol->identifier (string->symbol str)))
      (perform-search str))
  #t)

;;; macro-stepper

(define step-thunk/c (-> (cons/c (or/c 'original string? 'final) string?)))
(define step-thunk #f)

(define/contract (make-expr-stepper str)
  (-> string? step-thunk/c)
  (define step-num #f)
  (define last-stx (string->namespace-syntax str))
  (define (step)
    (cond [(not step-num)
           (set! step-num 0)
           (cons 'original (pretty-format-syntax last-stx))]
          [else
           (define this-stx (expand-once last-stx))
           (cond [(not (equal? (syntax->datum last-stx)
                               (syntax->datum this-stx)))
                  (begin0
                      (cons (~a step-num ": expand-once")
                            (diff-text (pretty-format-syntax last-stx)
                                       (pretty-format-syntax this-stx)
                                       #:unified 3))
                    (set! last-stx this-stx))]
                 [else
                  (cons 'final (pretty-format-syntax this-stx))])]))
  step)

(define/contract (make-file-stepper path into-base?)
  (-> (and/c path-string? absolute-path?) boolean? step-thunk/c)
  ;; If the dynamic-require fails, just let it bubble up.
  (define stepper-text (dynamic-require 'macro-debugger/stepper-text 'stepper-text))
  (define stx (file->syntax path))
  (define-values (dir _name _dir) (split-path path))
  (define raw-step (parameterize ([current-load-relative-directory dir])
                     (stepper-text stx
                                   (if into-base? (const #t) (not-in-base)))))
  (define step-num #f)
  (define step-last-after "")
  (define/contract (step) step-thunk/c
    (cond [(not step-num)
           (set! step-num 0)
           (cons 'original
                 (pretty-format-syntax stx))]
          [else
           (define out (open-output-string))
           (parameterize ([current-output-port out])
             (cond [(raw-step 'next)
                    (set! step-num (add1 step-num))
                    (match-define (list title before after)
                      (step-parts (get-output-string out)))
                    (set! step-last-after after)
                    (cons (~a step-num ": " title)
                          (diff-text before after #:unified 3))]
                   [else
                    (cons 'final step-last-after)]))]))
  step)

(define/contract (macro-stepper what into-base?)
  (-> (or/c (cons/c 'expr string?) (cons/c 'file path-string?)) elisp-bool/c
      (cons/c 'original string?))
  (set! step-thunk
        (match what
          [(cons 'expr str)  (make-expr-stepper str)]
          [(cons 'file path) (make-file-stepper path (as-racket-bool into-base?))]))
  (macro-stepper/next))

(define/contract (macro-stepper/next)
  (-> (cons/c (or/c 'original 'final string?) string?))
  (unless step-thunk
    (error 'macro-stepper "Nothing to expand"))
  (define v (step-thunk))
  (when (eq? 'final (car v))
    (set! step-thunk #f))
  v)

;; Borrowed from xrepl.
(define not-in-base
  (λ () (let ([base-stxs #f])
          (unless base-stxs
            (set! base-stxs ; all ids that are bound to a syntax in racket/base
                  (parameterize ([current-namespace (make-base-namespace)])
                    (let-values ([(vals stxs) (module->exports 'racket/base)])
                      (map (λ (s) (namespace-symbol->identifier (car s)))
                           (cdr (assq 0 stxs)))))))
          (λ (id) (not (ormap (λ (s) (free-identifier=? id s)) base-stxs))))))

(define (step-parts str)
  (match str
    [(pregexp "^(.+?)\n(.+?)\n +==>\n(.+?)\n+$"
              (list _ title before after))
     (list title before after)]))

(define (diff-text before-text after-text #:unified [-U 3])
  (define template "racket-mode-syntax-diff-~a")
  (define (make-temporary-file-with-text str)
    (define file (make-temporary-file template))
    (with-output-to-file file #:mode 'text #:exists 'replace
      (λ () (displayln str)))
    file)
  (define before-file (make-temporary-file-with-text before-text))
  (define after-file  (make-temporary-file-with-text after-text))
  (define out (open-output-string))
  (begin0 (parameterize ([current-output-port out])
            (system (format "diff -U ~a ~a ~a" -U before-file after-file))
            (match (get-output-string out)
              ["" " <empty diff>\n"]
              [(pregexp "\n(@@.+@@\n.+)$" (list _ v)) v]))
    (delete-file before-file)
    (delete-file after-file)))

(define (pretty-format-syntax stx)
  (pretty-format #:mode 'write (syntax->datum stx)))

;;; eval-commmand

(define/contract (eval-command str)
  (-> string? string?)
  (define results
    (call-with-values (λ ()
                        ((current-eval) (string->namespace-syntax str)))
                      list))
  (string-join (map ~v results) "\n"))

;;; requires

;; requires/tidy : (listof require-sexpr) -> require-sexpr
(define (requires/tidy reqs)
  (let* ([reqs (combine-requires reqs)]
         [reqs (group-requires reqs)])
    (require-pretty-format reqs)))

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
    (require-pretty-format reqs)))

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
    (require-pretty-format reqs)))

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

(define/contract (find-collection str)
  (-> path-string? (or/c 'find-collection-not-installed #f (listof string?)))
  (define fcd (with-handlers ([exn:fail:filesystem:missing-module?
                               (λ _ (error 'find-collection
                                           "For this to work, you need to `raco pkg install raco-find-collection`."))])
                (dynamic-require 'find-collection/find-collection
                                 'find-collection-dir)))
  (map path->string (fcd str)))

;;; profile

(define (get-profile)
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
               (+ (syntax-position stx) (syntax-span stx))))))

;;; coverage

(define (get-uncovered file)
  (consolidate-coverage-ranges
   (for*/list ([x (in-list (get-test-coverage-info))]
               [covered? (in-value (first x))]
               #:when (not covered?)
               [src (in-value (second x))]
               #:when (equal? file src)
               [pos (in-value (third x))]
               [span (in-value (fourth x))])
     (cons pos (+ pos span)))))

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
                           #:catch exn:fail? _ (λ _ 'not-supported))])
    ;; Note: Adjust all positions to 1-based Emacs `point' values.
    (λ (path-str)
      (define path (string->path path-str))
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
        (append infos defs/uses)))))

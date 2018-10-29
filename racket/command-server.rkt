#lang racket/base

(require racket/contract
         racket/format
         racket/function
         racket/lazy-require
         racket/match
         racket/set
         racket/tcp
         "channel.rkt"
         "debug.rkt"
         "elisp.rkt"
         "interactions.rkt"
         "md5.rkt"
         "mod.rkt"
         "util.rkt")

(lazy-require
 ["commands/check-syntax.rkt" (check-syntax)]
 ["commands/coverage.rkt"     (get-uncovered)]
 ["commands/describe.rkt"     (describe type)]
 ["commands/find-module.rkt"  (find-module)]
 ["commands/help.rkt"         (doc)]
 ["commands/macro.rkt"        (macro-stepper macro-stepper/next)]
 ["commands/profile.rkt"      (get-profile)]
 ["commands/requires.rkt"     (requires/tidy requires/trim requires/base)]
 ["find.rkt"                  (find-definition)])

(provide start-command-server
         attach-command-server
         make-prompt-read)

(define drracket:submit-predicate/c (-> input-port? boolean? boolean?))

(define-struct/contract context
  ([ns          namespace?]
   [maybe-mod   (or/c #f mod?)]
   [md5         string?]
   [submit-pred (or/c #f drracket:submit-predicate/c)]))

(define command-server-context (context (make-base-namespace) #f "" #f))

(define/contract (attach-command-server ns maybe-mod)
  (-> namespace? (or/c #f mod?) any)
  (set-debug-repl-namespace! ns)
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
;; Immediately after connecting, the client must send us exactly the
;; same '(accept ,random-value) value that it gave us as a command
;; line argument when it started us. Else we exit. See issue #327.
;;
;; Normally Emacs will make only one connection to us, ever. If the
;; user exits the REPL, then our entire Racket process exits. (Just in
;; case, we have an accept-a-connection loop below. It handles any
;; exns -- like exn:network -- not handled during command processing.
;; It uses a custodian to clean up.)
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
(define (start-command-server port launch-token)
  (thread
   (thunk
    (define listener (tcp-listen port 4 #t "127.0.0.1"))
    (let accept-a-connection ()
      (define custodian (make-custodian))
      (parameterize ([current-custodian custodian])
        (with-handlers ([exn:fail? void]) ;just disconnect; see #327
          (define-values (in out) (tcp-accept listener))
          (unless (or (not launch-token)
                      (equal? launch-token (elisp-read in)))
            (display-commented "Authorization failed; exiting")
            (exit 1)) ;see #327
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
            (elisp-writeln (sync response-channel
                                 debug-notify-channel)
                           out)
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

(define/contract ((make-prompt-read m))
  (-> (or/c #f mod?) (-> any))
  (begin0 (get-interaction (maybe-mod->prompt-string m))
    (next-break 'all))) ;let debug-instrumented code break again

(define/contract (command sexpr the-context)
  (-> pair? context? any/c)
  (match-define (context _ns maybe-mod md5 submit-pred) the-context)
  (define-values (dir file mod-path) (maybe-mod->dir/file/rmp maybe-mod))
  (define path (and dir file (build-path dir file)))
  ;; Note: Intentionally no "else" match clause -- let caller handle
  ;; exn and supply a consistent exn response format.
  (match sexpr
    [`(run ,what ,mem ,pp? ,ctx ,args ,dbg ,skel?)
     (run what mem pp? ctx args dbg skel?)]
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
    [`(debug-eval ,src ,l ,c ,p ,code) (debug-eval src l c p code)]
    [`(debug-resume ,v)                (debug-resume v)]
    [`(debug-disable)                  (debug-disable)]
    [`(exit)                           (exit)]))

;;; A few commands defined here

(define/contract (run what mem pp ctx args dbgs skel)
  (-> list? number? elisp-bool/c context-level? list? (listof path-string?) elisp-bool/c
      list?)
  (define ready-channel (make-channel))
  (channel-put message-to-main-thread-channel
               (rerun (->mod/existing what)
                      mem
                      (as-racket-bool pp)
                      ctx
                      (list->vector args)
                      (list->set (map string->path dbgs))
                      (as-racket-bool skel)
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

;;; eval-commmand

(define/contract (eval-command str)
  (-> string? string?)
  (define results
    (call-with-values (λ ()
                        ((current-eval) (string->namespace-syntax str)))
                      list))
  (~a (map ~v results) "\n"))

;;; find-collection

(define/contract (find-collection str)
  (-> path-string? (or/c 'find-collection-not-installed #f (listof string?)))
  (define fcd (with-handlers ([exn:fail:filesystem:missing-module?
                               (λ _ (error 'find-collection
                                           "For this to work, you need to `raco pkg install raco-find-collection`."))])
                (dynamic-require 'find-collection/find-collection
                                 'find-collection-dir)))
  (map path->string (fcd str)))

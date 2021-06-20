#lang racket/base

(require racket/contract
         racket/format
         racket/lazy-require
         racket/match
         "debug.rkt"
         "elisp.rkt"
         (only-in "instrument.rkt" get-uncovered get-profile)
         "logger.rkt"
         "mod.rkt"
         "repl.rkt"
         "repl-session.rkt"
         (only-in "scribble.rkt" libs-exporting-documented)
         "util.rkt")

(lazy-require
 ["commands/check-syntax.rkt" (check-syntax)]
 ["commands/describe.rkt"     (describe type)]
 ["commands/find-module.rkt"  (find-module)]
 ["commands/help.rkt"         (doc)]
 ["commands/macro.rkt"        (macro-stepper macro-stepper/next)]
 ["commands/requires.rkt"     (requires/tidy requires/trim requires/base)]
 ["commands/module-names.rkt" (module-names)]
 ["find.rkt"                  (find-definition find-definition/drracket-jump)])

(provide command-server-loop)

;; Command requests and responses are a subset of valid Emacs Lisp
;; s-expressions: See elisp-read and elisp-write.
;;
;; Command requests are (nonce session-id command param ...).
;;
;; `session-id` should be a REPL session ID returned from opening a
;; new connection to the REPL server, for commands that need to be
;; associated with a specific REPL session. (For other commands, this
;; may be nil a.k.a. #f).
;;
;; A thread is spun off to handle each request, so that a long-running
;; command won't block others. The nonce supplied with the request is
;; returned with the response, so that the client can match the
;; response with the request. The nonce needn't be random, just
;; unique; an increasing integer is fine.
;;
;; Command responses are (nonce 'ok sexp ...+) or (nonce 'error
;; "message") or (nonce 'break). The 'error response normally can and
;; should be displayed to the user in Emacs via error or message. We
;; handle exn:fail? up here; generally we're fine letting Racket
;; exceptions percolate up and be shown to the user. The 'break
;; response is for commands that can be aborted by other commands.
;; Typically our Emacs code will silently ignore these; the
;; affirmative break response allows the command callback to be
;; cleaned up.

(define (command-server-loop in out)
  ;; Because we have multiple command threads running, we should
  ;; synchronize writing responses to the output port. To do so, we
  ;; use a channel. Threads running `do-command/queue-response` put to
  ;; the channel. The `write-reponses-forever` thread empties it.
  (define response-channel (make-channel))

  (define (do-command/queue-response nonce sid sexp)
    (define (thk)
      (channel-put
       response-channel
       (cons
        nonce
        (with-handlers ([exn:fail?  (λ (e) `(error ,(exn-message e)))]
                        [exn:break? (λ (e) `(break))])
          `(ok ,(call-with-session-context sid command sexp))))))
    ;; Make "label" for logging. A thread name comes from its thunk ∴
    ;; renaming the thunk lets us log the thread more informatively.
    (define label (command-invocation-label nonce sid sexp))
    (log-racket-mode-info label)
    (procedure-rename thk (string->symbol label)))

  (define (write-responses-forever)
    (elisp-writeln (sync response-channel
                         logger-notify-channel
                         debug-notify-channel)
                   out)
    (flush-output out)
    (write-responses-forever))

  ;; With all the pieces defined, let's go:
  (thread write-responses-forever)
  (elisp-writeln `(ready) out)
  (let read-a-command ()
    (match (elisp-read in)
      [(list* nonce sid sexp) (thread (do-command/queue-response nonce sid sexp))
                              (read-a-command)]
      [(? eof-object?)        (void)]))  )

(define (command-invocation-label nonce sid sexp)
  (~v
   (list nonce
         (if (null? sid) "*" sid)
         (let limit-strings ([v sexp])
           (cond [(list? v)   (map limit-strings v)]
                 [(string? v) (~a #:max-width 80 #:limit-marker "⋯" v)]
                 [else        v])))))

(define/contract (command sexpr)
  (-> pair? any/c)
  (define-values (dir file mod-path) (maybe-mod->dir/file/rmp
                                      (current-session-maybe-mod)))
  (define path (and dir file (build-path dir file)))
  ;; Note: Intentionally no "else" match clause -- let caller handle
  ;; exn and supply a consistent exn response format.
  (match sexpr
    ;; Currently, we're called from `call-with-session-context` which
    ;; uses the possibly non-nil session id to look up the possible
    ;; REPL session, and set some parameters. That's because I chose
    ;; to make the session ID an additional "prefix" parameter for ALL
    ;; commands, like the nonce, and just after the nonce (see above).
    ;; That was convenient to let call-with-session-context wrap
    ;; everything, and not fiddle with individual commands. However.
    ;; Only _some_ commands need a valid session ID. It might be
    ;; clearer (if more tedious) to make that be an explicit new
    ;; argument for only such commands. And for those commands that
    ;; already have a "how" argument, instead of supplying 'namespace,
    ;; they would supply the session ID. Just in case I do that,
    ;; someday, I'm grouping the commands in these three categories,
    ;; below.

    ;; Commands that do NOT need a REPL session
    [`(no-op)                          #t]
    [`(logger ,v)                      (channel-put logger-command-channel v)]
    [`(repl-tcp-port-number)           repl-tcp-port-number]
    [`(check-syntax ,path-str ,code)   (check-syntax path-str code)]
    [`(macro-stepper ,str ,into-base?) (macro-stepper str into-base?)]
    [`(macro-stepper/next ,what)       (macro-stepper/next what)]
    [`(find-collection ,str)           (find-collection str)]
    [`(module-names)                   (module-names)]
    [`(requires/tidy ,reqs)            (requires/tidy reqs)]
    [`(requires/trim ,path-str ,reqs)  (requires/trim path-str reqs)]
    [`(requires/base ,path-str ,reqs)  (requires/base path-str reqs)]
    [`(requires/find ,str)             (libs-exporting-documented str)]

    ;; Commands that MIGHT need a REPL session for context (e.g. its
    ;; namespace), if their first "how" argument is 'namespace.
    [`(def ,how ,str)                  (find-definition how str)]
    [`(def/drr ,how ,path ,subs ,ids)  (find-definition/drracket-jump how path subs ids)]
    [`(describe ,how ,str)             (describe how str)]
    [`(doc ,how ,str)                  (doc how str)]
    [`(type ,how ,v)                   (type how v)]

    ;; Commands that DEFINITELY DO need a REPL session for context,
    ;; e.g. its namespace. Should they pass a session-id explicitly,
    ;; now?
    [`(run ,what ,subs ,mem ,pp? ,cols ,pix/char ,ctx ,args ,dbg)
     (run what subs mem pp? cols pix/char ctx args dbg)]
    [`(path)                           (or path 'top)]
    [`(syms)                           (syms)]
    [`(mod ,sym)                       (find-module sym (current-session-maybe-mod))]
    [`(get-profile)                    (get-profile)]
    [`(get-uncovered)                  (get-uncovered path)]
    [`(eval ,v)                        (eval-command v)]
    [`(repl-submit? ,str ,eos?)        (repl-submit? str eos?)]
    [`(debug-eval ,src ,l ,c ,p ,code) (debug-eval src l c p code)]
    [`(debug-resume ,v)                (debug-resume v)]
    [`(debug-disable)                  (debug-disable)]
    [`(break ,kind)                    (break-repl-thread (current-session-id) kind)]
    [`(repl-zero-column)               (repl-zero-column)]))

;;; A few commands defined here

(define/contract (repl-submit? text eos)
  (-> string? elisp-bool/c (or/c 'default #t #f))
  (if (current-session-submit-pred)
      ((current-session-submit-pred) (open-input-string text) (as-racket-bool eos))
      'default))

(define (syms)
  (sort (map symbol->string (namespace-mapped-symbols))
        string<?))

;;; eval-commmand

(define/contract (eval-command str)
  (-> string? string?)
  (call-with-values (λ ()
                      ((current-eval) (string->namespace-syntax str)))
                    (λ vs
                      (apply ~a #:separator "\n" (map ~v vs)))))

;;; find-collection

(define-polyfill (find-collection-dir str)
  #:module find-collection/find-collection
  (error 'find-collection-dir
         "For this to work, you need to `raco pkg install raco-find-collection`"))

(define/contract (find-collection str)
  (-> path-string? (listof string?))
  (map path->string (find-collection-dir str)))

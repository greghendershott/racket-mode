#lang racket/base

(require racket/contract
         racket/format
         racket/lazy-require
         racket/match
         "debug.rkt"
         "elisp.rkt"
         "mod.rkt"
         "repl.rkt"
         "util.rkt")

(lazy-require
 ["commands/check-syntax.rkt" (check-syntax)]
 ["commands/coverage.rkt"     (get-uncovered)]
 ["commands/describe.rkt"     (describe
                               type)]
 ["commands/find-module.rkt"  (find-module)]
 ["commands/help.rkt"         (doc)]
 ["commands/macro.rkt"        (macro-stepper macro-stepper/next)]
 ["commands/profile.rkt"      (get-profile)]
 ["commands/requires.rkt"     (requires/tidy requires/trim requires/base)]
 ["find.rkt"                  (find-definition
                               find-definition/drracket-jump)])

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
  (define ((do-command/queue-response nonce sid sexp))
    (log-racket-mode-info "(~v ~v ~v)" nonce sid sexp)
    (channel-put
     response-channel
     (cons
      nonce
      (with-handlers ([exn:fail?  (λ (e) `(error ,(exn-message e)))]
                      [exn:break? (λ (e) `(break))])
        `(ok ,(call-with-session-context sid command sexp))))))
  (define (write-responses-forever)
    (elisp-writeln (sync response-channel
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
    [`(check-syntax ,path-str ,code)   (check-syntax path-str code)]
    [`(macro-stepper ,str ,into-base?) (macro-stepper str into-base?)]
    [`(macro-stepper/next)             (macro-stepper/next)]
    [`(find-collection ,str)           (find-collection str)]
    [`(requires/tidy ,reqs)            (requires/tidy reqs)]
    [`(requires/trim ,path-str ,reqs)  (requires/trim path-str reqs)]
    [`(requires/base ,path-str ,reqs)  (requires/base path-str reqs)]

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
    [`(run ,what ,mem ,pp? ,ctx ,args ,dbg)
     (run what mem pp? ctx args dbg)]
    [`(path+md5)                       (cons (or path 'top) (current-session-md5))]
    [`(syms)                           (syms)]
    [`(mod ,sym)                       (find-module sym (current-session-maybe-mod))]
    [`(get-profile)                    (get-profile)]
    [`(get-uncovered)                  (get-uncovered path)]
    [`(eval ,v)                        (eval-command v)]
    [`(repl-submit? ,str ,eos?)        (repl-submit? str eos?)]
    [`(debug-eval ,src ,l ,c ,p ,code) (debug-eval src l c p code)]
    [`(debug-resume ,v)                (debug-resume v)]
    [`(debug-disable)                  (debug-disable)]
    [`(exit)                           (exit-repl-session (current-session-id))]))

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

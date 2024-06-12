;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/format
         racket/lazy-require
         racket/match
         "debug.rkt"
         "elisp.rkt"
         (only-in "instrument.rkt" get-uncovered get-profile)
         "hash-lang-bridge.rkt"
         "logger.rkt"
         "package.rkt"
         "repl.rkt"
         "repl-output.rkt"
         "repl-session.rkt"
         (only-in "scribble.rkt"
                  doc-index-names
                  doc-index-lookup
                  libs-exporting-documented)
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
    ;; Make "label" for logging. A thread name comes from its thunk ∴
    ;; renaming the thunk lets us log the thread more informatively.
    (define label (command-invocation-label nonce sid sexp))
    (define (thk)
      (channel-put
       response-channel
       (cons
        nonce
        (with-handlers ([exn:fail?  (λ (e) `(error ,(exn-message e)))]
                        [exn:break? (λ (e) `(break))])
          (with-time/log label
           `(ok ,(call-with-session-context sid command sexp)))))))
    (procedure-rename thk (string->symbol label)))

  (define (write-responses-and-notifications)
    (parameterize ([current-output-port out])
      (let loop ()
        (elisp-writeln (sync response-channel
                             repl-output-channel
                             logger-notify-channel
                             debug-notify-channel
                             hash-lang-notify-channel
                             package-notify-channel))
        (flush-output)
        (loop))))

  ;; With all the pieces defined, let's go:
  (thread write-responses-and-notifications)
  (parameterize ([current-output-port out])
    (elisp-writeln `(ready)))
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
  (define file (maybe-module-path->file (current-session-maybe-mod)))
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
    [`(check-syntax ,path-str ,code)   (check-syntax path-str code)]
    [`(macro-stepper ,path ,str ,pol)  (macro-stepper path str pol)]
    [`(macro-stepper/next ,what)       (macro-stepper/next what)]
    [`(module-names)                   (module-names)]
    [`(requires/tidy ,reqs)            (requires/tidy reqs)]
    [`(requires/trim ,path-str ,reqs)  (requires/trim path-str reqs)]
    [`(requires/base ,path-str ,reqs)  (requires/base path-str reqs)]
    [`(requires/find ,str)             (libs-exporting-documented str)]
    [`(doc-index-names)                (doc-index-names)]
    [`(doc-index-lookup ,str)          (doc-index-lookup str)]
    [`(hash-lang . ,more)              (apply hash-lang more)]
    [`(pkg-list)                       (package-list)]
    [`(pkg-details ,str)               (package-details str)]
    [`(pkg-op ,verb ,name)             (package-op verb name)]
    [`(pkg-doc-link ,name)             (catalog-package-doc-link name)]

    ;; Commands that MIGHT need a REPL session for context (e.g. its
    ;; namespace), if their first "how" argument is 'namespace.
    [`(def ,how ,str)                  (find-definition how str)]
    [`(def/drr ,how ,path ,subs ,ids)  (find-definition/drracket-jump how path subs ids)]
    [`(describe ,how ,str)             (describe how str)]
    [`(doc ,how ,str)                  (doc how str)]
    [`(type ,how ,v)                   (type how v)]
    [`(repl-start, sid)                (repl-start sid)]

    ;; Commands that DEFINITELY DO need a REPL session for context,
    ;; e.g. its namespace. Should they pass a session-id explicitly,
    ;; now?
    [`(run ,what ,subs ,mem ,pp? ,cols ,pix/char ,ctx ,args ,dbg)
     (run what subs mem pp? cols pix/char ctx args dbg)]
    [`(path)                           (or file 'top)]
    [`(syms)                           (syms)]
    [`(mod ,sym)                       (find-module sym (current-session-maybe-mod))]
    [`(get-profile)                    (get-profile)]
    [`(get-uncovered)                  (get-uncovered file)]
    [`(eval ,v)                        (eval-command v)]
    [`(debug-resume ,v)                (debug-resume v)]
    [`(debug-disable)                  (debug-disable)]
    [`(repl-input ,str)                (repl-input str)]
    [`(repl-submit ,str)               (repl-submit str)]
    [`(repl-break)                     (repl-break)]
    [`(repl-exit)                      (repl-exit)]))

;;; Some trivial commands defined here

(define (syms)
  (sort (map symbol->string (namespace-mapped-symbols))
        string<?))

(define/contract (eval-command str)
  (-> string? string?)
  (call-with-values (λ ()
                      ((current-eval) (string->namespace-syntax str)))
                    (λ vs
                      (apply ~a #:separator "\n" (map ~v vs)))))

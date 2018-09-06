#lang racket/base

(require racket/match
         "fresh-line.rkt")

(provide current-sync/yield
         get-interaction)

;; A channel to which a thread puts interactions that it reads using
;; the current-read-interaction handler (which can be set by a lang
;; from its configure-runtime, so, this should be compatible with
;; any lang, even non-sexpr langs).
;;
;; This is its own thread and channel for a couple reasons:
;;
;; - Issue #311. A consumer can use sync/timeout to avoid displaying a
;;   prompt when multiple interactions are waiting.
;;
;; - Debugging. We can switch from the normal REPL to a debugger REPL,
;;   without input being stuck inside a read call for the former.
;;
;; One wrinkle is we need to be careful about calling yield instead of
;; sync when the gui is active. See issue #326.

;; FIXME??: This used to be under the REPL custodian. Is it OK for it
;; _not_ to be, now? For instance what if user runs another file, but
;; this is still using the previous current-read-interaction value?
(define chan (make-channel))

(define (read-interaction/put-channel)
  (define in ((current-get-interaction-input-port)))
  (define (read-interaction)
    (with-handlers ([exn:fail? values])
      ((current-read-interaction) (object-name in) in))) ;[^1]
  (match (read-interaction)
    [(? eof-object?) (sync in)] ;[^2]
    [(? exn:fail? e) (channel-put chan e)] ;raise in other thread
    [v (channel-put chan v)])
  (read-interaction/put-channel))

(void (thread read-interaction/put-channel))

(define current-sync/yield (make-parameter sync)) ;see issue #326

(define (get-interaction prompt)
  (match (or (sync/timeout 0.01 chan) ;see issue #311
             (begin (display-prompt prompt)
                    ((current-sync/yield) chan)))
    [(? exn:fail? exn) (raise exn)]
    [v v]))

(define (display-prompt str)
  (flush-output (current-error-port))
  (fresh-line)
  (display str)
  (display "> ")
  (flush-output)
  (zero-column!))

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

#lang racket/base

(require racket/match
         racket/set
         syntax/parse/define
         "util.rkt")

(provide reset!
         get)

;;; online-check-syntax logger monitor

;; There exists a protocol for macros to communicate tooltips to
;; DrRacket via a log-message to the logger 'online-check-syntax. This
;; might seem strange, but one motivation for this protocol is that
;; e.g. a type-checker might learn things _during_ expansion that it
;; would like to show the user -- even if expansion fails.
;;
;; A consideration, for us, is that we cache fully-expanded syntax.
;; Therefore we also need to cache these these logger messages -- they
;; might occur long before our `check-syntax` command is called. To do
;; so our log receiver monitor runs all the time, storing messages in
;; a hash-table where the key is the syntax-source, and the value is
;; simply (set (list beg end string-or-thunk))).
;;
;; Rather than complicate the cache in syntax.rkt, for now I think
;; it's less-worse to keep our own hash-table, here, and have
;; syntax.rkt call `reset!` whenever it invalidates its cache so we
;; know to do same here.
;;
;; Note: When string-or-thunk is the latter, we record it as such.
;; Only force if/when `get` is called, e.g. when our check-syntax is
;; run. [Perhaps in DrRacket this could be delayed even further --
;; until a tooltip would actually be _displayed_. Because we must
;; marshal data to the Emacs front end, we must force sooner. At least
;; delay as long as we can. Not sure if that really helps but it seems
;; simple enough to do so here.]

(define ht (make-hash)) ;(hash/c path? (set/c (list/c nat nat (or/c string? (-> string?))

(define sema (make-semaphore 1))

(define-simple-macro (with-sema e:expr ...+)
  (call-with-semaphore sema (λ () e ...)))

(define (reset! src)
  (with-sema (hash-remove! ht src)))

(define (record! src beg end string-or-thunk)
  (with-sema
    (hash-update! ht
                  src
                  (λ (v) (set-add v
                                  (list beg end string-or-thunk)))
                  (set))))

(define (get src)
  (sleep 0) ;yield to let receiver-thread handle any pending log events
  (with-sema (hash-ref ht src (set))))

(define (receiver-thread)
  (define receiver (make-log-receiver (current-logger)
                                      'info 'online-check-syntax))
  (for ([event (in-producer sync 'n/a receiver)])
    (match-define (vector _level _message stxs _topic) event)
    (for ([stx (in-list stxs)])
      (let walk ([v (syntax-property stx 'mouse-over-tooltips)])
        (match v
          ;; "The value of the 'mouse-over-tooltips property is
          ;; expected to be to be a tree of cons pairs (in any
          ;; configuration)..."
          [(cons v more)
           (walk v)
           (walk more)]
          ;; "...whose leaves are either ignored or are vectors of the
          ;; shape:"
          [(vector (? syntax? stx)
                   (? exact-positive-integer? beg)
                   (? exact-positive-integer? end)
                   (or (? string? string-or-thunk)
                       (? procedure? string-or-thunk)))
           (record! (syntax-source stx)
                    beg
                    end
                    string-or-thunk)]
          ;; Expected; quietly ignore
          [(or (list) #f) (void)]
          ;; Unexpected; log warning and ignore
          [v (log-racket-mode-warning "unknown online-check-syntax ~v" v)
            (void)])))))

(void (thread receiver-thread))

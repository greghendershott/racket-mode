#lang racket/base

(require racket/list
         racket/match
         racket/path
         racket/set
         syntax/modread)

(provide check-syntax)

;; Our front-end issues check-syntax requests after the user edits a
;; buffer, plus a short idle delay (e.g. 1 second).
;;
;; drracket/check-syntax can take even tens of seconds to complete on
;; even moderately complex inputs.
;;
;; As a result, we might be called to work on a file for which a
;; previous command thread is still running. Although that will work
;; _correctly_, eventually, it is wasteful -- both here in the
;; back-end (calculating) and in the front-end (updating buffer
;; properties).

;; Instead: We'd like to kill the old command thread and inform our
;; front-end that the old command was "canceled". We can do so here
;; simply by `break`ing the old thread. See how command-server.rkt
;; handles exn:break by returning a `(break)` response, and, how
;; racket-repl.el handles that by doing nothing (except cleaning up
;; the nonce->callback hash-table).
(define check-syntax
  (let ([sema (make-semaphore 1)]       ;guard concurrent mod of ht
        [ht   (make-hash)])             ;path-str -> thread
    (λ (path-str code-str)
      (dynamic-wind
        (λ () (call-with-semaphore
               sema
               (λ ()
                 (match (hash-ref ht path-str #f)
                   [#f (void)]
                   [t  (break-thread t)])
                 (hash-set! ht path-str (current-thread)))))
        (λ () (do-check-syntax path-str code-str))
        (λ () (call-with-semaphore
               sema
               (λ ()
                 (hash-remove! ht path-str))))))))

(define do-check-syntax
  (let ([show-content
         (with-handlers ([exn:fail? (λ _ 'not-supported)])
           (dynamic-require 'drracket/check-syntax 'show-content))])
    ;; Note: Adjust all positions to 1-based Emacs `point' values.
    (λ (path-str code-str)
      (define path (string->path path-str))
      (parameterize ([current-load-relative-directory (path-only path)])
        (define stx (with-module-reading-parameterization
                      (λ ()
                        (parameterize ([port-count-lines-enabled #t])
                          (read-syntax path
                                       (open-input-string code-str))))))
        (with-handlers ([exn:fail? handle-fail])
          (define xs (remove-duplicates (show-content stx)))
          ;; Extract the add-mouse-over-status items into a list.
          (define infos
            (remove-duplicates
             (filter
              values
              (for/list ([x (in-list xs)])
                (match x
                  [(vector 'syncheck:add-mouse-over-status beg end str)
                   ;; Avoid silly "imported from “\"file.rkt\"”"
                   (define cleansed (regexp-replace* #px"[“””]" str ""))
                   (list 'info (add1 beg) (add1 end) cleansed)]
                  [_ #f])))))
          ;; Consolidate the add-arrow/name-dup items into a hash table
          ;; with one item per definition. The key is the definition
          ;; position. The value is the set of its uses.
          ;;
          ;; Note: We only want items where the `require-arrow?` member
          ;; is #f, meaning a local definition -- not items imported by
          ;; a require or module language.
          (define ht-defs/uses (make-hash))
          (for ([x (in-list xs)])
            (match x
              [(or (vector 'syncheck:add-arrow/name-dup
                           def-beg def-end
                           use-beg use-end
                           _ _ #f _)
                   (vector 'syncheck:add-arrow/name-dup/pxpy
                           def-beg def-end _ _
                           use-beg use-end _ _
                           _ _ #f _))
               (hash-update! ht-defs/uses
                             (list (add1 def-beg) (add1 def-end))
                             (λ (v) (set-add v (list (add1 use-beg) (add1 use-end))))
                             (set))]
              [_ #f]))
          ;; Convert the hash table into a list, sorting the usage positions.
          (define defs/uses
            (for/list ([(def uses) (in-hash ht-defs/uses)])
              (match-define (list def-beg def-end) def)
              (list 'def/uses def-beg def-end
                    (sort (set->list uses) < #:key car))))
          ;; Append both lists, sort by positions, and print as Elisp values.
          (cons 'check-syntax-ok
                (sort (append infos defs/uses) < #:key cadr)))))))

(define (handle-fail e)
  (cons 'check-syntax-errors
        (cond [(exn:srclocs? e)
               (for/list ([sl (in-list ((exn:srclocs-accessor e) e))])
                 (match sl
                   [(srcloc path _ _ ofs span)
                    (list 'error (path->string path)
                          ofs (+ ofs span)
                          (exn-message e))]))]
              [else
               (list
                (list 'error "unknown" 1 0 (exn-message e)))])))

(module+ test
  (require rackunit
           racket/file
           racket/format
           "../error.rkt")
  (show-full-path-in-errors)

  (define this-file (path->string (syntax-source #'here)))
  (check-false (empty? (check-syntax this-file (file->string this-file))))

  (check-match
   (check-syntax this-file "#lang racket\nx")
   (list
    'check-syntax-errors
    (list
     'error this-file 14 15
     ;; Racket 6.2 thru 6.12 add "in module" to the error message
     (pregexp
      (~a this-file
          ":2:0: x: unbound identifier(?: in module)?\n  in: x"))))))

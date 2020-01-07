#lang racket/base

(require racket/list
         racket/match
         racket/path
         racket/set
         syntax/modread)

(provide check-syntax)

(define check-syntax
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
             (filter values
                     (for/list ([x (in-list xs)])
                       (match x
                         [(vector 'syncheck:add-mouse-over-status beg end str)
                          (list 'info (add1 beg) (add1 end) str)]
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

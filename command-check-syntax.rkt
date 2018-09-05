#lang racket/base

(require racket/list
         racket/match
         racket/path
         racket/set)

(provide check-syntax)

(define check-syntax
  (let ([show-content
         (with-handlers ([exn:fail? (λ _ 'not-supported)])
           (let ([f (dynamic-require 'drracket/check-syntax 'show-content)])
             ;; Ensure correct position info for Unicode like λ.
             ;; show-content probably ought to do this itself, but
             ;; work around that.
             (λ (path)
               (parameterize ([port-count-lines-enabled #t])
                 (f path)))))])
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

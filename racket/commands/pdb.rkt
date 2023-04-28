;; Copyright (c) 2013-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/set
         racket/string
         syntax/parse/define
         "../util.rkt")

(provide pdb-available?
         pdb-analyze-path
         pdb-point-info
         pdb-doc-link
         pdb-visit
         pdb-rename-sites)

;;; pdb checked syntax (if available)

(define-syntax-parser define-from-pdb
  [(_ pred:id [id:id ...])
   #`(define-values (pred id ...)
       (with-handlers ([exn:fail?
                        (λ (e)
                          (log-racket-mode-info "Using classic check-syntax:\n~a"
                                                (exn-message e))
                          (values (λ () #f)
                                  #,@(map (λ _ void) (syntax->list #'(id ...)))))])
         (values (λ () #t)
                 (dynamic-require 'pdb 'id) ...)))])

(define-from-pdb pdb-available?
  [analyze-path get-errors get-completion-candidates
                get-point-info get-doc-link get-require-path
                use->def rename-sites])

(define (pdb-analyze-path path-str code-str)
  (define path (string->path path-str))
  (define result (analyze-path path #:code code-str))
  (if (exn:break? result)
      `(break) ;abandoned due to newer request; ignore/cleanup
      (list (cons 'completions (sort (map symbol->string
                                          (set->list (get-completion-candidates path)))
                                     string<=?))
            (cons 'errors      (get-errors path)))))

(define (pdb-point-info path-str pos beg end)
  (hash-update (get-point-info (string->path path-str) pos beg end)
               'mouse-over
               (match-lambda
                 [(list beg end texts)
                  (list beg end (string-join (sort (set->list texts) string<=?)
                                             "; "))]
                 [#f #f])))

(define (pdb-doc-link path-str pos)
  (get-doc-link (string->path path-str) pos))

(define (pdb-visit path-str pos)
  (define path (string->path path-str))
  (or (use->def path pos)
      (match (get-require-path path pos)
        [(? path? req-path) (list req-path 1 2)]
        [#f #f])))

(define (pdb-rename-sites path-str pos)
  (rename-sites (string->path path-str) pos))

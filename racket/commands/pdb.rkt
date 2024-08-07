;; Copyright (c) 2013-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/set
         racket/string
         syntax/parse/define
         "../util.rkt"
         "../elisp.rkt")

(provide pdb-command)

;;; pdb checked syntax (if available)

(define-syntax-parser define-from-pdb
  [(_ pred:id [id:id ...])
   #`(define-values (pred id ...)
       (with-handlers ([exn:fail?
                        (λ (e)
                          (log-racket-mode-info "Using classic check-syntax:\n~a"
                                                (exn-message e))
                          (values (λ () #f)
                                  'id ...))])
         (values (λ () #t)
                 (dynamic-require 'pdb 'id) ...)))])

(define-from-pdb available?
  [analyze-path forget-path
   add-directory forget-directory
   get-errors get-submodule-names get-completion-candidates
   get-point-info get-doc-link get-require-path
   use->def rename-sites
   db-stats])

(define (pdb-command . args)
  (match args
    [`(available?)                      (available?)]
    [`(analyze-path ,path ,code)        (pdb-analyze-path path code)]
    [`(forget-path ,path)               (pdb-forget-path path)]
    [`(add-directory ,path ,depth ,always) (pdb-add-directory path depth always)]
    [`(forget-directory ,path)          (pdb-forget-directory path)]
    [`(submodules ,path ,pos)           (pdb-submodules path pos)]
    [`(completions ,path ,pos)          (pdb-completions path pos)]
    [`(point-info ,path ,pos ,beg ,end) (pdb-point-info path pos beg end)]
    [`(doc-link ,path ,pos)             (pdb-doc-link path pos)]
    [`(visit ,path, pos)                (pdb-visit path pos)]
    [`(rename-sites ,path ,pos)         (pdb-rename-sites path pos)]
    [`(db-stats)                        (db-stats)]))

(define (pdb-analyze-path path-str code-str)
  (define path (string->path path-str))
  (define result (analyze-path path #:code code-str))
  (if (exn:break? result)
      `(break) ;abandoned due to newer request; ignore/cleanup
      (list (cons 'errors (get-errors path)))))

(define (pdb-forget-path path-str)
  (define path (string->path path-str))
  (forget-path path))

(define (pdb-add-directory path-str depth always)
  (define path (string->path path-str))
  (define always? (as-racket-bool always))
  (add-directory path #:import-depth depth #:always always?))

(define (pdb-forget-directory path-str)
  (define path (string->path path-str))
  (forget-directory))

(define (pdb-submodules path-str pos)
  (define path (string->path path-str))
  (get-submodule-names path pos))

(define (pdb-completions path-str pos)
  (define path (string->path path-str))
  (sort (map symbol->string
             (set->list (get-completion-candidates path pos)))
        string<=?))

(define (pdb-point-info path-str pos beg end)
  (hash-update (get-point-info (string->path path-str) pos beg end)
               'point-mouse-over
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

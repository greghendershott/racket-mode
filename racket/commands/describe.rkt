;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         (only-in "../find.rkt" find-signature)
         "../identifier.rkt"
         (only-in "../scribble.rkt"
                  identifier->bluebox
                  binding->path+anchor))

(provide type
         describe)

(module+ test
  (require rackunit))

;;; type

(define/contract (type how str)
  (-> how/c string? (or/c #f string?))
  (or (and (eq? how 'namespace)
           (->identifier 'namespace str type-or-contract))
      (->identifier how str identifier->bluebox)
      (match (find-signature how str)
        [#f #f]
        [x (~a x)])))

(define (type-or-contract v) ;any/c -> (or/c #f string?)
  (or
   ;; 1. Try using Typed Racket's REPL simplified type.
   (with-handlers ([exn:fail? (λ _ #f)])
     (match (with-output-to-string
              (λ ()
                ((current-eval)
                 (cons '#%top-interaction v))))
       [(pregexp "^- : (.*) \\.\\.\\..*\n" (list _ t)) t]
       [(pregexp "^- : (.*)\n$"            (list _ t)) t]))
   ;; 2. Try to find a contract.
   (with-handlers ([exn:fail? (λ _ #f)])
     (parameterize ([error-display-handler (λ _ (void))])
       ((current-eval)
        (cons '#%top-interaction
              `(if (has-contract? ,v)
                (~a (contract-name (value-contract ,v)))
                (error ""))))))))

;;; describe

;; When `str` is an identifier for which we can find documentation,
;; return (cons path anchor).
;;
;; Otherwise, try to find a function definition signature (the
;; argument names may have explanatory value), and/or a Typed Racket
;; type or a contract, if any. If found return (list 'shr-dom dom)
;; where dom is the Emacs equivalent of an x-expression.
;;
;; Otherwise return #f.
(define/contract (describe how str)
  (-> how/c
      string?
      any) ;(or/c #f (cons/c path-string? string?) shr-dom)
  (->identifier
   how str
   (λ (stx)
     (or (binding->path+anchor stx)
         (sig-and/or-type how stx)))))

(define/contract (sig-and/or-type how stx)
  (-> how/c identifier? any) ;shr-dom
  (define dat (syntax->datum stx))
  (define sig (match (find-signature how (symbol->string dat))
                [#f #f]
                [x (~a x)]))
  (define type (and (eq? how 'namespace)
                    (type-or-contract stx)))
  (define in (if (eq? how 'namespace) "current-namespace" (~v how)))
  (and (or sig type)
       (list 'shr-dom
             `(div ()
               (h1 () (code () ,(or sig (~a dat))))
               (p () ,(if type `(code () ,type) ""))
               (p () "In " (code () ,in) ".")))))

(module+ test
  (require rackunit
           "../syntax.rkt")
  ;; Check something that is in the namespace resulting from
  ;; module->namespace on, say, this source file.
  (parameterize ([current-namespace (module->namespace (syntax-source #'this-file))])
    (check-equal?
     (describe 'namespace "describe")
     '(shr-dom
       (div
        ()
        (h1 () (code () "(describe how str)"))
        (p () (code () "(-> (or/c (quote namespace) path-string?) string? any)"))
        (p () "In " (code () "current-namespace") "."))))
    (check-false
     (describe 'namespace "something-not-defined-in-the-namespace")))

  ;; Check something that is not in the current namespace, but is an
  ;; identifier in the lexical context of an expanded module form --
  ;; including imported identifiers -- from the expanded syntax
  ;; cache.
  (define top (case (system-type) [(windows) "C:\\"] [(unix macosx) "/"]))
  (define path-str (path->string (build-path top "path" "to" "foobar.rkt")))
  (define code-str (~a '(module foobar racket/base
                         (define (fun a b c)
                          (void)))))
  ;; Get the expanded syntax in our cache
  (string->expanded-syntax path-str code-str void)
  ;; Note that this doesn't find contracts, just sigs.
  (check-equal?
   (describe path-str "fun")
   `(shr-dom
     (div ()
      (h1 () (code () "(fun a b c)"))
      (p ()  "")
      (p () "In " (code () ,(~v path-str)) "."))))
  (check-false
   (describe path-str "something-not-defined-in-the-file")))


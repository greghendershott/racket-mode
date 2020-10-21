#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         (only-in xml xexpr->string)
         (only-in "../find.rkt" find-signature)
         "../identifier.rkt"
         "../scribble.rkt")

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

;; If a symbol has installed documentation, display it.
;;
;; Otherwise, walk the source to find a function definition signature
;; (the argument names may have explanatory value). When using a
;; module->namespace, also look for Typed Racket type or a contract,
;; if any.

(define/contract (describe how str)
  (-> (or/c how/c
            (cons/c path-string? string?)
            (list/c path-string?))
      string?
      string?)
  (or (match how
        [(list (? path-string? path))
         (path+anchor->html (cons path #f))]
        [(cons (? path-string? path) (? string? anchor))
         (path+anchor->html (cons path anchor))]
        [(and (or 'namespace (? path-string?)) how)
         (->identifier how str
                       (λ (stx)
                         (or (path+anchor->html (binding->path+anchor stx))
                             (sig-and/or-type how stx))))])
      (format "Found no documentation for ~v in ~a."
              str how)))

(define/contract (sig-and/or-type how stx)
  (-> how/c identifier? string?)
  (define dat (syntax->datum stx))
  (define s (match (find-signature how (symbol->string dat))
              [#f #f]
              [x (~a x)]))
  (define t (and (eq? how 'namespace)
                 (type-or-contract stx)))
  (xexpr->string
   `(div ()
     (h1 () ,(or s (~a dat)))
     ,(cond [(not (or s t))
             `(p ()
               (em ()  ,(if (eq? how 'namespace)
                            "(Found no documentation, signature, type, or contract.)"
                            "(Found no documentation or signature.)")))]
            [t `(pre () ,t)]
            [else ""])
     (br ()))))

(module+ test
  (require rackunit
           (only-in xml string->xexpr)
           "../syntax.rkt")
  ;; Check something that is in the namespace resulting from
  ;; module->namespace on, say, this source file.
  (parameterize ([current-namespace (module->namespace (syntax-source #'this-file))])
    (check-equal?
     ;; Convert back to an xexpr because easier to grok test and also
     ;; older xexpr->string probably used <br /> instead of <br/>.
     (string->xexpr (describe 'namespace "describe"))
     `(div ()
       (h1 () "(describe how str)")
       (pre ()  "(-" ">" " (or/c (or/c (quote namespace) path-string?) (cons/c path-string? string?) (list/c path-string?)) string? string?)")
       (br ()))))

  ;; Check something that is not in the current namespace, but is an
  ;; identifier in the lexical context of an expanded module form --
  ;; including imported identifiers -- from the expanded syntax
  ;; cache.
  (define path-str "/path/to/foobar.rkt")
  (define code-str (~a '(module foobar racket/base
                         (define (fun a b c)
                          (void)))))
  ;; Get the expanded syntax in our cache
  (string->expanded-syntax path-str code-str void)
  ;; Note that this doesn't find contracts, just sigs.
  (check-equal?
   (string->xexpr (describe path-str "fun"))
   `(div ()
     (h1 () "(fun a b c)")
     (br ()))))

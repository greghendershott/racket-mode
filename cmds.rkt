#lang racket/base

(require racket/string
         racket/contract
         racket/pretty
         racket/match
         racket/format
         syntax/modresolve
         "defn.rkt"
         "logger.rkt"
         "util.rkt")

(provide make-prompt-read)

(define (make-prompt-read path put/stop rerun)
  (define-values (base name dir?) (cond [path (split-path path)]
                                        [else (values "" "" #f)]))
  (λ ()
    (flush-output (current-error-port))
    (display name) (display "> ")
    (define in ((current-get-interaction-input-port)))
    (define stx ((current-read-interaction) (object-name in) in))
    (syntax-case stx ()
      [(uq cmd)
       (eq? 'unquote (syntax-e #'uq))
       (case (syntax-e #'cmd)
         [(contract) (get-contract (read))]
         [(run) (put/stop (rerun (~a (read))))]
         [(top) (put/stop (rerun #f))]
         [(def) (def (read))]
         [(doc) (doc (read-line))]
         [(mod) (mod (read) path)]
         [(exp) (exp1 (read))]
         [(exp+) (exp+)]
         [(exp!) (exp! (read))]
         [(log) (log-display (map string->symbol (string-split (read-line))))]
         [(pwd) (display-commented (~v (current-directory)))]
         [(cd) (cd (~a (read)))]
         [else (usage)])]
      [_ stx])))

(define (usage)
  (displayln
   "Commands:
,contract <identifier>
,run </path/to/file.rkt>
,top
,def <identifier>
,mod <module-path>
,doc <string>
,exp <stx>
,exp+
,exp! <stx>
,pwd
,cd <path>
,log <opts> ...")
  (void))

(define (get-contract sym)
  (elisp-println
   (match ((current-eval) (cons '#%top-interaction
                          `(if (has-contract? ,sym)
                            (contract-name (value-contract ,sym))
                            #f)))
     [(list-rest '-> lst) 
      ;;FIXME: it seems the elisp code truncate to "> "
      lst]
     [lst lst])))

(define (def sym)
  (elisp-println (find-definition (symbol->string sym))))

(define (mod v rel)
  (define (mod* mod rel)
    (define path (with-handlers ([exn:fail? (λ _ #f)])
                   (resolve-module-path mod rel)))
    (and path
         (file-exists? path)
         (list (path->string path) 1 0)))
  (elisp-println (cond [(module-path? v) (mod* v rel)]
                       [(symbol? v)      (mod* (symbol->string v) rel)]
                       [else             #f])))

(define (elisp-println v)
  (elisp-print v)
  (newline))

(define (elisp-print v)
  (match v
    [#f (display "nil")]
    [#t (display "t")]
    [v (print v)]))

(define (doc str)
  (eval `(begin
          (require racket/help)
          (help ,(string-trim str))
          (newline))))

(define (cd s)
  (let ([old-wd (current-directory)])
    (current-directory s)
    (unless (directory-exists? (current-directory))
      (display-commented (format "~v doesn't exist." (current-directory)))
      (current-directory old-wd))
    (display-commented (format "In ~v" (current-directory)))))

;;; syntax expansion

(define last-stx #f)

(define (exp1 stx)
  (set! last-stx (expand-once stx))
  (pp-stx last-stx))

(define (exp+)
  (when last-stx
    (define this-stx (expand-once last-stx))
    (cond [(equal? (syntax->datum last-stx) (syntax->datum this-stx))
           (display-commented "Already fully expanded.")
           (set! last-stx #f)]
          [else
           (pp-stx this-stx)
           (set! last-stx this-stx)])))

(define (exp! stx)
  (set! last-stx #f)
  (pp-stx (expand stx)))

(define (pp-stx stx)
  (newline)
  (pretty-print (syntax->datum stx)
                (current-output-port)
                1))

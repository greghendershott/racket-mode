;;; Quoted list

'(a b
  (a b
   c))

'((1) 2 3
  (3)
  4 5)

;;; Quasiquoted list (align with head) and unquote or unquote-splicing
;;; (use normal indent rules for the form).

`(Part ()
  (PartNumber ()
   ,part)
  (ETag ()
   ,etag))

`((,(x)
   ,y))

`(Delete
  ,@(for/list ([p (in-list paths)])
      `(Object ()
        (Key () ,p))))

;;; Syntax

#'(for/list ([x xs])
    x)

#`(for/list ([x xs])
    x)

#'(#%app (#%app hasheq (quote a) (quote 42))
         (quote a))

(#%app (#%app hasheq (quote a) (quote 42))
       (quote a))

#'(foo (#%app hasheq (quote a) (quote 42))
       (quote a))

;;; Rackjure style dictionary

{a b
 c d}

{a b
 c d
 b '(a x
     s (x y
        x v))}

;;; Vector

#(a b
  c d)

;;; List with a keyword as first member (e.g. in many contracts)

(#:x y
 #:y x)

;;; Normal function application.

(foobar x
        y
        z)

(foobar
 x
 y
 z)

(dict-set a
          b
          c)

(dict-set
 a
 b
 c)

;;; Forms with special indentation

(let ([x 0])
  x)

(syntax-case stx ()
  [(_ x) #'#f]
  [(_ x y) #'#t])

;; begin and cond have 0 style
(begin
  0
  0)

(begin 0
       0)

(cond [1 2]
      [3 4])

(cond
  [1 2]
  [3 4])

(if a
    x
    x)

;; begin*

(begin-for-foo 0
               0)

(begin-for-foo
  0
  0)

(with-handlers ([x y])
  a b c)

;; define and other 'defun style

(define (x) x x
  x)

(struct x x
  ())

(match-define (list x y)
  (list 1 2))

;;; Bug #50

'((x
   y) A
  z
  (x
   y) A
  z)

(match args
  [(list x) (x
             y)] ...
  [(list x) (x y)] ...
  [(list x) (x y)] ...)

(define-syntax (fstruct stx)
  (syntax-parse stx
    [(_ id:id (field:id ...))
     (with-syntax ([(accessor ...)
                    (for/list ([fld (in-list (syntax->list #'(field ...)))])
                      (format-id stx "~a-~a" (syntax->datum #'id) fld))])
       #'(serializable-struct
          id (field ...) #:transparent
          #:property prop:procedure
          (lambda (self . args)
            (match args
              [(list 'field) (accessor self)] ...
              [(list (list 'field)) (accessor self)] ...
              [(list (list-rest 'field fields)) ((accessor self) fields)] ...
              [(list-rest 'field f args)
               (struct-copy id self
                            [field (apply f (accessor self) args)])] ...
              [(list-rest (list 'field) f args)  ;<-- THIS SEXPR IS INDENTED TOO FAR
               (struct-copy id self
                            [field (apply f (accessor self) args)])] ...
              [(list-rest (list-rest 'field fields) args)
               (struct-copy id self
                            [field (apply (accessor self) fields args)])] ...))))]))

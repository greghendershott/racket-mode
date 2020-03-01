#lang racket/base

(require rackunit)

;; examples for comment fontification

(check-eq? 'one ; comment
           'one)

#;(a b c ;comment
     d e f)

(check-eq? 'two #; one 'two)

(check-eq? 'three #; #; one two 'three)

(check-eq? 'three
           #;
           #;
           one
           two
           'three)

(check-eq? 'three
           #; ; comment
           #; ; comment
           one
           two
           'three)

(check-eq? 'three
           #; ; comment
           #; ; comment
           (list one)
           (list two)
           'three)

(check-eq? 'three
           #;
           ; comment
           #;
           #|
           comment
           |#
           one
           (a b c ; comment
              d e f)
           'three)

(check-eq? 'one ; #;
           'one)

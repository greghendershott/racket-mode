#lang info
(define version "0.0")
(define collection 'multi)
(define deps '(["base" #:version "6.12"]
               ["data-lib" #:version "1.1"]
               ["syntax-color-lib" #:version "1.1"]))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Use and/or implement hash-lang lexers and/or indenters.")
(define scribblings '("token-map.scrbl"))

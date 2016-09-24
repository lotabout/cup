#lang info
(define collection "cup")
(define deps '("base"
               "rackunit-lib"
               "axe"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/cup.scrbl" ())))
(define pkg-desc "Simple REST Web Framework")
(define version "0.0")
(define pkg-authors '(jinzhouz))

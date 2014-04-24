;;;; package.lisp

(in-package :cl-user)

(defpackage #:hash-set
  (:use #:cl)
  (:export #:hash-set
           #:dohashset
           #:list-to-hs
           #:hs-to-list
           #:hs-copy
           #:hs-equal
           #:hs-memberp
           #:hs-insert
           #:hs-delete
           #:hs-map
           #:hs-count
           #:hs-union
           #:hs-intersection
           #:hs-cartesian-product
           #:hs-powerset
           #:hs-difference
           #:hs-symmetric-difference
           #:hs-pretty-print))

(defpackage #:hash-set-test
  (:use #:cl
        #:fiveam
        #:hash-set))

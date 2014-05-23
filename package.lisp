(in-package :cl-user)

(defpackage #:hash-set
  (:use #:cl)
  (:export #:hash-set
           #:make-hash-set
           #:list-to-hs
           #:hs-to-list

           #:hs-copy
           #:hs-equal
           #:hs-memberp
           #:hs-emptyp

           #:hs-insert
           #:hs-remove
           #:hs-remove-if
           #:hs-remove-if-not

           #:hs-any
           #:hs-all
           #:hs-map
           #:hs-filter
           #:dohashset

           #:hs-count
           #:hs-union
           #:hs-subsetp
           #:hs-supersetp
           #:hs-powerset
           #:hs-difference
           #:hs-intersection
           #:hs-cartesian-product
           #:hs-symmetric-difference))

(defpackage #:hash-set-test
  (:use #:cl
        #:fiveam
        #:hash-set))

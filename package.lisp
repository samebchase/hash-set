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
           #:hs-ninsert

           #:hs-remove
           #:hs-nremove

           #:hs-remove-if
           #:hs-nremove-if

           #:hs-remove-if-not
           #:hs-nremove-if-not

           #:hs-any
           #:hs-all
           #:hs-map
           #:hs-filter
           #:dohashset

           #:hs-count

           #:hs-union
           #:hs-nunion

           #:hs-intersection
           #:hs-nintersection

           #:hs-difference
           #:hs-ndifference

           #:hs-symmetric-difference

           #:hs-cartesian-product

           #:hs-subsetp
           #:hs-proper-subsetp
           #:hs-supersetp
           #:hs-proper-supersetp
           #:hs-powerset))

(defpackage #:hash-set-test
  (:use #:cl
        #:fiveam
        #:hash-set))

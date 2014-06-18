(defpackage :hash-set-system
  (:use :cl :asdf))

(in-package :hash-set-system)

(defsystem #:hash-set
  :serial t
  :description "An implementation of the hash-set data structure."
  :author "Samuel Chase <samebchase@gmail.com>"
  :license "Unlicense"
  :depends-on (#:alexandria
               #:optima
               #:fiveam)
  :components ((:file "package")
               (:file "hash-set")
               (:file "test")))


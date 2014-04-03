;;;; graphs.asd

(defpackage :hash-set-system
  (:use :cl :asdf))

(in-package :hash-set-system)

(defsystem #:hash-set
  :serial t
  :description "Explorations in set theory."
  :author "Samuel Chase <samebchase@gmail.com>"
  :license "Unknown"
  :depends-on (#:alexandria
               #:optima
               #:fiveam)
  :components ((:file "package")
               (:file "hash-set")
               (:file "test")))


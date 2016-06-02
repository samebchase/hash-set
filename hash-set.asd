(in-package #:asdf-user)

(defsystem #:hash-set
  :serial t
  :description "An implementation of the hash-set data structure."
  :author "Samuel Chase <samebchase@gmail.com>"
  :license "Unlicense"
  :depends-on (#:alexandria
               #:optima)
  :components ((:file "package")
               (:file "hash-set")))


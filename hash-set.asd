(in-package #:asdf-user)

(defsystem #:hash-set
  :serial t
  :description "An implementation of the hash-set data structure."
  :author '("Samuel Chase <samebchase@gmail.com>"
            "Jeremiah LaRocco <jeremiah_larocco@fastmail.com")
  :license "Unlicense"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "hash-set")))


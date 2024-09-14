(in-package #:asdf-user)

(defsystem #:hash-set-tests
  :serial t
  :description "An implementation of the hash-set data structure."
  :author '("Samuel Chase <samebchase@gmail.com>"
            "Jeremiah LaRocco <jeremiah_larocco@fastmail.com"
            )
  :license "Unlicense"
  :depends-on (#:hash-set
               #:fiveam)
  :components ((:file "test")
               (:file "benchmark")))

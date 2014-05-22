(in-package :hash-set-test)

(def-suite all-tests)

(in-suite all-tests)

(test sanity
  (is (= 2 2)))

(test hs-memberp
  (let ((hash-set (make-instance 'hash-set)))
    (hs-insert hash-set 42)
    (is (hs-memberp hash-set 42))))

(test hs-insert-count
  (let ((hash-set (make-instance 'hash-set)))
    (loop repeat 3 do
         (dotimes (i 10)
           (hs-insert hash-set i)))
    (is (= 10 (hs-count hash-set)))))

(test hs-remove-count
  (let ((hash-set (make-instance 'hash-set)))
    (dotimes (i 100)
      (hs-insert hash-set i))
    (loop for i from 10 below 20 do
         (hs-remove hash-set i))
    (hs-insert hash-set 15)
    (hs-remove hash-set 15)
    (is (= 90 (hs-count hash-set)))))

(test hs-equality
  (let* ((hs-a (list-to-hs (alexandria:iota 10)))
         (hs-b (list-to-hs (alexandria:iota 10)))
         (hs-c (list-to-hs (alexandria:iota 20)))
         (hs-d (list-to-hs (alexandria:iota 10 :start 10)))
         (hs-e (list-to-hs '(a b c d e)))
         (hs-f (list-to-hs '(e d c b a)))
         (hs-a-copy (hs-copy hs-a)))
    (is (hs-equal hs-a hs-b))
    (is (hs-equal hs-e hs-f))
    (is-false (hs-equal hs-b hs-c))
    (is-false (hs-equal hs-c hs-d))
    (is (hs-equal hs-a hs-a-copy))))

(test hs-difference
  (let ((hs-a (list-to-hs (alexandria:iota 15))))
    (is (hs-equal
         (hs-difference hs-a (list-to-hs '(10 11 12 13 14)))
         (list-to-hs (alexandria:iota 10))))))

(test hs-symmetric-difference
  (is (hs-equal (list-to-hs (alexandria:iota 6 :start 1))
                (hs-symmetric-difference (list-to-hs '(1 2 3))
                                         (list-to-hs '(4 5 6)))))) 

    
    
    
    
    




      


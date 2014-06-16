(in-package :hash-set-test)

(def-suite all-tests)

(in-suite all-tests)

(test sanity
  (is (= 2 2)))

(test hs-memberp
  (let ((hash-set (make-instance 'hash-set)))
    (hs-ninsert hash-set 42)
    (is (hs-memberp hash-set 42))))

(test hs-ninsert-count
  (let ((hash-set (make-instance 'hash-set)))
    (loop repeat 3 do
         (dotimes (i 10)
           (hs-ninsert hash-set i)))
    (is (= 10 (hs-count hash-set)))))

(test hs-ninsert
  (let ((hash-set (list-to-hs '(1 2 3 4))))
    (hs-ninsert hash-set 5)
    (is (hs-equal hash-set
                  (list-to-hs '(1 2 3 4 5))))))

(test hs-nremove-count
  (let ((hash-set (make-instance 'hash-set)))
    (dotimes (i 100)
      (hs-ninsert hash-set i))
    (loop for i from 10 below 20 do
         (hs-nremove hash-set i))
    (hs-ninsert hash-set 15)
    (hs-nremove hash-set 15)
    (is (= 90 (hs-count hash-set)))))

(test hs-nremove
  (let ((hash-set (list-to-hs '(1 2 3 4))))
    (hs-nremove hash-set 3)
    (is (hs-equal hash-set
                  (list-to-hs '(1 2 4))))))

(test hs-nremove-if
  (let ((hash-set (list-to-hs (alexandria:iota 10))))
    (hs-nremove-if hash-set #'evenp)
    (is (hs-equal hash-set
                  (list-to-hs (alexandria:iota 5 :start 1 :step 2))))))

(test hs-nremove-if-not
  (let ((hash-set (list-to-hs (alexandria:iota 10))))
    (hs-nremove-if hash-set #'oddp)
    (is (hs-equal hash-set
                  (list-to-hs (alexandria:iota 5 :start 1 :step 2))))))

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

(test hs-any
  (is (hs-any (list-to-hs '(1 3 4 5)) #'evenp))
  (is-false (hs-any (list-to-hs '(1 3 4 5)) #'zerop)))

(test hs-all
  (is (hs-all (list-to-hs (alexandria:iota 5 :step 2)) #'evenp))
  (is-false (hs-all (list-to-hs '(2 4 6 8 10 12 13)) #'evenp)))

(test hs-map
  (is (hs-equal (hs-map (lambda (x) (* x x)) (list-to-hs '(1 2 3 4)))
                (list-to-hs (mapcar (lambda (x) (* x x)) '(1 2 3 4))))))

(test hs-filter
  (is (hs-equal (hs-filter #'evenp (list-to-hs (alexandria:iota 10)))
                (list-to-hs (remove-if #'evenp (alexandria:iota 10))))))

(test hs-union
  (is (hs-equal (hs-union (list-to-hs ())
                          (list-to-hs ()))
                (list-to-hs ())))                
  (is (hs-equal (hs-union (list-to-hs '(0 1 2 3))
                          (list-to-hs '(4 5 6 7)))
                (list-to-hs (alexandria:iota 8)))))

(test hs-intersection
  (is (hs-equal (hs-intersection (list-to-hs '(1 2 3 4))
                                 (list-to-hs '(3 4 5 6)))
                (list-to-hs '(3 4))))
  (is (hs-equal (hs-intersection (list-to-hs '(1 2 3))
                                 (list-to-hs '(4 5 6)))
                (list-to-hs ()))))

(test hs-difference
  (let ((hs-a (list-to-hs (alexandria:iota 15))))
    (is (hs-equal
         (hs-difference hs-a (list-to-hs '(10 11 12 13 14)))
         (list-to-hs (alexandria:iota 10))))))

(test hs-symmetric-difference
  (is (hs-equal (list-to-hs (alexandria:iota 6 :start 1))
                (hs-symmetric-difference (list-to-hs '(1 2 3))
                                         (list-to-hs '(4 5 6))))))

(test hs-cartesian-product
  (is (hs-equal
       (hs-cartesian-product
        (list-to-hs (alexandria:iota 10))
        (list-to-hs (alexandria:iota 10)))
       (list-to-hs (loop for i below 10
                      appending
                        (loop for j below 10
                           collect (list i j)))))))

(test hs-subsetp
  (is-false (hs-subsetp (list-to-hs '(6 7 8 9 10))
                        (list-to-hs '(6 7 8 9))))
  (is (hs-subsetp (list-to-hs '(1 2 3 4))
                  (list-to-hs '(1 2 3 4 5))))
  (is (hs-subsetp (list-to-hs '(4 5 6 7))
                  (list-to-hs '(4 5 6 7)))))

(test hs-supersetp
  (is-false (hs-supersetp (list-to-hs '(6 7 8 9))
                          (list-to-hs '(6 7 8 9 10))))
  (is (hs-supersetp (list-to-hs '(1 2 3 4 5))
                    (list-to-hs '(1 2 3 4))))
  (is (hs-supersetp (list-to-hs '(4 5 6 7))
                    (list-to-hs '(4 5 6 7)))))

(test hs-powerset
  (is (hs-equal (hs-powerset (list-to-hs '(1 2 3)))
                (list-to-hs '(NIL (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)))))
  (is (hs-equal (hs-powerset (list-to-hs '()))
                (list-to-hs '(())))))

(in-package :hash-set-test)

(def-suite benchmarking)

(in-suite benchmarking)

(use-package :hash-set)
(defparameter *benchmark-enabled* t)

(test intersection-speed
  (when *benchmark-enabled*

    (let ((a-size 0)
          (b-size 5000000)
          (hsa (make-hash-set))
          (hsb (make-hash-set)))
      (dotimes (num (max  a-size b-size))
        (when (< num a-size)
          (hs-ninsert hsa (random a-size)))
        (when (< num b-size)
          (hs-ninsert hsb (random b-size))))

      (format t "====================================================================================================~%")
      (format t "    Time should be roughly equal.~%")
      (time (hs-intersection hsa hsb))
      (time (hs-intersection hsb hsa)))))

(test insert-speed
  (when *benchmark-enabled*

    (let* ((max-int 5000000)
           (inplace-size 500000)
           (hsa (make-hash-set))
           (hsb (make-hash-set inplace-size)))
      (format t "====================================================================================================~%")
      (format t "    Second time should be a little faster with less consing, third time slowest with more consing.~%")
      (time
       (dotimes (num inplace-size)
         (hs-ninsert hsa (random max-int))))
      (time
       (dotimes (num inplace-size)
         (hs-ninsert hsb (random max-int))))

      ;; Fewer inserts because set copy is slow
      (time
       (loop :with small-size = 5000
             :for i :below small-size
             :for set = (make-hash-set)
               :then (hs-insert set (random max-int)))))))



(test difference-speed
  (when *benchmark-enabled*

    (let* ((max-int 50000000)
           (large-size 50000)
           (small-size 50)
           (big-set (make-hash-set large-size))
           (small-set (make-hash-set small-size)))
      (dotimes (i large-size)
        (when (< i small-size)
          (hs-ninsert small-set (random max-int)))
        (hs-insert big-set (random max-int)))
      (format t "====================================================================================================~%")
      (format t "    Time should be roughly equal .~%")
      (time (hs-difference big-set small-set))
      (time (hs-difference small-set big-set)))))


(test dohashset-speed
  (when *benchmark-enabled*

    (let* ((max-int 50000000)
           (sizes '(1000
                    10000
                    100000
                    1000000))
           (tables (mapcar #'make-hash-set sizes)))
      (loop :for i :below (car (last sizes))
            :do
               (loop :for tab :in tables
                     :for siz :in sizes
                     :when (< i siz)
                       :do
                          (hs-ninsert tab (random max-int))))
      (format t "====================================================================================================~%")
      (format t "    Time should increase linear (by roughly 10x).~%")
      (dolist (tab tables)
        (let ((sum 0))
          (time (dohashset (var tab)
                  (incf sum var))))))))

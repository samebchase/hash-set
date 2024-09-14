(in-package :hash-set)

(declaim (inline hs-map
                 hs-copy
                 hs-memberp
                 hs-count
                 hs-ninsert
                 hs-insert))
(defun make-hs-hash-table (size-hint)
  #+sbcl(make-hash-table :test #'equal :synchronized t :size size-hint)
  #+clozure(make-hash-table :test #'equal :shared t :size size-hint)
  #-(or sbcl clozure)(make-hash-table :test #'equal :size size-hint)
  )



(defclass hash-set ()
  ((table :accessor table
          :initform (make-hs-hash-table 10)
          :initarg :table))
  (:documentation "A hashset."))


(defun make-hash-set (&optional (size-hint 10))
  (declare (type integer size-hint))
  (make-instance 'hash-set :table (make-hs-hash-table size-hint)))

(defun hs-map (fn hash-set)
  (let ((result (make-hash-set (hs-count hash-set))))
    (loop
      :for key :being :the :hash-keys :of (table hash-set)
      :do (hs-ninsert result (funcall fn key)))
    result))

(defmacro dohashset ((var hash-set &optional result) &body body)
  `(progn
     (loop
       :for ,var :being :the :hash-keys :of (table ,hash-set)
       :do
          (tagbody
             ,@body))
     ,result))

(defun hs (&rest values)
  (list-to-hs values))

(defun list-to-hs (list)
  (let ((hash-set (make-hash-set (length list))))
    (loop
      :for elt :in list
      :do
         (if (consp elt)
             (hs-ninsert hash-set (list-to-hs elt))
             (hs-ninsert hash-set elt)))
    hash-set))

(defun hs-to-list (hash-set)
  (let ((result ()))
    (dohashset (elt hash-set (nreverse result))
      (if (eq (type-of elt) 'hash-set)
          (push (hs-to-list elt) result)
          (push elt result)))))

(defun hash-keys-to-set (hash-table)
  (let ((result (make-hash-set (hash-table-count hash-table))))
    (loop :for key :being :the :hash-keys :of hash-table
       :do (hs-ninsert result key))
    result))

(defun hash-values-to-set (hash-table)
  (let ((result (make-hash-set (hash-table-count hash-table))))
    (loop :for value :being :the :hash-values :of hash-table
       :do (hs-ninsert result value))
    result))

(defun hash-table-to-set (hash-table)
  (let ((result (make-hash-set (hash-table-count hash-table))))
    (loop :for key :being :the :hash-keys :of hash-table
       :using (hash-value value)
       :do (hs-ninsert result (cons key value)))
    result))

(defun hs-count (hash-set)
  (declare (type hash-set hash-set))
  (hash-table-count (table hash-set)))

(defun hs-emptyp (hash-set)
  (declare (type hash-set hash-set))
  (= 0 (hs-count hash-set)))

(defun hs-memberp (hash-set item)
  (declare (type hash-set hash-set))
  (nth-value 1 (gethash item (table hash-set))))

(defun hs-equal (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (when (/= (hs-count hs-a) (hs-count hs-b))
    (return-from hs-equal nil))
  (dohashset (elt hs-a t)
    (when (not (hs-memberp hs-b elt))
      (return nil))))

(defun hs-copy (hash-set &optional (extra-capacity 0))
  (declare (type hash-set hash-set)
           (type fixnum extra-capacity))
  (let ((hs-copy (make-hash-set (+ extra-capacity (hs-count hash-set)))))
    (dohashset (elt hash-set hs-copy)
      (hs-ninsert hs-copy elt))))

(defun hs-filter (fn hash-set)
  (declare (type hash-set hash-set))
  (let ((result (make-hash-set (floor (* 0.5 (hs-count hash-set))))))
    (dohashset (elt hash-set result)
      (when (funcall fn elt)
        (hs-ninsert result elt)))))



(defun hs-insert (hash-set item)
  (declare (type hash-set hash-set))
  (let ((result (hs-copy hash-set 1)))
    (setf (gethash item (table result)) t)
    result))

(defun hs-ninsert (hash-set item)
  (declare (type hash-set hash-set))
  (setf (gethash item (table hash-set)) t)
  hash-set)

(defun hs-remove (hash-set item)
  (declare (type hash-set hash-set))
  (let ((result (hs-copy hash-set)))
    (when (hs-memberp result item)
      (remhash item (table result)))
    result))

(defun hs-nremove (hash-set item)
  (declare (type hash-set hash-set))
  (remhash item (table hash-set))
  hash-set)

(defun hs-remove-if (predicate hash-set)
  (declare (type hash-set hash-set)
           (type function predicate))
  (let ((result (hs-copy hash-set)))
    (dohashset (elt result result)
      (when (funcall predicate elt)
        (hs-nremove result elt)))))

(defun hs-nremove-if (predicate hash-set)
  (declare (type hash-set hash-set)
           (type function predicate))
  (dohashset (elt hash-set hash-set)
    (when (funcall predicate elt)
      (hs-nremove hash-set elt))))

(defun hs-remove-if-not (predicate hash-set)
  (declare (type hash-set hash-set)
           (type function predicate))
  (let ((result (hs-copy hash-set)))
    (dohashset (elt result result)
      (unless (funcall predicate elt)
        (hs-nremove result elt)))))

(defun hs-nremove-if-not (predicate hash-set)
  (declare (type hash-set hash-set)
           (type function predicate))
  (dohashset (elt hash-set hash-set)
    (unless (funcall predicate elt)
      (hs-nremove hash-set elt))))

(defun hs-union (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (let ((result (hs-copy hs-a (hs-count hs-b))))
    (dohashset (elt hs-b result)
      (hs-ninsert result elt))))

(defun hs-nunion (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (dohashset (elt hs-b hs-a)
    (hs-ninsert hs-a elt)))

(defun hs-intersection (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (let* (
         ;; Loop over the smaller of the sets
         ;; and check if the entries exists in the larger
         (smaller (if (< (hs-count hs-a) (hs-count hs-b))
                      hs-a
                      hs-b))
         (larger (if (< (hs-count hs-a) (hs-count hs-b))
                     hs-b
                     hs-a))
         (result (make-hash-set (hs-count smaller))))
    (dohashset (elt smaller result)
      (when (hs-memberp larger elt)
        (hs-ninsert result elt)))))

(defun hs-nintersection (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (dohashset (elt hs-a hs-a)
    (unless (hs-memberp hs-b elt)
      (hs-nremove hs-a elt))))

(defun hs-difference (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (let ((result (hs-copy hs-a)))
    (dohashset (elt hs-b result)
      (hs-nremove result elt))))

(defun hs-ndifference (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (dohashset (elt hs-b hs-a)
    (hs-nremove hs-a elt)))

(defun hs-symmetric-difference (hs-a hs-b)
  (declare (type hash-set hs-a hs-b))
  (hs-union (hs-difference hs-a hs-b)
            (hs-difference hs-b hs-a)))

(defun hs-subsetp (hs-subset hs-superset)
  (declare (type hash-set hs-subset hs-superset))
  (dohashset (subset-elt hs-subset t)
    (unless (hs-memberp hs-superset subset-elt)
      (return-from hs-subsetp nil))))

(defun hs-proper-subsetp (hs-subset hs-superset)
  (declare (type hash-set hs-subset hs-superset))
  (and (hs-subsetp hs-subset hs-superset)
       (> (hs-count hs-superset) (hs-count hs-subset))))

(defun hs-supersetp (hs-superset hs-subset)
  (declare (type hash-set hs-subset hs-superset))
  (hs-subsetp hs-subset hs-superset))

(defun hs-proper-supersetp (hs-superset hs-subset)
  (declare (type hash-set hs-subset hs-superset))
  (hs-proper-subsetp hs-subset hs-superset))

(defun hs-any (predicate hash-set)
  (declare (type hash-set hash-set)
           (type function predicate))
  (dohashset (elt hash-set nil)
    (when (funcall predicate elt)
      (return-from hs-any t))))

(defun hs-all (predicate hash-set)
  (declare (type hash-set hash-set)
           (type function predicate))
  (dohashset (elt hash-set t)
    (unless (funcall predicate elt)
      (return-from hs-all nil))))

(declaim (inline %one-bit-positions))
(defun %one-bit-positions (n)
  (loop
    :with result = (make-hash-set 64)
    :for i :from 0 :below (integer-length n)
    :for one-bitp = (logbitp i n)
    :when one-bitp
      :do (hs-ninsert result i)
    :finally (return result)))

(defun hs-powerset (hash-set)
  (declare (type hash-set hash-set)
           (optimize (speed 3) (space 3)))
  (let* ((result-length (expt 2 (hs-count hash-set)))
         (result (make-hash-set result-length))
         (indexed-set-table (make-hash-table :test 'equal))
         (idx 0))
    (declare (type fixnum idx result-length)
             (type hash-set result))
    (flet ((subset-from-bit-repr-int (bit-repr-int)
             (let ((result (make-hash-set 64)))
               (dohashset (var (%one-bit-positions bit-repr-int) result)
                 (hs-ninsert result (gethash var indexed-set-table))))))
      (dohashset (var hash-set)
        (setf (gethash idx indexed-set-table) var)
        (incf idx))
      (loop
        :for bit-repr fixnum :from 0 :below result-length
        :do (hs-ninsert result (subset-from-bit-repr-int bit-repr))))
    result))

(defun hs-cartesian-product (hs-a hs-b)
  (declare (type hash-set hs-a hs-b)
           (optimize (speed 3) (space 3)))
  (let* ((a-cnt (the fixnum (hs-count hs-a)))
         (b-cnt (the fixnum (hs-count hs-b)))
         (mul (the fixnum (* a-cnt b-cnt)))
         (result (make-hash-set (the fixnum mul))))
    (declare (type fixnum a-cnt b-cnt mul)
             (type hash-set result))
    (dohashset (elt-a hs-a result)
      (dohashset (elt-b hs-b)
        (hs-ninsert result (list elt-a elt-b))))))

(defmethod print-object ((hash-set hash-set) stream)
  (print-unreadable-object (hash-set stream :identity t :type t)
    (format stream "of count: ~a" (hs-count hash-set))))

(declaim (inline hs-first))
(defun hs-first (hash-set)
(declare (type hash-set hash-set)
           (optimize (speed 3) (space 3)))
  (loop :for i :below 1
        :for key :being :the :hash-keys :of (table hash-set)
        :finally (return key)))

(defun hs-pop (hash-set)
  (declare (type hash-set hash-set)
           (optimize (speed 3) (space 3)))
  (let* ((element (hs-first hash-set))
         (result (hs-remove hash-set element)))
    (values element result)))

(defun hs-npop (hash-set)
  (declare (type hash-set hash-set)
           (optimize (speed 3) (space 3)))
  (let ((element (hs-first hash-set)))
    (hs-nremove hash-set element)
    (values element hash-set)))



(in-package :hash-set)

#|

Engineering guidance taken from Robert Smith's map-set library that
can be installed using Quicklisp.

|#

(defclass hash-set ()
  ((table :accessor table :initform (make-hash-table :test #'equalp))
   (size  :accessor size :initform 0))
  (:documentation "A hashset."))

(defun make-hash-set ()
  (make-instance 'hash-set))

(defun hs-map (fn hash-set)
  (let ((result (make-hash-set)))
    (loop for key being the hash-keys of (table hash-set)
       do (hs-ninsert result (funcall fn key)))
    result))

(defmacro dohashset ((var hash-set &optional result) &body body)
  ;; magic due to pjb from #lisp
  `(block nil (hs-map (lambda (,var)
                        (tagbody ,@body))
                      ,hash-set)
          ,result))

(defun list-to-hs (list)
  (let ((hash-set (make-hash-set)))
    (loop for elt in list do
         (if (consp elt)
             (hs-ninsert hash-set (list-to-hs elt))
             (hs-ninsert hash-set elt)))
    hash-set))

(defun hs-to-list (hash-set)
  (let ((result ()))
    (dohashset (elt hash-set)
      (if (eq (type-of elt) 'hash-set)
          (push (hs-to-list elt) result)
          (push elt result)))
    (nreverse result)))

(defun hs-count (hash-set)
  (size hash-set))

(defun hs-emptyp (hash-set)
  (= 0 (hs-count hash-set)))

(defun hs-equal (hs-a hs-b)
  (if (/= (hs-count hs-a) (hs-count hs-b))
      nil
      (progn
        (dohashset (elt hs-a)
          (unless (hs-memberp hs-b elt)
            (return nil)))
        t)))

(defun hs-copy (hash-set)
  (let ((hs-copy (make-hash-set)))
    (dohashset (elt hash-set)
      (hs-ninsert hs-copy elt))
    hs-copy))

(defun hs-filter (fn hash-set)
  (let ((result (make-hash-set)))
    (dohashset (elt hash-set)
      (when (funcall fn elt)
        (hs-ninsert result elt)))
    result))

(defun hs-memberp (hash-set item)
  (gethash item (table hash-set)))

(defun hs-insert (hash-set item)
  (let ((result (hs-copy hash-set)))
    (unless (hs-memberp result item)
      (push t (gethash item (table result)))
      (incf (size result)))
    result))

(defun hs-ninsert (hash-set item)
  (unless (hs-memberp hash-set item)
    (push t (gethash item (table hash-set)))
    (incf (size hash-set)))
  hash-set)

(defun hs-remove (hash-set item)
  (let ((result (hs-copy hash-set)))
    (when (hs-memberp result item)
      (remhash item (table result))
      (decf (size result)))
    result))

(defun hs-nremove (hash-set item)
  (when (hs-memberp hash-set item)
    (remhash item (table hash-set))
    (decf (size hash-set)))
  hash-set)

(defun hs-remove-if (predicate hash-set)
  (let ((result (hs-copy hash-set)))
    (dohashset (elt result)
      (when (funcall predicate elt)
        (hs-nremove result elt)))
    result))

(defun hs-nremove-if (predicate hash-set)
  (dohashset (elt hash-set)
    (when (funcall predicate elt)
      (hs-nremove hash-set elt)))
  hash-set)

(defun hs-remove-if-not (predicate hash-set)
  (let ((result (hs-copy hash-set)))
    (dohashset (elt result)
      (unless (funcall predicate elt)
        (hs-nremove result elt)))
    result))

(defun hs-nremove-if-not (predicate hash-set)
  (dohashset (elt hash-set)
    (unless (funcall predicate elt)
      (hs-nremove hash-set elt)))
  hash-set)

(defun hs-union (hs-a hs-b)
  (let ((result (hs-copy hs-a)))
    (dohashset (elt hs-b)
      (hs-ninsert result elt))
    result))

(defun hs-nunion (hs-a hs-b)
  (dohashset (elt hs-b)
    (unless (hs-memberp hs-a elt)
      (hs-ninsert hs-a elt)))
  hs-a)

(defun hs-intersection (hs-a hs-b)
  (let ((result (make-hash-set)))
    (dohashset (elt hs-a)
      (when (hs-memberp hs-b elt)
        (hs-ninsert result elt)))
    result))

(defun hs-nintersection (hs-a hs-b)
  (dohashset (elt hs-a)
    (unless (hs-memberp hs-b elt)
      (hs-nremove hs-a elt)))
  hs-a)

(defun hs-difference (hs-a hs-b)
  (let ((result (hs-copy hs-a)))
    (dohashset (elt hs-b)
      (hs-nremove result elt))
    result))

(defun hs-ndifference (hs-a hs-b)
  (dohashset (elt hs-b)
    (hs-nremove hs-a elt))
  hs-a)

(defun hs-symmetric-difference (hs-a hs-b)
  (hs-union (hs-difference hs-a hs-b)
            (hs-difference hs-b hs-a)))

(defun hs-subsetp (hs-subset hs-superset)
  "Returns T when hs-subset is a subset of hs-superset."
  (let ((return-value t))
    (dohashset (subset-elt hs-subset)
      (unless (hs-memberp hs-superset subset-elt)
        (setf return-value nil)
        (return)))
    return-value))

(defun hs-proper-subsetp (hs-subset hs-superset)
  (and (hs-subsetp hs-subset hs-superset)
       (> (hs-count hs-superset) (hs-count hs-subset))))

(defun hs-supersetp (hs-superset hs-subset)
  (hs-subsetp hs-subset hs-superset))

(defun hs-proper-supersetp (hs-superset hs-subset)
  (hs-proper-subsetp hs-subset hs-superset))

(defun hs-any (predicate hash-set)
  (let ((return-value nil))
    (dohashset (elt hash-set)
      (when (funcall predicate elt)
        (setf return-value t)
        (return)))
    return-value))

(defun hs-all (predicate hash-set)
  (let ((return-value t))
    (dohashset (elt hash-set)
      (unless (funcall predicate elt)
        (setf return-value nil)
        (return)))
    return-value))

(defun %one-bit-positions (n)
  (let ((result (make-hash-set)))
    (loop for i from 0 below (integer-length n)
       for one-bitp = (logbitp i n)
       when one-bitp
       do (hs-ninsert result i))
    result))

(defun hs-powerset (hash-set)
  "Generates the powerset of hash-set."
  (let ((result (make-hash-set))
        (result-length (expt 2 (hs-count hash-set)))
        (indexed-set-table (make-hash-table :test 'equal))
        (idx 0))
    (flet ((subset-from-bit-repr-int (bit-repr-int)
             (let ((result (make-hash-set)))
               (dohashset (var (%one-bit-positions bit-repr-int))
                 (hs-ninsert result (gethash var indexed-set-table)))
               result)))
      (dohashset (var hash-set)
        (setf (gethash idx indexed-set-table) var)
        (incf idx))
      (loop for bit-repr from 0 below result-length
         do (hs-ninsert result (subset-from-bit-repr-int bit-repr))))
    result))

(defun hs-cartesian-product (hs-a hs-b)
  (let ((result (make-hash-set)))
    (dohashset (elt-a hs-a)
      (dohashset (elt-b hs-b)
        (hs-ninsert result (list elt-a elt-b))))
    result))

(defmethod print-object ((hash-set hash-set) stream)
  (print-unreadable-object (hash-set stream :identity t :type t)
    (format stream "of count: ~a" (hs-count hash-set))))

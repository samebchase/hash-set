(in-package :hash-set)

#|

Engineering guidance taken from Robert Smith's map-set library that
can be installed using Quicklisp.

|#

(defclass hash-set ()
  ((table :accessor table :initform (make-hash-table :test #'equalp))
   (size  :accessor size :initform 0))
  (:documentation "A hashset."))

(defun list-to-hs (list)
  (let ((hash-set (make-instance 'hash-set)))
    (loop for elt in list do
         (if (consp elt)
             (hs-insert hash-set (list-to-hs elt))
             (hs-insert hash-set elt)))
    hash-set))

(defun hs-to-list (hash-set)
  (let ((result ()))
    (with-hash-table-iterator (iterator (table hash-set))
      (loop for i from 1 to (hs-count hash-set) do
           (let ((value (nth-value 1 (iterator))))
             (if (eq (type-of value) 'hash-set)
                 (push (hs-to-list value) result)
                 (push value result)))))
    (nreverse result)))

(defun hs-memberp (hash-set item)
  (gethash item (table hash-set)))

(defun hs-insert (hash-set item)
  (unless (hs-memberp hash-set item)
    (push item (gethash item (table hash-set)))
    (incf (size hash-set))))

(defun hs-delete (hash-set item)
  (when (hs-memberp hash-set item)
    (remhash item (table hash-set))
    (decf (size hash-set))))

(defun hs-count (hash-set)
  (size hash-set))  

(defun hs-map (hash-set fn)
  (let ((result (make-instance 'hash-set)))
    (with-hash-table-iterator (iterator (table hash-set))
      (loop for i from 1 to (hs-count hash-set) do
           (let ((value (nth-value 1 (iterator))))
             (hs-insert result (funcall fn value)))))
    result))

;; (defmacro dohashset (hashset)

(defun hs-union (hs-a hs-b)
  (let ((count-a (hs-count hs-a))
        (count-b (hs-count hs-b))
        (result (make-instance 'hash-set)))
    (with-hash-table-iterator (iterator (table hs-a))
      (loop for i from 1 to count-a do
           (hs-insert result (nth-value 1 (iterator)))))
    (with-hash-table-iterator (iterator (table hs-b))
      (loop for i from 1 to count-b do
           (hs-insert result (nth-value 1 (iterator)))))
    result))
    
(defun hs-intersection (hs-a hs-b)
  (let ((result (make-instance 'hash-set)))
    (with-hash-table-iterator (iterator (table hs-a))
      (loop for i from 1 to (hs-count hs-a) do
           (let ((value (nth-value 1 (iterator))))
             (when (hs-memberp hs-b value)
               (hs-insert result value)))))
    result))

(defun hs-equal (hs-a hs-b)
  (if (/= (hs-count hs-a) (hs-count hs-b))
      nil
      (with-hash-table-iterator (iterator (table hs-a))
        (loop for i from 1 to (hs-count hs-a) do
             (let ((value (nth-value 1 (iterator))))
               (unless (hs-memberp hs-b value)
                 (return nil))))
        t)))

(defun hs-copy (hash-set)
  (let ((copy (make-instance 'hash-set)))
    (with-hash-table-iterator (iterator (table hash-set))
      (loop for i from 1 to (hs-count hash-set) do
           (hs-insert copy (nth-value 1 (iterator)))))
    copy))

(defun hs-difference (hs-a hs-b)
  (let ((result (hs-copy hs-a)))
    (with-hash-table-iterator (iterator (table hs-b))
      (loop for i from 1 to (hs-count hs-b) do
           (let ((value (nth-value 1 (iterator))))
             (hs-delete result value))))
    result))

(defun hs-symmetric-difference (hs-a hs-b)
  (hs-union (hs-difference hs-a hs-b)
            (hs-difference hs-b hs-a)))

(defun hs-cartesian-product (hs-a hs-b) 1
nil)

(defun hs-powerset (hash-set)
  nil)

(defmethod print-object ((hash-set hash-set) stream)
  (print-unreadable-object (hash-set stream :identity t :type t)
    (format stream "of count: ~a" (hs-count hash-set))))

(defun hs-pretty-print (hash-set)
  (let ((count (hs-count hash-set)))
    (if (= count 0)
        (format t "{}")
        (with-hash-table-iterator (iterator (table hash-set))
          (format t "{")
          (loop for i from 1 to (1- count) do
               (let ((value (nth-value 1 (iterator))))
                 (if (eq (type-of value) 'hash-set)
                     (hs-pretty-print value)
                     (format t "~a" value))
                 (format t ", ")))
          (let ((value (nth-value 1 (iterator))))
            (if (eq (type-of value) 'hash-set)
                (hs-pretty-print value)
                (format t "~a" value))
            (format t "}"))))))

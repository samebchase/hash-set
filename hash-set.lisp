(in-package :hash-set)

#|

Engineering guidance taken from Robert Smith's map-set library that
can be installed using Quicklisp.

|#

(defclass hash-set ()
  ((table :accessor table :initform (make-hash-table :test #'equalp))
   (size  :accessor size :initform 0))
  (:documentation "A hashset."))

(defun hs-map (fn hash-set)
  (let ((result (make-instance 'hash-set)))
    (with-hash-table-iterator (iterator (table hash-set))
      (loop for i from 1 to (hs-count hash-set) do
           (let ((value (nth-value 1 (iterator))))
             (hs-insert result (funcall fn value)))))
    result))

(defmacro dohashset ((var hash-set &optional result) &body body)
  ;; magic due to pjb from #lisp
  `(block nil (hs-map (lambda (,var)
                        (tagbody ,@body))
                      ,hash-set)
          ,result))

(defun list-to-hs (list)
  (let ((hash-set (make-instance 'hash-set)))
    (loop for elt in list do
         (if (consp elt)
             (hs-insert hash-set (list-to-hs elt))
             (hs-insert hash-set elt)))
    hash-set))

(defun hs-to-list (hash-set)
  (let ((result ()))
    (dohashset (elt hash-set)
      (if (eq (type-of elt) 'hash-set)
          (push (hs-to-list elt) result)
          (push elt result)))
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

(defun hs-union (hs-a hs-b)
  (let ((result (make-instance 'hash-set)))
    (dohashset (elt hs-a)
      (hs-insert result elt))
    (dohashset (elt hs-b)
      (hs-insert result elt))
    result))
    
(defun hs-intersection (hs-a hs-b)
  (let ((result (make-instance 'hash-set)))
    (dohashset (elt hs-a)
      (when (hs-memberp hs-b elt)
        (hs-insert result elt)))
    result))

(defun hs-equal (hs-a hs-b)
  (if (/= (hs-count hs-a) (hs-count hs-b))
      nil
      (progn
        (dohashset (elt hs-a)
          (unless (hs-memberp hs-b elt)
            (return nil)))
        t)))

(defun hs-copy (hash-set)
  (let ((hs-copy (make-instance 'hash-set)))
    (dohashset (elt hash-set)
      (hs-insert hs-copy elt))
    hs-copy))

(defun hs-difference (hs-a hs-b)
  (let ((result (hs-copy hs-a)))
    (dohashset (elt hs-b)
      (hs-delete result elt))
    result))

(defun hs-symmetric-difference (hs-a hs-b)
  (hs-union (hs-difference hs-a hs-b)
            (hs-difference hs-b hs-a)))

;; (defun hs-cartesian-product (hs-a hs-b) 1
;; nil)

;; (defun hs-powerset (hash-set)
;;   nil)

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

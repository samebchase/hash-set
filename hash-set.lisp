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


(defparameter *assumed-filter-size* 0.5)

(defparameter *show-elements* t)

(defclass hash-set ()
  ((table :accessor table
          :initform (make-hs-hash-table 10)
          :initarg :table
          :type hash-table
          :documentation "The underlying hash-table storing the values of the set."))
  (:documentation "A hashset."))


(defun make-hash-set (&optional (size-hint 10))
  "Create a new `hash-set`, allocating at least enough initial capacity to store `size-hint` elements.
{}"
  (declare (type integer size-hint))
  (make-instance 'hash-set :table (make-hs-hash-table size-hint)))

(defun hs-map (fn hash-set)
  "Create a new `hash-set` whose elements are calculated by calling `fn` on every element of `hash-set`.
{(fn x) : x ∈ hash-set }"
  (let ((result (make-hash-set (hs-count hash-set))))
    (loop
      :for key :being :the :hash-keys :of (table hash-set)
      :do (hs-ninsert result (funcall fn key)))
    result))

(defmacro dohashset ((var hash-set &optional result) &body body)
  "Evaluate `body` for every element `var` of `hash-set`.  Returns result."
  `(progn
     (loop
       :for ,var :being :the :hash-keys :of (table ,hash-set)
       :do
          (tagbody
             ,@body))
     ,result))

(defun hs (&rest values)
  "Create a `hash-set` containing values."
  (list-to-hs values))

(defun list-to-hs (list)
  "Create a `hash-set` containing the elements of list."
  (let ((hash-set (make-hash-set (length list))))
    (dolist (elt list)
      (hs-ninsert hash-set elt))
    hash-set))

(defun hs-to-list (hash-set)
  "Create a list containing the elements of `hash-set`."
  (let ((result ()))
    (dohashset (elt hash-set (nreverse result))
      (push elt result))))

(defun hash-keys-to-set (hash-table)
  "Create a `hash-set` containing the `:hash-keys` of `hash-table` as elements.  See also `hash-values-to-set`."
  (let ((result (make-hash-set (hash-table-count hash-table))))
    (loop :for key :being :the :hash-keys :of hash-table
          :do (hs-ninsert result key))
    result))

(defun hash-values-to-set (hash-table)
  "Create a `hash-set` containing the `:hash-values` of `hash-table` as elements.  See also `hash-keys-to-set`."
  (let ((result (make-hash-set (hash-table-count hash-table))))
    (loop :for value :being :the :hash-values :of hash-table
          :do (hs-ninsert result value))
    result))

(defun hash-table-to-set (hash-table)
  "Create a `hash-set` containing key value pairs of the entries in `hash-table`."
  (let ((result (make-hash-set (hash-table-count hash-table))))
    (loop :for key :being :the :hash-keys :of hash-table
            :using (hash-value value)
          :do (hs-ninsert result (cons key value)))
    result))

(defun hs-count (hash-set)
  "Return the number of elements in `hash-set`."
  (declare (type hash-set hash-set))
  (hash-table-count (table hash-set)))

(defun hs-emptyp (hash-set)
  "`t` if `hash-set` is empty.  `nil` otherwise."
  (declare (type hash-set hash-set))
  (zerop (hs-count hash-set)))

(defun hs-memberp (hash-set item)
  "Test if `item` is an element of `hash-set`."
  (declare (type hash-set hash-set))
  (nth-value 1 (gethash item (table hash-set))))

(defun hs-equal (hs-a hs-b)
  "Test that `hs-a` and `hs-b` have the same `hs-count` and, if so, that every element of `hs-a` is a member of `hs-b`."
  (declare (type hash-set hs-a hs-b))
  (when (/= (hs-count hs-a) (hs-count hs-b))
    (return-from hs-equal nil))
  (dohashset (elt hs-a t)
    (when (not (hs-memberp hs-b elt))
      (return-from hs-equal nil))))

(defun hs-copy (hash-set &optional (extra-capacity 0))
  "Return a copy of `hash-set`, with `extra-capacity` extra capacity pre-allocated."
  (declare (type hash-set hash-set)
           (type fixnum extra-capacity))
  (let ((hs-copy (make-hash-set (+ extra-capacity (hs-count hash-set)))))
    (dohashset (elt hash-set hs-copy)
      (hs-ninsert hs-copy elt))))

(defun hs-filter (predicate hash-set)
  "Return a copy of `hash-set` where every element satisfies `predicate`."
  (declare (type hash-set hash-set)
           (type function predicate))
  (let ((result (make-hash-set (floor (* *assumed-filter-size* (hs-count hash-set))))))
    (dohashset (elt hash-set result)
      (when (funcall predicate elt)
        (hs-ninsert result elt)))))



(defun hs-insert (hash-set item)
  "Return a copy of `hash-set` containing `item`. Non-mutating version of `hs-ninsert`."
  (declare (type hash-set hash-set))
  (hs-ninsert (hs-copy hash-set 1) item))

(defun hs-ninsert (hash-set item)
  "Add `item` as an element of `hash-set` and return `hash-set`. Mutating version of `hs-insert`."
  (declare (type hash-set hash-set))
  (setf (gethash item (table hash-set)) t)
  hash-set)

(defun hs-remove (hash-set item)
  "Return a copy of `hash-set` that does not contain `item` as an element. Non-mutating version of `hs-nremove`."
  (declare (type hash-set hash-set))
  (hs-nremove (hs-copy hash-set) item))

(defun hs-nremove (hash-set item)
  "Remove `item` from `hash-set` if it was an element, and return `hash-set`. Mutating version of `hs-remove`."
  (declare (type hash-set hash-set))
  (remhash item (table hash-set))
  hash-set)

(defun hs-remove-if (predicate hash-set)
  "Return a copy of `hash-set` where elements that satisfy `predicate` have been `hs-nremove`-ed.  Non-mutating version of `hs-nremove-if`."
  (declare (type hash-set hash-set)
           (type function predicate))
  (hs-nremove-if predicate (hs-copy hash-set)))

(defun hs-nremove-if (predicate hash-set)
  "`hs-nremove` elements of `hash-set` that satisfy `predicate`.  Mutating version of `hs-remove-if`."
  (declare (type hash-set hash-set)
           (type function predicate))
  (dohashset (elt hash-set hash-set)
    (when (funcall predicate elt)
      (hs-nremove hash-set elt))))

(defun hs-remove-if-not (predicate hash-set)
  "Return a copy of `hash-set` where elements that do not satisfy `predicate` have been `hs-nremove`-ed.  Non-mutating version of `hs-nremove-if-not`."
  (declare (type hash-set hash-set)
           (type function predicate))
  (hs-nremove-if-not predicate (hs-copy hash-set)))

(defun hs-nremove-if-not (predicate hash-set)
  "`hs-nremove` elements of `hash-set` that satisfy `predicate`.  Mutating version of `hs-remove-if-not`."
  (declare (type hash-set hash-set)
           (type function predicate))
  (dohashset (elt hash-set hash-set)
    (unless (funcall predicate elt)
      (hs-nremove hash-set elt))))

(defun hs-union (hs-a hs-b)
  "Return a set containing all elements of `hs-a` and `hs-b`."
  (declare (type hash-set hs-a hs-b))
  (hs-nunion (hs-copy hs-a (hs-count hs-b)) hs-b))

(defun hs-nunion (hs-a hs-b)
  "Add all elements of `hs-b` to `hs-a`."
  (declare (type hash-set hs-a hs-b))
  (dohashset (elt hs-b hs-a)
    (hs-ninsert hs-a elt)))

(defun hs-intersection (hs-a hs-b)
  "Return a new hash-set containing elements that are members of both `hs-b` and `hs-a`."
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
  "Remove, with `hs-nremove`, any elements of `hs-a` that are not elements of `hs-b`."
  (declare (type hash-set hs-a hs-b))
  (dohashset (elt hs-a hs-a)
    (unless (hs-memberp hs-b elt)
      (hs-nremove hs-a elt))))

(defun hs-difference (hs-a hs-b)
  "Create a copy of `hs-a` with the elements of `hs-b` removed."
  (declare (type hash-set hs-a hs-b))
  (hs-ndifference (hs-copy hs-a) hs-b))

(defun hs-ndifference (hs-a hs-b)
  "Remove all of the elements of `hs-b` from `hs-a`."
  (declare (type hash-set hs-a hs-b))
  (dohashset (elt hs-b hs-a)
    (hs-nremove hs-a elt)))

(defun hs-symmetric-difference (hs-a hs-b)
  "Return the set of elements that are only in `hs-a` or only in `hs-b` but not both."
  (declare (type hash-set hs-a hs-b))
  (hs-union (hs-difference hs-a hs-b)
            (hs-difference hs-b hs-a)))

(defun hs-subsetp (hs-subset hs-superset)
  "Test that every element of `hs-subset` is also a member of `hs-superset`."
  (declare (type hash-set hs-subset hs-superset))
  (dohashset (subset-elt hs-subset t)
    (unless (hs-memberp hs-superset subset-elt)
      (return-from hs-subsetp nil))))

(defun hs-proper-subsetp (hs-subset hs-superset)
  "Test that every element of `hs-subset` is also a member of `hs-superset` that `hs-subset` contains more elements than `hs-subset`."
  (declare (type hash-set hs-subset hs-superset))
  (and (hs-subsetp hs-subset hs-superset)
       (> (hs-count hs-superset) (hs-count hs-subset))))

(defun hs-supersetp (hs-superset hs-subset)
  "Test that every element of `hs-subset` is also a member of `hs-superset`."
  (declare (type hash-set hs-subset hs-superset))
  (hs-subsetp hs-subset hs-superset))

(defun hs-proper-supersetp (hs-superset hs-subset)
  "Test that every element of `hs-subset` is also a member of `hs-superset` that `hs-subset` contains more elements than `hs-subset`."
  (declare (type hash-set hs-subset hs-superset))
  (hs-proper-subsetp hs-subset hs-superset))

(defun hs-any (predicate hash-set)
  "Test if any elements of `hash-set` satisfy `predicate` and returns `t` and the first element found.  Returns `nil` if no elements satisfy `predicate`"
  (declare (type hash-set hash-set)
           (type function predicate))
  (dohashset (elt hash-set nil)
    (when (funcall predicate elt)
      (return-from hs-any (values t elt)))))

(defun hs-all (predicate hash-set)
  "Tests if all elements in `hash-set` satisfy `predicate`."
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
  "Return the set of all subsets of `hash-set`.
𝒫(hash-set)"
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
  "Return a set of pairs for the elements of `hs-a` and `hs-b`.
{(a, b) | a ∈ hs-a and b ∈ hs-b }"
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

(declaim (inline hs-first))
(defun hs-first (hash-set)
  "Returns the element of `hash-set` that would be returned by `hs-pop` or `hs-npop`."
  (declare (type hash-set hash-set)
           (optimize (speed 3) (space 3)))
  (loop :for i :below 1
        :for key :being :the :hash-keys :of (table hash-set)
        :finally (return key)))

(defun hs-pop (hash-set)
  "Returns `(hs-first hash-set)` and a copy of `hash-set` with `(hs-first hash-set)` removed."
  (declare (type hash-set hash-set)
           (optimize (speed 3) (space 3)))
  (let* ((element (hs-first hash-set))
         (result (hs-remove hash-set element)))
    (values element result)))

(defun hs-npop (hash-set)
  "Removes `(hs-first hash-set)` from `hash-set` and returns it and `hash-set`."
  (declare (type hash-set hash-set)
           (optimize (speed 3) (space 3)))
  (let ((element (hs-first hash-set)))
    (hs-nremove hash-set element)
    (values element hash-set)))


(defmethod print-object ((hash-set hash-set) stream)
  "Print `hash-set` to `stream`."
  (print-unreadable-object (hash-set stream :identity t :type t)
    (cond
      (*show-elements*
       (format stream "(~a) {" (hs-count hash-set))
       (loop
         :for key :being :the :hash-keys :of (table hash-set)
         :do
            (format stream "~a " key))
       (format stream "}"))
      (t
       (format stream "hash-set count: ~a" (hs-count hash-set))))))

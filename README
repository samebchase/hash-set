## hash-set

hash-set is an implementation of the hash-set data structure. It has
constant time lookup, insertion and deletion.

### make-hash-set

```() -> hash-set```

Creates a new hash-set

```lisp

(let ((hash-set (make-hash-set)))
  ;; Operations on hash-set
  )

```

### list-to-hs

```list -> hash-set```

Creates a hash-set containing all the elements of a list.

```lisp
HASH-SET> (list-to-hs (alexandria:iota 10))
#<HASH-SET of count: 10 {1008832EF3}>
```

### hs-to-list

```hash-set -> list```

Creates a list containing all the elements of the hash-set

```lisp
HASH-SET> (hs-to-list (list-to-hs (alexandria:iota 10)))
(0 1 2 3 4 5 6 7 8 9)
```

### hs-count

```hash-set -> integer```

Return the number of elements in the hash-set.

### hs-emptyp

```hash-set -> bool```

Predicate that tests whether the hash-set is empty or not.

### hs-equal

```hash-set hash-set -> bool```

Compares two hash-sets for equality

### hs-copy

```hash-set -> hash-set```

Returns a copy of the hash-set

### hs-memberp

```hash-set elt -> bool```

Predicate that tests the existence of an element in the hash-set.

### hs-insert

```hash-set elt -> hash-set```

Returns a new hash-set which contains the element ```elt``` in
addition to all the elements of the hash-set given as the argument.

### hs-ninsert

```hash-set elt -> *!hash-set!*```

Inserts elt into the hash-set and returns the modified hash-set.

### hs-remove

```hash-set elt -> hash-set```

Returns a copy of the hash-set, but with the element ```elt``` removed from it.

### hs-nremove

```hash-set elt -> *!hash-set!*```

Removes the element ```elt``` from the hash-set.

### hs-remove-if

```predicate hash-set -> hash-set```

The elements testing true with the predicate are removed from a copy
of the hash-set.

### hs-nremove-if

```predicate hash-set -> *!hash-set!*```

The elements testing true with the predicate are removed from the
hash-set.

### hs-remove-if-not

```predicate hash-set -> hash-set```

The elements testing false with the predicate are removed from a copy
of the hash-set.

### hs-nremove-if-not

```predicate hash-set -> *!hash-set!*```

The elements testing false with the predicate are removed from the
hash-set.

### hs-any

```predicate hash-set -> bool```

A function that returns true if any elements of the hash-set test true with the predicate.

### hs-all

```predicate hash-set -> bool```

A function that returns true if all elements of the hash-set test true with the predicate.

### hs-union

```hash-set hash-set -> hash-set```

Returns a hash-set that is the union of two hash-sets.

### hs-nunion

```hash-set-a hash-set-b -> *!hash-set-a!*```

Returns a modified ```hash-set-a``` with all of ```hash-set-b```s
elements added to it.

### hs-intersection

```hash-set hash-set -> hash-set```

Returns a hash-set that is the intersection of two hash-sets.

### hs-nintersection

```hash-set-a hash-set-b -> *!hash-set-a!*```

Returns a modified ```hash-set-a``` which contains the elements of the intersection of ```hash-set-a``` and ```hash-set-b```.

### hs-difference

```hash-set-a hash-set-b -> hash-set```

Returns a hash-set that is the set-difference of ```hash-set-a``` and ```hash-set-b```.

### hs-ndifference

```hash-set-a hash-set-b -> *!hash-set-a!*```

Returns a modified ```hash-set-a``` that contains the elements of the
set-difference of ```hash-set-a``` and ```hash-set-b```.

### hs-symmetric-difference

```hash-set-a hash-set-b -> hash-set```

Returns a hash-set with the common elements removed.

```
HASH-SET> (hs-to-list (hs-symmetric-difference (list-to-hs '(1 2 3 4)) (list-to-hs '(3 4 5 6))))
(1 2 5 6)
```

### hs-subsetp

```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a subset of ```hash-set-b```.

### hs-proper-subsetp

```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a proper-subset of ```hash-set-b```.

### hs-supersetp

```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a superset of ```hash-set-b```.

### hs-proper-supersetp

```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a proper-superset of ```hash-set-b```.

### hs-powerset

```hash-set -> hash-set```

Returns the powerset of the hash-set.

### hs-cartesian-product

```hash-set-a hash-set-b -> hash-set```

Returns the hash-set containing the elements of the cartesian product of ```hash-set-a``` and ```hash-set-b```.

```lisp
HASH-SET> (hs-to-list (hs-cartesian-product (list-to-hs (alexandria:iota 3 :start 1)) 
                                            (list-to-hs (alexandria:iota 3 :start 10))))
((1 10) (1 11) (1 12) (2 10) (2 11) (2 12) (3 10) (3 11) (3 12))
```

### hs-map

```function hash-set -> hash-set```

Maps a function over a hash-set and returns a hash-set containing all the mapped values.

### hs-filter

```function hash-set -> hash-set```

Filters out elements from a hash-set that test true with ```function```.

### dohashset

Do something with each element of the hash-set.

```lisp

HASH-SET> (hs-to-list (hs-cartesian-product (list-to-hs (alexandria:iota 3 :start 1)) 
                                            (list-to-hs (alexandria:iota 3 :start 10))))
((1 10) (1 11) (1 12) (2 10) (2 11) (2 12) (3 10) (3 11) (3 12))
```

### hs-map

```function hash-set -> hash-set```

Maps a function over a hash-set and returns a hash-set containing all the mapped values.

### hs-filter

```function hash-set -> hash-set```

Filters out elements from a hash-set that test true with ```function```.

### dohashset

Do something with each element of the hash-set.

```lisp

HASH-SET> (hs-to-list (hs-cartesian-product (list-to-hs (alexandria:iota 3 :start 1)) 
                                            (list-to-hs (alexandria:iota 3 :start 10))))
((1 10) (1 11) (1 12) (2 10) (2 11) (2 12) (3 10) (3 11) (3 12))
```

### hs-map

```function hash-set -> hash-set```

Maps a function over a hash-set and returns a hash-set containing all the mapped values.

### hs-filter

```function hash-set -> hash-set```

Filters out elements from a hash-set that test true with ```function```.

### dohashset

Do something with each element of the hash-set.

```lisp
HASH-SET> (dohashset (elt (list-to-hs (alexandria:iota 10)))
            (princ elt))
0123456789
NIL
```


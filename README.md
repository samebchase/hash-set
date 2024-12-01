## hash-set

hash-set is an implementation of the hash-set data structure. It has
constant time lookup, insertion and deletion.

All tests are known to run successfully on SBCL, CCL, ECL, ABCL and CLISP.

Basic usage:

1. Please install [Quicklisp](http://www.quicklisp.org/) first.
2. ```(ql:quickload 'hash-set)```

### Function reference

Note: ```*!hash-set!*``` means the hash-set is destructively
modified. Functions that are destructive have an 'n' in front of their
name like CL's ```reverse``` and ```nreverse```. So, the destructive
version of ```hs-insert``` is ```hs-ninsert```.

##### make-hash-set : ```() -> hash-set```

Creates a new hash-set.

```lisp
(let ((hash-set (make-hash-set)))
  ;; Operations on hash-set
  )
```

##### list-to-hs : ```list -> hash-set```

Creates a hash-set containing all the elements of a list.

```lisp
HASH-SET> (list-to-hs (alexandria:iota 10))
#<HASH-SET of count: 10 {1008832EF3}>
```

##### hs : ```&rest elements -> hash-set```
Convenience wrapper around ```list-to-hs``` taking ```&rest``` arguments.

```lisp
HASH-SET> (hs 1 2 3 4 5)
#<HASH-SET of count: 5 {1005343743}>
```

##### hs-to-list : ```hash-set -> list```

Creates a list containing all the elements of the hash-set.

```lisp
HASH-SET> (hs-to-list (list-to-hs (alexandria:iota 10)))
(0 1 2 3 4 5 6 7 8 9)
```

##### hs-count : ```hash-set -> integer```

Return the number of elements in the hash-set.

```lisp
HASH-SET> (hs-count (list-to-hs '(4 5 6 7)))
4
```

##### hs-emptyp : ```hash-set -> bool```

Predicate that tests whether the hash-set is empty or not.

```lisp
HASH-SET> (hs-emptyp (make-hash-set))
T
```

##### hs-equal : ```hash-set hash-set -> bool```

Compares two hash-sets for equality.

```lisp
HASH-SET> (hs-equal (list-to-hs '(7 8 9))
                    (list-to-hs '(7 8 9)))
T
```

##### hs-copy : ```hash-set -> hash-set```

Returns a copy of the hash-set.

```lisp
HASH-SET> (let ((hash-set (list-to-hs '(1 2 3 4))))
            (hs-equal hash-set
                      (hs-copy hash-set)))
T
```

##### hs-memberp : ```hash-set elt -> bool```

Predicate that tests the existence of an element in the hash-set.

```lisp
HASH-SET-TEST> (let ((hash-set (list-to-hs '(1 2 3 4))))
                 (hs-memberp hash-set 4))
T
HASH-SET-TEST> (let ((hash-set (list-to-hs '(1 2 3 4))))
                 (hs-memberp hash-set 8))
NIL
```

##### dohashset

Do something with each element of the hash-set.

```lisp
HASH-SET> (dohashset (elt (list-to-hs (alexandria:iota 10)))
            (princ elt))
0123456789
NIL
```

##### hs-map : ```function hash-set -> hash-set```

Maps a function over a hash-set and returns a hash-set containing all
the mapped values.

```lisp
HASH-SET> (hs-to-list (hs-map (lambda (x) (* x x))
                              (list-to-hs (alexandria:iota 10))))
(0 1 4 9 16 25 36 49 64 81)
```

##### hs-filter : ```function hash-set -> hash-set```

Filters out elements from a hash-set that test true with ```function```.

```lisp
HASH-SET> (hs-to-list (hs-filter #'oddp
                                 (list-to-hs (alexandria:iota 10))))
(1 3 5 7 9)
```

#### Insertion/Deletion

##### hs-insert : ```hash-set elt -> hash-set```

Returns a new hash-set which contains the element ```elt``` in
addition to all the elements of the hash-set given as the argument.

```lisp
HASH-SET> (hs-to-list (hs-insert (list-to-hs '(4 5 6)) 123))
(4 5 6 123)
```

##### hs-ninsert : ```hash-set elt -> *!hash-set!*```

Inserts elt into the hash-set and returns the modified hash-set.

```lisp
HASH-SET> (let ((hash-set (list-to-hs '(1 2 3 4))))
            (hs-ninsert hash-set 1984)
            (hs-to-list hash-set))
(1 2 3 4 1984)
```

##### hs-remove : ```hash-set elt -> hash-set```

Returns a copy of the hash-set, but with the ```elt``` removed from
it.

```lisp
HASH-SET> (hs-to-list (hs-remove (list-to-hs '(4 5 6 7)) 5))
(4 6 7)
```

##### hs-nremove : ```hash-set elt -> *!hash-set!*```

Removes the element ```elt``` from the hash-set.

##### hs-remove-if : ```predicate hash-set -> hash-set```

```lisp
HASH-SET> (hs-to-list (hs-remove-if #'evenp
                                    (list-to-hs (alexandria:iota 10))))
(1 3 5 7 9)
```

The elements testing true with the predicate are removed from a copy
of the hash-set.

##### hs-nremove-if : ```predicate hash-set -> *!hash-set!*```

The elements testing true with the predicate are removed from the
hash-set.

##### hs-remove-if-not : ```predicate hash-set -> hash-set```

The elements testing false with the predicate are removed from a copy
of the hash-set.

##### hs-nremove-if-not : ```predicate hash-set -> *!hash-set!*```

The elements testing false with the predicate are removed from the
hash-set.

##### hs-first : ```hash-set -> elt```
Returns an arbitrary element of hash-set.  This would be the element returned by ```hs-pop```
or ```hs-npop```

##### hs-pop : ```hash-set -> elt hash-set```

Removes an arbitrary element of hash-set and returns both it and a copy of hash-set
with the element removed.
Returns nil on an empty set.

```lisp
HASH-SET> (hs-pop  (hs 1 2 3 4 5))
1
#<HASH-SET of count: 4 {104DE04E43}>

HASH-SET> (hs-pop (hs))
NIL
#<HASH-SET of count: 0 {104DE05CD3}>
```


##### hs-npop : ```hash-set -> elt *!hash-set!*```
Modifying version of ```hs-pop```.
Removes an arbitrary element of hash-set and returns both it and hash-set with the
element removed.
Returns nil on an empty set.

```lisp
HASH-SET> (let ((hs (hs 1 2 3 4 5)))
           (hs-npop hs)
           (hs-npop hs)
           (hs-npop hs))
3
#<HASH-SET of count: 2 {104DF7DDD3}>

HASH-SET> (hs-npop  (hs))
NIL
#<HASH-SET of count: 0 {104DE05CD3}>
```

```hs-pop``` and ```hs-npop``` are useful for iterating over sets when the size can change
during the loop.
```lisp
HASH-SET> (loop :with hs = (list-to-hs (alexandria:iota 10))
                :while (not (zerop (hs-count hs)))
                :for removed = (hs-npop hs)
                :when (evenp removed)
                  :do
                     (dotimes (i 3)
                       (hs-ninsert hs (random 20)))
                :do
                   (format t "hs is now: ~a~%" (hs-to-list hs)))
```


#### Set operations

##### hs-any : ```predicate hash-set -> bool```

A function that returns true if any elements of the hash-set test true
with the predicate.

```lisp
HASH-SET> (hs-any #'oddp (list-to-hs '(2 4 6 8 9)))
T
```

##### hs-all : ```predicate hash-set -> bool```

A function that returns true if all elements of the hash-set test true
with the predicate.

```lisp
HASH-SET> (hs-all #'evenp (list-to-hs '(2 4 6 8 9)))
NIL
```

##### hs-union : ```hash-set hash-set -> hash-set```

Returns a hash-set that is the union of two hash-sets.

```lisp
HASH-SET> (hs-to-list (hs-union (list-to-hs '(1 2 3))
                                (list-to-hs '(4 5 6))))
(1 2 3 4 5 6)
```

##### hs-nunion : ```hash-set-a hash-set-b -> *!hash-set-a!*```

Returns a modified ```hash-set-a``` with all of ```hash-set-b```s
elements added to it.

##### hs-intersection : ```hash-set hash-set -> hash-set```

Returns a hash-set that is the intersection of two hash-sets.

##### hs-nintersection : ```hash-set-a hash-set-b -> *!hash-set-a!*```

Returns a modified ```hash-set-a``` which contains the elements of the
intersection of ```hash-set-a``` and ```hash-set-b```.

##### hs-difference : ```hash-set-a hash-set-b -> hash-set```

Returns a hash-set that is the set-difference of ```hash-set-a``` and ```hash-set-b```.

```lisp
HASH-SET> (hs-to-list (hs-intersection (list-to-hs '(1 2 3 4))
                                       (list-to-hs '(3 4 5 6))))
(3 4)
```

##### hs-ndifference : ```hash-set-a hash-set-b -> *!hash-set-a!*```

Returns a modified ```hash-set-a``` that contains the elements of the
set-difference of ```hash-set-a``` and ```hash-set-b```.

##### hs-symmetric-difference : ```hash-set-a hash-set-b -> hash-set```

Returns a hash-set with the common elements removed.

```lisp
HASH-SET> (hs-to-list (hs-symmetric-difference (list-to-hs '(1 2 3 4))
                                               (list-to-hs '(3 4 5 6))))
(1 2 5 6)
```

##### hs-subsetp : ```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a subset of ```hash-set-b```.

```lisp
HASH-SET> (hs-subsetp (list-to-hs '(1 2)) (list-to-hs '(1 2 3)))
T
```

##### hs-proper-subsetp : ```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a proper-subset of ```hash-set-b```.

##### hs-supersetp : ```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a superset of ```hash-set-b```.

##### hs-proper-supersetp : ```hash-set-a hash-set-b -> bool```

Returns ```t``` if ```hash-set-a``` is a proper-superset of ```hash-set-b```.

##### hs-powerset : ```hash-set -> hash-set```

Returns the powerset of the hash-set.

```lisp
HASH-SET> (hs-to-list (hs-powerset (list-to-hs '(1 2 3))))
(NIL (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))
```

##### hs-cartesian-product : ```hash-set-a hash-set-b -> hash-set```

Returns the hash-set containing the elements of the cartesian product
of ```hash-set-a``` and ```hash-set-b```.

```lisp
HASH-SET> (hs-to-list (hs-cartesian-product (list-to-hs (alexandria:iota 3 :start 1)) 
                                            (list-to-hs (alexandria:iota 3 :start 10))))
((1 10) (1 11) (1 12) (2 10) (2 11) (2 12) (3 10) (3 11) (3 12))
```

For even more usage examples please see ```test.lisp```.

### Running the tests

```lisp
CL-USER> (ql:quickload 'hash-set-tests)
To load "hash-set-tests":
  Load 1 ASDF system:
    hash-set-tests
; Loading "hash-set-tests"
[package hash-set]................................
[package hash-set-test].
(HASH-SET-TESTS)

CL-USER> (in-package :hash-set-test)
#<PACKAGE "HASH-SET-TEST">
HASH-SET-TEST> (run!)

Running test suite ALL-TESTS
...
```

### Credits

Engineering guidance taken from Robert Smith's
[map-set](https://bitbucket.org/tarballs_are_good/map-set) and Takaya
Ochiai's [cl-intset](https://github.com/tkych/cl-intset) libraries.

#### Contributors
[Javier Olaechea](http://www.puercopop.com) 

#### Thanks to
The people at #lisp for their help and guidance.

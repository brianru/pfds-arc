; Ch 3.2 Binomial Heaps

(deftem 'node 'r nil 'x nil 'c nil)

; we always link trees of equal rank
(def link (t1 t2)
  (if (<= t1!x t2!x)
    (inst 'node 'r (+ t1!r 1) 'x t1!x 'c (cons t2 t1!c))
    (inst 'node 'r (+ t1!r 1) 'x t2!x 'c (cons t1 t2!c))))

(deftem 'heap 'ts nil)

(def instree (t1 ts)
; creating a new heap and inserting a new min tree to an existing heap are the same operation
  (if (empty ts)        (cons t1 ts)
      (< t1!r car.ts!r) (cons t1 ts)
                        (instree (link t1 car.ts) cdr.ts)))

(def insert (x ts) (instree (inst 'node 'r 0 'x x 'c ts)))

(def merge (ts1 ts2)
  (if (no:and ts1 ts2)        (or ts1 ts2)
      (< car.ts1!r car.ts2!r) (cons car.ts1 (merge cdr.ts1 ts2))
      (< car.ts2!r car.ts1!r) (cons car.ts2 (merge ts1 cdr.ts2))
                              (instree (link car.ts1 car.ts2) (merge cdr.ts1 cdr.ts2))))

(def removemintree (hp)
  (if (single hp)
    (car hp)
    (with ((tp tsp) (removemintree cdr.hp))
      (if (<= car.hp!x tp!x) ; compare root of first tree to min root of all other trees in heap
        (list car.hp cdr.hp)
        (list tp (cons car.hp tsp))))))

(def findmin (ts) (car:removemintree ts))

(def deletemin (ts)
  (with ((a d) (removemintree ts))
    (merge (rev a!c) d)))

; exercise 3.5 Define findmin directly rather than via a call to removemintree.

(def findmin-3-5 (ts)
  (if (single ts)             (car ts)
      (<= car.ts!x cadr.ts!x) (car ts)
                              (findmin-3-5 cdr.ts)))

; exercise 3.6
; 
; Most of the rank annotations in this representation of binomial heaps are redundant
; becauase we know that the children of a node of rank r have ranks r - 1, ..., 0.
; 
; Thus, we can remove the rank annotations from each node and instead
; pair each tree at the top-level with its rank, i.e.,
;   datatype Tree = Node of Elem x Tree list
;   type Heap = (int x Tree) list
;
; Reimplement binomial heaps with this new representation.

(deftem 'node-6 'x nil 'c nil)

(deftem 'heap-6 'ts nil) ; each elem in 'ts is (rank node)

(def rank-6 (n) (len n!c))

(def root-6 (n) n!x)

(def link-6 (t1 t2)
  (if (<= t1!x t2!x)
    (inst 'node-6 'x t1!x 'c (cons t2 t1!c))
    (inst 'node-6 'x t2!x 'c (cons t1 t2!c))))

(def instree-6 (t1 ts)
  (if (empty ts)       (cons (list rank-6.t1 t1) ts)
      (< t1!r caar.ts) (cons (list rank-6.t1 t1) ts)
                       (instree-6 (link-6 t1 (last car.ts)) cdr.ts))) 

(def insert-6 (x ts) (instree (inst 'node-6 'x x 'c ts)))

(def merge-6 (ts1 ts2)
  (if (no:and ts1 ts2)      (or ts1 ts2)
      (< caar.ts1 caar.ts2) (cons car.ts1 (merge-6 cdr.ts1 ts2))
      (< caar.ts2 caar.ts1) (cons car.ts2 (merge-6 ts1 cdr.ts2))
                            (instree-6 (link caar.ts1 caar.ts2)
                                       (merge-6 cdr.ts1 cdr.ts2))))

(def removemintree-6 (hp)
  (if (single hp)
    (car hp)
    (with ((tp tsp) (removemintree cdr.hp))
      (if (<= ((last car.hp) 'x) last.tp!x)
        (list car.hp cdr.hp)
        (list tp (cons car.hp tsp))))))

(def findmin (ts) (car:removemintree-6 ts))

(def deletemin (ts)
  (with ((a d) (removemintree-6 ts))
    (merge-6 (rev last.a!c) d)))

; exercise 3.7
;
; One clear advantage of leftist heaps over binomial heaps is that findMin
; takes only O(1) time, rather than O(logn) time. The following functor
; skeleton improves the running time of findMin to O(1) by storing the
; minimum element separately from the rest of the heap.
;
;   functor ExplicitMin (H : Heap) : Heap =
;   struct
;         structure Elem = H.Elem
;         datatype Heap  = E | NE of Elem.T x H.Heap
;         ...
;   end
;
; Noe that this functor is not specific to binomial heaps, but rather takes
; any implementation of heaps as a parameter. Complete this functor so that
; findMin takes O(1) time, and insert, merge, and deleteMin take O(logn) time
; (assuing that all four take O(logn) time or better for the underlying
; implementation H).




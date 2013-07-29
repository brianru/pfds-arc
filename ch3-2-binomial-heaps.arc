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
                        (instree (link t car.ts) cdr.ts)))

(def insert (x ts) (instree (inst 'node 'r 0 'x x 'c ts)))

(def merge (ts1 ts2)
  (if (no:and ts1 ts2) (or ts1 ts2)
      (< car.ts1!r car.ts2!r) (cons car.ts1 (merge cdr.ts1 ts2))
      (< car.ts2!r car.ts1!r) (cons car.ts2 (merge ts1 cdr.ts2))
                              (instree (link car.ts1 car.ts2) (merge cdr.ts1 cdr.ts2))))

(def removemintree (hp)
  (if (single hp) (car hp)
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

(require "../pfds-arc/bigo.arc")
; TODO can I use unit testing to verify whether the exercises are passed?

; Exercise 2.1
;
; Write a function suffixes of type "a list -> a list list" that takes a list xs and returns a list of all the suffixes of xs in decreasing order of length.
; For example, 
;   suffixes[1,2,3,4] = [[1,2,3,4], [2,3,4], [3,4], [4], []]
;
; Show that the resulting list of suffixes can be generated in O(n) time and represented in O(n) space.

(def suffixes (xs)
  (if (empty xs)
      '()
      (cons xs (suffixes (cdr xs)))))

(deftem node
  d nil  ; node
  l nil  ; left sub-tree
  r nil) ; right sub-tree

; TODO replace < with f< which is passed in as an arg to each function

(def member (x b)
  (if (no b)    nil
      (< x b!d) (member x b!l)
      (< b!d x) (member x b!r)
                t))
    
(def insert (x b)
  (if (no b)    (inst 'node 'd x)
      (< x b!d) (inst 'node 'l (insert x b!l) 'd b!d 'r b!r)
      (< b!d x) (inst 'node 'l b!l 'd b!d 'r (insert x b!r))
                 b))

; exercise 2.2
; In the worst case, member performs approximately 2d comparisons, where d is the depth of the tree. Rewrite member to take no more than d + 1 comparisons by keeping track of a candidate element that might be equal to the query element (say, the element for which < returned fales of <= returned true) and checking for equality only when you hit the bottom of the tree.
; TODO refactor to maintain std member signature
; create separate rmember2-2 function that tracks candidate
(def member-2-2 (x b (o c nil))
  (if (no b)             (if (is c x) t nil)
      (< x b!d)          (member-2-2 x b!l c)
      (or 
        (no (< x b!d))
        (<= x b!d))      (member-2-2 x b!r b!d)))


; exercise 2.3
; Inserting an existing element into a binary search tree copies the entire search path even though the copied nodes are indistringuishable from the originals. Rewrite insert using exceptions to avoid this copying. Establish only one handler per insertion rather than one handler per iteration.
(def insert-2-3 (x b)
  (catch (rinsert-2-3 x b b)))

; TODO I don't like this. It still copies the nodes, just ignores the copies if they're found to be extraneous.
(def rinsert-2-3 (x b root)
  (if (no b)    (inst 'node 'd x)
      (< x b!d) (inst 'node 'l (rinsert-2-3 x b!l root) 'd b!d 'r b!r)
      (< b!d x) (inst 'node 'l b!l 'd b!d 'r (rinsert-2-3 x b!r root))
                (throw root)))

; exercise 2.4
; Combine the ideas of the previous two exercises to obtain a version of insert that performs no unnecessary copying and uses no more than d + 1 comparisons.
(def insert-2-4 (x b)
  (catch (rinsert-2-4 x b b)))

(def rinsert-2-4 (x b root (o c nil))
  (if (no b)           (if (is c x) (throw root) (inst 'node 'd x))
      (< x b!d)        (inst 'node 'l (rinsert-2-4 x b!l root c) 'd b!d 'r b!r)
      (or
        (no (< x b!d))
        (<= x b!d))    (inst 'node 'l b!l 'd b!d 'r (rinsert-2-4 x b!r root b!d))))
  
; exercise 2.5
; Sharing can also be useful within a single object, not just between objects. For example, if the two subtrees of a given node are identical, then they can be represented by the same tree.
; (a) Using this idea, write a function "complete" of type Elem x int -> Tree where
;     "complete(x, d) creates a complete binary tree of depth d with x stored in every node.
;     (Of course, this function makes no sense for the set abstraction, but it can be useful as an auxiliary function for other abstractions, such as bags.) This function should run in O(d) time.
(def complete-2-5 (x d)
  (if (< d 1) nil
              (let st (complete-2-5 x (- d 1))
                (inst 'node 'l st 'd x 'r st))))

; bad-complete implementation does not share object for left/right subtrees
(def bad-complete-2-5 (x d)
  (if (< d 1) nil
              (inst 'node 'l (bad-complete-2-5 x (- d 1)) 'd x 'r (bad-complete-2-5 x (- d 1)))))

; (b) Extend this function to create balanced trees of arbitrary size. These trees will not always be complete binary trees, but should be as balanced as possible:  for any given node, the two subtrees should differ in size by at most one. This function should run in O(log n) time. (Hint: use a helper function "create2" that, given a size m, creates a pair of trees, one of size m and one of size m+1.
(def balanced-2-5 (x m)
  (if (< m 1)        nil
      (is m 1)       (inst 'node 'd x)
      (even (- m 1)) (let st (balanced-2-5 x (/ (- m 1) 2))
                       (inst 'node 'l st 'd x 'r st))
                     (withs (half (trunc (/ (- m 1) 2))
                             l-st (balanced-2-5 x half)
                             r-st (balanced-2-5 x (+ half 1)))
                       (inst 'node 'l l-st 'd x 'r r-st))))

; exercise 2.6
; Adapt the UnbalancedSet functor to support finite maps rather than sets. Figure 2.10 gives a minimal signature for finite maps. (Note that the NotFound exception is not predefined in Standard ML--you will have to define it yourself. Although this exception could be made part of the FiniteMap signature, with every impleentation defining its own NotFound exception, it is convenient for all finite maps to use the same exception.
(deftem 'mnode 'l nil 'd nil 'r nil)

; pg's code
; source: http://www.arclanguage.com/item?id=2330
(def bst-mem (x b f<) ; x is proposed node, b is target bst, f< is comparison function
  (if (no b)     nil
      (is x b!d) t
                 (bst-mem x
                         (b (if (f< x b!d) 'l 'r))
                         f<))) 

(def bst-ins (x b f<)
  (if (no b)     (inst 'node 'd x)
      (is x b!d) b
                 (recurse bst-ins x b f<)))

(def recurse (f x b f<)
  (with (side (if (f< x b!d) 'l 'r)
         bc (copy b))
    (= (bc side) (f x b.side f<))
    bc))


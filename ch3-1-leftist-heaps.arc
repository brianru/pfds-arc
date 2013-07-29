; Ch 3.1 Leftist Heaps

; exercise 3.1
; Prove that the right spine of a leftist heap of size n contains at most floor(log(n + 1)) elements. (All algorithms in this book are base 2 unless otherwise indicated.)
;
; Informal proof:
;   The height of a balanced binary tree is log(n + 1).
;   A leftist heap does not require the tree to be balanced, but does require the height of left spines to be >= height of right spines.
;   Therefore, a leftist heap will contain more elements outside the right spine than a balanced tree and the right spine of a leftist heap can contain no more than log(n + 1) elements, the number of elements in a balanced tree.

(deftem 'node 'r nil 'x nil 'a nil 'b nil)

(def rank (h)
  (if h h!r 0))

(def maket (x a b)
  (if (>= rank.a rank.b) ; enforce leftist property. swap children if rank.b > rank.a
    (inst 'node 'r (+ 1 rank.b) 'x x 'a a 'b b)
    (inst 'node 'r (+ 1 rank.a) 'x x 'a b 'b a)))

(def isempty (h)
  (if (is (type h) 'tem) nil t))
    
(def merge (h1 h2)
  (if (no (and h1 h2)) (or h1 h2) ; if both aren't there, return whichever is
      (<= h1!x h2!x)   (maket h1!x h1!a (merge h1!b h2)) ; smaller value moves to new root
                       (maket h2!x h2!a (merge h1 h2!b))))
      
(def insert (x h)
  (merge h (inst 'node 'r 1 'x x)))

(def findmin (h)
  (if (isempty h) (err "EMPTY") h!x))

(def deletemin (h)
  (if (isempty h) (err "EMPTY") (merge h!a h!b)))

; exercise 3.2
; Define insert directly rather than via a call to merge.
(def insert-3-2 (x h)
  (if (no h)     (inst 'node 'r 1 'x x)
      (<= x h!x) (maket x h!a (insert-3-2 h!x h!b))
                 (maket h!x h!a (insert-3-2 x h!b)))) 

; exercise 3.3
; Implement a function fromList of type Elem.T list -> Heap that produces a leftist heap from an unordered list of elements by first converting each element into a singleton heap and then merging the heaps until only one heap remains. Instead of merging the heaps in one right-to-left or left-to-right pass using foldr or foldl, merge the heaps in ceiling(log n) passes, where each pass merges adjacent pairs of heaps. Show that fromList takes only O(n) time.
; 1) Create list of singleton heaps
; 2) Recursively merge mapped pairs until 1 element remains in list
(def fromlist (l)
  (if (no:is (type l) 'cons) (err "not a list!")
                             (pairwise-reduce (map [maket _ nil nil] l) merge)))
      
(def pairwise-reduce (l f)
  (if (is (len l) 1)
    (car l)
    (pairwise-reduce (flat:pair l f) f)))
    
; exercise 3.4
; Weight-biased leftist heaps are an alternative to leftist heaps that replace the leftist property with the weight-biased leftist property: the size of any left child is at least as large as the size of its right sibling.
; (a) Prove that the right spine of a weight-biased leftist heap contains at most floor(log(n+1)) elements.
;     It's the same argument as before, except size is used instead of rank.
;     Tree will be more balanced but left is still heavier than right.

; (b) Modify the implementation in Figure 3.2 to obtain weight-biased leftist heaps.
(deftem 'wnode 's nil 'x nil 'a nil 'b nil)

(def size (h)
  (if (no h) 0
      (no:and h!a h!b) 0
      h!s))

(def maket-3-4 (x a b)
  (if (>= size.a size.b)
    (inst 'wnode 's (+ size.a size.b 1) 'x x 'a a 'b b)
    (inst 'wnode 's (+ size.a size.b 1) 'x x 'a b 'b a)))

(def merge-3-4 (h1 h2)
  (if (no:and h1 h2) (or h1 h2)
      (<= h1!x h2!x) (maket-3-4 h1!x h1!a (merge-3-4 h1!b h2))
                     (maket-3-4 h2!x h2!a (merge-3-4 h1 h2!b)))) 

(def insert-3-4 (x h)
  (merge-3-4 h (inst 'wnode 's 1 'x x)))

(def findmin-3-4 (h)
  (findmin h))

(def deletemin-3-4 (h)
  (if (isempty h) (err "empty!")
                  (merge-3-4 h!a h!b)))

; (c) Currently, merge operates in two passes: a top-down pass consisting of calls to merge, and a bottom-up pass consisting of calls to the helper function makeT. Modify merge for weight-biased leftist heaps to operate in a single, top-down pass.
(def merge-3-4c (h1 h2)
  (if (no:and h1 h2) (or h1 h2)
      (with (mh (if (<= h1!x h2!x) h1 h2) ; clean this up
             nmh (if (is mh h1) h2 h1)
             a (if (>= mh!a!s (+ mh!b!s nmh!s)) mh!a (merge-3-4c mh!b nmh))
             b (if (is a mh!a) (merge-3-4c mh!b nmh) mh!a)) 
        (inst 'wnode 's (+ h1!s h2!s 1) ; is this right?
                     'x mh!x
                     'a a
                     'b b))))
; TODO the solution is to inline maket while still recursing down via merge
; doing so means everything happens top down and with lazy evaluation you could get very useful performance characteristics
                      
; I could inline maket...but that would not actually give us tail calls.
; Let's think about what merge is doing.
;
; If h1 or h2 is nil, return the other.
; Else if h1 < h2, make a new root with h1's value, h1's left child, and merge h1's right child with h2s.
;            else, same but flipped 

; (d) What advantages would the top-down version of merge have in a lazy environment? In a concurrrent environment?
;    
;     In a lazy environment the top-down version suspends construction of lower parts of the tree until those parts are needed. This speeds up access to upper elements following merges, as the entire merge does not have to be completed prior to access.
;
;     In a concurrent environment the top-down version allows maintenance of the lower tree to be handled on a background thread while other threads can continue accessing the computed portions of the tree.

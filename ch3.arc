; exercise 3.1
; Prove that the right spine of a leftist heap of size n contains at most floor(log(n + 1)) elements. (All algorithms in this book are base 2 unless otherwise indicated.)
;
; Informal proof:
;   The height of a balanced binary tree is log(n + 1).
;   A leftist heap does not require the tree to be balanced, but does require the height of left spines to be >= height of right spines.
;   Therefore, a leftist heap will contain more elements outside the right spine than a balanced tree and the right spine of a leftist heap can contain no more than log(n + 1) elements, the number of elements in a balanced tree.

(deftem 'node 'r nil 'x nil 'a nil 'b nil)

; TODO refactor. this is ugly.
(def rank (h)
  (if (no h) 0
      h!r    h!r
             0))

; constructs with x a b and calculated r
(def maket (x a b)
  (if (>= rank.a rank.b)
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
  (if (isempty h) (err "EMPTY")
                  h!x))

(def deletemin (h)
  (if (isempty h) (err "EMPTY")
                  (merge h!a h!b)))

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
    
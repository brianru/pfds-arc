; Ch 3.3 Red-Black Trees

; 'c == color
; takes values:
;   1 == red
;   0 == black
(deftem 'tree 'c nil 'a nil 'b nil 'x nil)

; exercise 3.8
;
; Prove that the maximum depth of a node in a red-black tree of size n is at most 2*floor(log(n+1)).
;
; A balanced tree has depth d and size n where d = ceiling(log(n+1)).
; Let s equal the shortest path to a leaf.
; Invariants 1 guarantees p <= 2*s, where p is the longest path to a leaf.
; p/2 <= s <= log(n+1) because the shortest past is <= the depth of the tree
; p <= 2*log(n+1)
;
; Not the most formal proof in the world, but I'm convinced.

(def member (x tree)
  (if (no tree)    nil
      (< x tree!x) (member x tree!a)
      (> x tree!x) (member x tree!b)
                   t))

(def balance (tree) nil)



(def insert (x tree)
  (def ins (s)
    (if (no s) (inst 'tree 'c 'red 'x x)
        (< x s!x) (balance s!c (ins s!a) s!x s!b)
        (> x s!x) (balance s!c s!a s!x (ins s!b))
                  s))
  (let r (ins tree)
    (inst 'node 'c 'black 'a r!a 'x r!x 'b r!b)))

; I don't have pattern matching in Arc. Not fair :-(.



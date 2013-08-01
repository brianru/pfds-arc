; Chapter 4 Lazy Evaluation

; 4.1 % ($) - Notation

; with help from http://cadrlife.blogspot.com/2008/02/lazy-lists-in-arc.html
; and CLAZY http://common-lisp.net/project/clazy/

(mac delay (expr) `(memo (fn () ,expr)))

; sugar, ex: (force (%:+ 2 2)) => 4
(mac % (expr) `(delay ,expr))

; example:
;   (force (delay (+ 2 2))) => 4
(def force (thunk) (apply thunk))

; todo am i ensuring args are evaluated lazily?
; look at def-lazy-function
; http://common-lisp.net/viewvc/clazy/clazy/clazy.lisp?view=markup
(mac deflazy (name args . body)
  `(def ,name (,@args)
     ,@body))

; 4.2 Streams

(mac cons_ (a b) `(cons (delay ,a) (delay ,b)))

(def car_ (li) ((zap force (car li))))

(def cdr_ (li) ((zap force (cdr li))))

(def append (s t) nil) ; todo

(def take (n s)
  (if (is n 0) nil
      (no s)   nil
               (cons_ car_.s (take_ (- n 1) cdr_.s))))

(def drop1 (n s)
  (if (is n 0) nil
      (no s)   nil
               (drop1 (- n 1) s)))

(def drop2 (n s)
  (let dp (fn (n s)
            (if (is n 0) nil
                (no s)   nil
                         (dp (- n 1) s)))
    (dp n s)))

; Exercise 4.1
; 
; Use the fact that force ($e) is equivalent to e to show that these two definitions of drop
; are equivalent.
;
; Just look at it. drop1 is assigned to dp within drop2. (force:drop2 n s) would
; just get you (dp n s) which evaluates immediately. Adding a layer of indirection, that's it.

(def reverse (s r)
  (if (no s) r

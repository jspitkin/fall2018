#lang plait
;(print-only-errors #t)

(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

; Part 1 -- sum
(define (sum [t : Tree]) : Number
  (type-case Tree t
    [(leaf v) v]
    [(node v l r) (+ v (+ (sum l) (sum r)))]))

(test (sum (node 5 (leaf 6) (leaf 7))) 18)

(test (sum (leaf 10)) 10)

(test (sum (node 5 (node 5 (leaf 10) (leaf 7)) (leaf 3))) 30)

; Part 2 -- negate
(define (negate [t : Tree]) : Tree
  (type-case Tree t
    [(leaf v) (leaf (* -1 v))]
    [(node v l r) (node (* -1 v) (negate l) (negate r))]))

(test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))

(test (negate (leaf 10)) (leaf -10))

(test (negate (node 5 (node 5 (leaf 10) (leaf 7)) (leaf 3))) (node -5 (node -5 (leaf -10) (leaf -7)) (leaf -3)))

; Part 3 -- contains?
(define (contains? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
    [(leaf v) (equal? v n)]
    [(node v l r) (or (or (equal? v n) (contains? l n)) (contains? r n))]))

(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)

(test (contains? (node 5 (leaf 6) (leaf 7)) 4) #f)

(test (contains? (node 5 (leaf 6) (leaf 7)) 5) #t)

(test (contains? (node 5 (node 5 (leaf 10) (leaf 7)) (leaf 3)) 10) #t)

(test (contains? (node 5 (node 5 (leaf 10) (leaf 7)) (leaf 3)) 12) #f)

(test (contains? (leaf 5) 4) #f)

(test (contains? (leaf 5) 5) #t)

; Part 4 -- big-leaves?
(define (big-leaves? [t : Tree]) : Boolean
  (bigger-leaves? t 0))

(define (bigger-leaves? [t : Tree] [sum : Number]) : Boolean
  (type-case Tree t
    [(leaf v) (> v sum)]
    [(node v l r) (and (bigger-leaves? l (+ v sum)) (bigger-leaves? r (+ v sum)))]))

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)

(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)

(test (big-leaves? (leaf 3)) #t)

; Part 5 -- sorted?
; Not working correctly - only checking if the parents of the leafs are proper
(define (sorted? [t : Tree]) : Boolean
  (type-case Tree t
    [(leaf v) #t]
    [(node v l r) (and (sorted-helper l v #t) (sorted-helper r v #f))]))

(define (sorted-helper [t : Tree] [pv : Number] [left? : Boolean]) : Boolean
  (type-case Tree t
    [(leaf v) (cond
                [left? (<= v pv)]
                [else (>= v pv)])]
    [(node v l r) (and (sorted-helper l v #t) (sorted-helper r v #f))]))

(test (sorted? (node 2 (leaf 1) (leaf 3))) #t)

(test (sorted? (node 1 (leaf 2) (leaf 3))) #f)

(test (sorted? (node 4 (node 2 (leaf 1) (leaf 3)) (leaf 5))) #t)

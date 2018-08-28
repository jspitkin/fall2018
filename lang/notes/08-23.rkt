#lang plait

(define-type Robot-ins
  (forward [feet : Number])
  (left)
  (right))

(define (amount-forward [ins : Robot-ins]) : Number
  (type-case Robot-ins ins
    [(forward f) f]
    [(left) 0]
    [(right) 0]))

(test (amount-forward (forward 4))
      4)
(test (amount-forward (left))
      0)
(test (amount-forward (right))
      0)

(define (is-turn? [ins : Robot-ins]) : Boolean
  (type-case Robot-ins ins
    [(forward f) #f]
    [(left) #t]
    [(right) #t]))

(test (is-turn? (forward 5))
      #f)
(test (is-turn? (left))
      #t)
(test (is-turn? (right))
      #t)

;; -----------------------

#;
(define-type (Listof Robot-ins)
  empty
  (cons [item : Robot-ins]
        [rst : (Listof Robot-ins)]))

(define (total-distance [ris : (Listof Robot-ins)]) : Number
  (type-case (Listof Robot-ins) ris
    [empty 0]
    [(cons i r) (+ (amount-forward i)
                   (total-distance r))]))

(test (total-distance empty)
      0)
(test (total-distance (cons (forward 5) (cons (left) empty)))
      5)
(test (total-distance (cons (forward 5) (cons (forward 4) empty)))
      9)

(define (turns? [ris : (Listof Robot-ins)]) : Boolean
  (type-case (Listof Robot-ins) ris
    [empty #f]
    [(cons i r) (or (is-turn? i)
                    (turns? r))]))

(test (turns? empty)
      #f)
(test (turns? (cons (right) empty))
      #t)
(test (turns? (cons (forward 5) empty))
      #f)
(test (turns? (cons (forward 5) (cons (left) empty)))
      #t)
(test (turns? (cons (left) (cons (forward 5) empty)))
      #t)

; Incomplete
(define (any-u-turn? [ris : (Listof Robot-ins)]
                     [prev : Robot-ins]) : Boolean
  (type-case (Listof Robot-ins) ris
    [empty #f]
    [(cons i r) .. i ... r]))

(test (any-u-turn? (cons (right) (cons (right) empty)))
      #t)
(test (any-u-turn? (cons (forward 5) (cons (left) empty)))
      #f)
(test (any-u-turn? empty)
      #f)
(test (any-u-turn? (cons (forward 10) empty))
      #f)
(test (any-u-turn? (cons (right) (cons (left) empty)))
      #f)
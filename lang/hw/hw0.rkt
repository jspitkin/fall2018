#lang plait
(print-only-errors #t)

; Part 1 -- 3rd-power
(define (3rd-power [n : Number]) : Number
  (* n (* n n)))

(test (3rd-power 17) 4913)

(test (3rd-power 0) 0)

(test (3rd-power 1) 1)

; Part 2 -- 42nd-power
(define (42nd-power [n : Number]) : Number
  (* (* (* (3rd-power (3rd-power (3rd-power n))) (3rd-power (3rd-power n))) (3rd-power n)) (3rd-power n)))

(test (42nd-power 17) 4773695331839566234818968439734627784374274207965089)

; Part 3 -- plural
(define (plural [s : String]) : String
  (cond
    [(equal? 0 (string-length s)) s]
    [(equal? #\y (string-ref s (- (string-length s) 1))) (string-append (substring s 0 (- (string-length s) 1)) "ies")]
    [else (string-append (substring s 0 (string-length s)) "s")]))

(test (plural "baby") "babies")

(test (plural "fish") "fishs")

(test (plural "") "")

; Part 4 -- energy-usage
(define-type Light
  (bulb [watts : Number]
        [technology : Symbol])
  (candle [inches : Number]))

(define (energy-usage [l : Light]) : Number 
  (type-case Light l
    [(bulb w t) (* (/ w 1000) 24)]
    [(candle i) 0]))

(test (energy-usage (bulb 100.0 'halogen)) 2.4)

(test (energy-usage (candle 10.0)) 0)
      

; Part 5 -- use-for-one-hour
(define (use-for-one-hour [l : Light]) : Light
  (type-case Light l
    [(bulb w t) (bulb w t)]
    [(candle i) (candle (- i 1))]))

(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))

(test (use-for-one-hour (candle 10.0)) (candle 9.0))
#lang plait

(define-type Story
  (happy [text : String])
  (sad [text : String])
  (choice [text : String]
          [left : Story]
          [right : Story]))

(define (has-happy? [s : Story]) : Boolean
  (type-case Story s
    [(happy t) #t]
    [(sad t) #f]
    [(choice t l r) (or (has-happy? l) (has-happy? r))]))

(test (has-happy? (choice "pick"
                      (sad "bad")
                      (sad "another bad")))
      #f)

(test (has-happy? (choice "pick"
                      (happy "happy")
                      (sad "sad")))
      #t)

(test (has-happy? (choice "pick"
                      (happy "happy")
                      (happy "happy")))
      #t)

(test (has-happy? (happy "happy"))
      #t)

(test (has-happy? (sad "sad"))
      #f)
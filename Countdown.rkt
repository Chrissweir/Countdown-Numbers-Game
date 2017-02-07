#lang racket

;Define a list called nums which contains the set of numbers that will be chosen at random.
(define nums(list 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100))


(define (calculate x list)
  (+ (car list) x))


(define (generate min max)
  (if (< max min)
      0
    (cons min (generate (+ min 1) max))))

(calculate 5 nums)
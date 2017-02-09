#lang racket

;Define a list called nums which contains the set of numbers that will be chosen at random.
(define nums(list 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100))

;Define empty list to hold the 6 random numbers.
(define n(list))

;Define a variable called target-number that will store the value of the target number.
(define target-number 0)

;Define a function (select-numbers) that will take in a list, then get a random value from that list.
;It will then remove that value from the givin list, and then add that value to the list n.
;The function will then check if the list n is equal to 6, meaning there are 6 numbers in the
;list. If this is so then the function will end, if not then it will repeat with the modified
;list as the input.
(define (select-numbers l)
  (define r(list-ref l (random (length l))))
  (set! l(remove r l))
  (set! n(cons r n))
  (if(= (length n) 6) 
     n
  (select-numbers l)))

;Define a function (get-target-number) that takes in two values (min, max) which will calculate a random
;number between the min and max.
(define (get-target-number min max)
  (set! target-number(+ (random min max) target-number)))

;Main Function Calculate
(define (calculate x list)
  (+ (car list) x))

(get-target-number 101 999)
;(select-numbers nums)
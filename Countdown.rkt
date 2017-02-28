#lang racket
;Author: Christopher Weir
;Student No: G00309429
;Module: Theory of Algorithms
;Project: Countdown Numbers Game Solver

;Define the namespace
(define ns (make-base-namespace))

;Define a list called nums which contains the set of numbers that will be chosen at random.
(define nums(list 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100))

;Define empty list called selected-numbers to hold the 6 random numbers.
(define selected-numbers(list))

;Define a variable called target-number that will store the value of the target number.
(define target-number 10)

;Define custom operators
(define operators (list "+" "-" "/" "*"))

(define (createEquation op n m)
  (string-append "(" op " " (~v n) " " (~v m) ")"))

;Define a function (select-numbers) that will take in a list, then using a for loop[1], get a random[2] value
;from that list using list-ref[3]. It will then remove[4] that value from the givin list, and then add that value
;to the list (selected-numbers) using set![5]. The for loop is set to loop 6 times allowing for 6 numbers to be assigned to
;the list(selected-numbers).
(define (select-numbers numbers)
  (for([i 2])
    (define random-number(list-ref numbers (random (length numbers))))
    (set! numbers(remove random-number numbers))
    (set! selected-numbers(cons random-number selected-numbers))))

;Define a function (get-target-number) that takes in two values (min, max) which will calculate a random
;number between the min and max. Max will be 1000 in this case for there to be a possibility for 999 to be selected.
(define (get-target-number min max)
  (set! target-number(+ (random min max) target-number)) target-number)

;Main Function Calculate
(define (calculate x operators)
  (display "Countdown Number Game Solver\n============================\n")
  (display(string-append "Numbers to choose from: " (~v nums) "\n"))
  (select-numbers nums)
  (display(string-append "Numbers selected: " (~v selected-numbers) "\n"))
 ; (display(string-append "Target number: " (~v (get-target-number 101 1000)) "\n" ));Min = 101, Max - 1000

  ;Get all permutations of the sets
  (define sets(permutations selected-numbers))

  ;For every set in the sets
  (for([set sets])
    ;Display the set
    (display(string-append "\nSet: " (~v set) "\n"))
    (define num1 (car set))
    (define num2 (car (cdr set)))
    (for([o operators])
      (define currentEquation(createEquation o num1 num2))
      (define currentAnswer (eval (read (open-input-string currentEquation)) ns))
      ;Display the calculation
      (display (string-append (~v currentEquation) " = "(~v currentAnswer)))
      ;Evaluate if answer is equal to the answer number
      (if ( = currentAnswer target-number)
          (display (string-append  " --> success" "\n" )) (display (string-append "\n" ))) ;TODO return)
      )
    ))
  
;Call the Main Function
(calculate target-number operators)

;Custom operators - NOT FINALIZED
;(define (?+ a b)
;  (cond
;    [(null? a) 0] 
;    [(null? b) 0] 
;    [(< a b) 0]
;    [(zero? a) 0]
;    [else (+ a b)]))
;
;(define (?- a b)
;  (cond
;    [(null? a) 0] 
;    [(null? b) 0] 
;    [(< a b) 0]
;    [(zero? b) 0]
;    [else (- a b)]))
;
;(define (?* a b)
;  (cond
;    [(null? a) 0] 
;    [(null? b) 0] 
;    [(< a b) 0]
;    [(= a 1) 0]
;    [(= b 1) 0]
;    [(= a 0) 0]
;    [(= b 0) 0]
;    [else (* a b)]))

;(define (?/ a b)
;  (cond
;    [(null? a) 0] 
;    [(null? b) 0] 
;    [(< a b) 0]
;    [(zero? b) 0]
;    [(= b 1) 0]
;    [(inexact? (/ a b)) 0]
;    [else (/ a b)]))


;REFERENCES
;================================
;[1]: for (https://docs.racket-lang.org/guide/for.html).
;[2]: random (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._remove%29%29).
;[3]: list-ref (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29).
;[4]: remove (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._remove%29%29).
;[5]: set! (http://docs.racket-lang.org/reference/set_.html).
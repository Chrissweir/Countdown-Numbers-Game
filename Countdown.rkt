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
(define target-number 0)

;Define custom operators
(define operators (list "+" "-" "/" "*"))

(define permCount 0)
(define correctCount 0)

;Define a function to construct the equation
(define (createEq oper a b)
  (~a "( " oper " "   a  " "  b " )")
  )

;Define a function (select-numbers) that will take in a list, then using a for loop[1], get a random[2] value
;from that list using list-ref[3]. It will then remove[4] that value from the givin list, and then add that value
;to the list (selected-numbers) using set![5]. The for loop is set to loop 6 times allowing for 6 numbers to be assigned to
;the list(selected-numbers).
(define (select-numbers numbers)
  (for([i 6])
    (define random-number(list-ref numbers (random (length numbers))))
    (set! numbers(remove random-number numbers))
    (set! selected-numbers(cons random-number selected-numbers))))

;Define a function (get-target-number) that takes in two values (min, max) which will calculate a random
;number between the min and max. Max will be 1000 in this case for there to be a possibility for 999 to be selected.
(define (get-target-number min max)
  (set! target-number(+ (random min max) target-number)) target-number)

;Main Function Calculate
(define (menu)
  (select-numbers nums)
  (display "Countdown Number Game Solver\n============================\n")
  (display(string-append "Numbers to choose from: " (~v nums) "\n"))
  (display(string-append "Numbers selected: " (~v selected-numbers) "\n"))
  (display(string-append "Target Number: " (~v(get-target-number 101 1000))"\n"));Min = 101, Max - 1000))))
  (display "\nCalculating.....\n")
  (define sets(remove-duplicates(permutations selected-numbers)))
  (for ([set sets])
    (define firstValue (car set))
    (set! set (remove firstValue set))
    (calculate set firstValue)
    )
  (display "==========================================================================")
  )
  

;Initial method for beginning the search
(define(calculate selected-numbers oAnswer )
  ;Outer for loop iterating through each number in list
  (for([currentNumber selected-numbers])
    ;Remove the used number from the current list
    (set! selected-numbers (remove currentNumber selected-numbers))
    ;Inner for loop applying each sign to the equation
    (for ([sign operators])
        
      ;Define the current equation
      (define currentEq (createEquation oAnswer sign currentNumber));
      ;Calculate the current Equation
      (define currentAnswer (eval (read (open-input-string currentEq)) ns))

      ;Evaluate if answer is equal to the answer number
      (if ( = currentAnswer target-number)
          (begin
            ;(display (string-append  " success" "\n" ))
            (display (~a  currentEq " = " currentAnswer "\n") )
            (set! permCount (+ permCount 1))
            (set! correctCount (+ correctCount 1)))
            
          (begin
            ;(display (string-append (~v currentEq) " = "(~v currentAnswer) "\n") )
            (set! permCount (+ permCount 1))
            )) ;TODO return)
        
      ;Evaluate if the answer is a negative number or a fraction
      (if (exact-positive-integer? currentAnswer )
          (begin
            ;Call the function again 
            (calculate selected-numbers currentEq))
            
          (display ""))
      )
    )
  )

;Method for calculating the current equation
(define (createEquation oAnswer sign currentNumber)
  (~a "( " sign " "   oAnswer  " "  (~v currentNumber) " )")
  )

;Call the main search method
(menu)
(display (~a "\nTotal Permutations: " permCount "\n"))
(display (~a "Correct Permutations: " correctCount ))

;REFERENCES
;================================
;[1]: for (https://docs.racket-lang.org/guide/for.html).
;[2]: random (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._remove%29%29).
;[3]: list-ref (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29).
;[4]: remove (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._remove%29%29).
;[5]: set! (http://docs.racket-lang.org/reference/set_.html).
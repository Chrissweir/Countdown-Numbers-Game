# Countdown Numbers Game #

**Student** : Christopher Weir- G00309429 <br>
**Module**: Theory of Algorithms <br>
**Lecturer:** Dr. Ian McLoughlin <br>

## Introduction ##
The aim of this project was to solve the Countdown Numbers Game problem using the Racket functional programming language . I have created a program that calculates the possible combinations of six numbers that evaluate in a Equation to a single answer number with two different algorithms: A Brute Force algorithm, and a Reverse Polish Notation algorithm.

## Brute Force Algorithm ##
I originally started with my own brute force algorithm which iterates over every possible permutation of the equation. I chose this approach as at the time as I did not know of any other way to attempt this problem. To get an idea of how to develop this algorithm I first had to research the calculation factors of the game. From reading the article at http://www.datagenetics.com/blog/august32014/index.html along with studying solutions to the problem that are written in other programming languages at https://www.reddit.com/r/dailyprogrammer/comments/452omr/20160210_challenge_253_intermediate_countdown/, I understood the problem better and I tried to implement some of the optimizations into my algorithm to improve the space and time complexity. When running the program on my laptop the calculation finishes in 1-2 minutes iterating over approximately 800 - 900 thousand permutations.  However, this figure varies from time to time.

I used nested loops to check each number and operator which then called the method again over each iteration. I did this based upon the branching factor of Tree Data Structures, eg. https://www.tutorialspoint.com/data_structures_algorithms/tree_data_structure.htm
The optimizations I included allowed for certain "branches" to be eliminated using some built in Racket functions which allowed the program to check other branches instead. These include branches that equal to zero, or negative.
```
 (if (exact-positive-integer? currentAnswer )
```
If the statement above evaluates true the function is called again with the updated list and the current equation that has been evaluated to a positive integer.
```
(findAnswers originalList currentEq))
```
The reason for adding the currentEq is so that we can nest the last equation inside the next one, creating a list of equations that represent actual racket equations.

```
( * ( - 50 8 ) 4 ) = 168
```
In this case the first equation ( - 50 8 ) has been evaluated, then sent to that calculate function again, with the originalList without the two numbers used in it. It is then evaluated along with the next number (4) then reaching 168.

When the calculate function is called initially in the permutations function, we have to take the initial number in the list and send it as currentEq to the method.
```
(for ([set sets])
    (define firstValue (car set))
    (set! set (remove firstValue set))
    (calculate set firstValue))
```

We can edit both the amount of numbers in the equation list and the answer number.
* Edit the list by changing the [i 6]  value to as many numbers as you need.
```
(define (select-numbers numbers)
  (for([i 6])
    (define random-number(list-ref numbers (random (length numbers))))
    (set! numbers(remove random-number numbers))
    (set! selected-numbers(cons random-number selected-numbers))))
```
* The target answer number can be changed near the top of the page.
```
(define target-answer 0)
```

## Sample Output ##
Sample output for a 6 number random list trying to evaluate to answer number of 6 :

```
Countdown Number Game Solver
============================
Numbers to choose from: '(1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 25 50 75 100)
Numbers selected: '(6 6 5 10 4 7)
Target Number: 622

Calculating please wait.....
==========================================================================

Total Permutations: 426128
Correct Permutations: 2
'("( + ( * ( - ( * ( + 5 6 ) 10 ) 7 ) 6 ) 4 ) = 622"
  "( + ( * ( - ( * ( + 6 5 ) 10 ) 7 ) 6 ) 4 ) = 622")
...

```

## Limitations: ##
* The program currently prints out duplicate answers to the solved equations.
* The time complexity is rather large but with extra tweaks to the algorithm it could be reduced, however I was unable do as such.

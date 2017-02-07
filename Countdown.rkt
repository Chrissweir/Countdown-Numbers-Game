#lang racket

(define (Calculate x (list)))


(define (generate min max)
  (if (< max min)
      0
    (cons min (generate (+ min 1) max))))

(generate 101 999)
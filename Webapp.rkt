#lang web-server/insta
(require racket/include)
(include "Countdown.rkt")
(struct post (title body))

(define (start request)
  (response/xexpr
   '(html
     (head (title "Countdown"))
     (body (h2 "Countdown Number Game")
           (button "Press"))
           (p 'nums))))

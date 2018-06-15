#lang racket

(define myfunc
  (lambda (z) (= z 0)))

(define greatfunc
  (lambda (z) (> z 0)))

(define lessfunc
  (lambda (z) (< z 0)))

(define myfilter
  (lambda (pred L)
    (cond
      ((null? L) L)
    ((pred (car L)) (cons (car L)(myfilter pred (cdr L))))
    (else (myfilter pred (cdr L)))
     )))

;(myfilter (lambda (x) (> x 0)) '(9 8 0 -3 -1 2 0 3 4))

;(myfilter (lambda (y) (< y 6)) '(2 6 3 9 -5 -8  -2 0 5))


;(myfilter myfunc '(4 2 0 4 6 3))

;(myfilter greatfunc '(5 3 -9 -4 0 2 -5))

;(myfilter lessfunc '(5 3 -9 -4 0 2 -5))
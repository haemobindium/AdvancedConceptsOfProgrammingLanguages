#lang racket
(define mysquare
  (lambda (x) (* x x)))

(define addThree
  (lambda (f) (+ f f f)))

(define mymap
  (lambda (func L)
    (cond
      ((null? L) '())
      (else
       (cons (func (car L))
             (mymap func(cdr L)))))
    )
  )

;(filter (lambda (x) (> x 0)) '(-1 2 0 3 4))

;(mymap mysquare '(1 2 3))

;(mymap (lambda (f) (/ f 2)) '(4 6 8 10))

;(mymap addThree '(5 6 7))

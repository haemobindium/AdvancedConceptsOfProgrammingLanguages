#lang scheme

(define myfactorial
  (lambda (n)
     (cond
       ((= n 1)1)
       (else (* n (myfactorial (- n 1))))
       )
    )
  )
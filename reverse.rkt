#lang racket
(require racket/date)
(define myreverse
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else
       ( append (myreverse (cdr lst))
                (cons (car lst) '())
                )
       )
      )
    )
  )
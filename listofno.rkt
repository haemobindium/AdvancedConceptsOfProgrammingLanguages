#lang racket
(require racket/date)
(define mylist-of-numbers?
 (lambda (lst)
   (or (null? lst)
       (and (number? (car lst))
            (mylist-of-numbers? (cdr lst))
            )
       )
   )
  )

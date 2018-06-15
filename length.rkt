#lang racket
(require racket/date)
(define mylength
  (lambda (lst)
    (cond
      ((null? lst) 0)
      (else
       (+ 1 (mylength (cdr lst)))
       )
      )
    )
  )

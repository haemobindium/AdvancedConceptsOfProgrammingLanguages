#lang racket
(require racket/date)
(define append
  (lambda (a lst)
    (cond
      ((null? a) lst)
      (else (cons a lst))
      )
    )
  )

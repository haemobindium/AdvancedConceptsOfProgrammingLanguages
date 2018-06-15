#lang racket
(define occurs-free?
  (lambda (var exp)
    (cases expression exp
      (varialbe-expression (id) (eqv? id var))
      (lambda-expression (id body)
                         (and(not (eqv? id var))
                             (occurs-free? var body)))
      (application-expression (E1 E2)
                              (or (occurs-free? var E1)
                                  (occurs-free? var E2)))
      )
    ))
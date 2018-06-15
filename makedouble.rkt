#lang racket
(require racket/date)
(define make-double (lambda (f)
                      (lambda (x) (f x x))))
(define double (make-double +))

(double 5)
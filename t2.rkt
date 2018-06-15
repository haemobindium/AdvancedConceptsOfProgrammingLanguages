#lang racket
(define factorial
  (lambda (x)
      (cond
        ((zero? x) 1)
        ((= x 1) 1)
        (else (* x (factorial(- x 1)))))))

(define pi 3.14)
(define two_pi (* 2 pi))

(define square (lambda (x) (* x x)))
(square 2)

(define flip
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (cons (car (cdr l))
                  (cons (car l)
                        (flip (cdr (cdr l)))))))))

(define reverse
  (lambda (r)
    (cond
      ((null? r) '())
      (else
       (append (reverse (cdr r))
               (cons (car r) '()))))))

(define append
  (lambda (x y)
    (cond
      ((null? x) y)
      (else
       (cons x y)))))

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
       (+ 1 (length (cdr l)))))))

(define (flatten l)
  (cond ((null? l) '())
        ((pair? l)(append (flatten (car l))
                          (flatten (cdr l))))
        (else (list l))))
(define atom
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define flatten1
  (lambda (f)
    (cond
      ((null? f) '())
      ((atom? (car f)) (cons (car f) (flatten1 (cdr f))))
      (else (append (flatten1 (car f)) (flatten1 (cdr f)))))))

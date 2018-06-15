#lang eopl
;;; begin -- abstract syntax definition


(define-datatype  program  program?
   (a-program (exp expression?)))

(define-datatype expression expression?
   (lit-exp (datum number?))
   (var-exp (id symbol?))
   (primapp-exp
     (prim  primitive?)
     (rands  (list-of expression?))) )

(define-datatype primitive primitive?
   (add-prim)
   (subtract-prim)
   (mult-prim)
   (incr-prim)
   (decr-prim))
;;; end -- abstract syntax definition

; program example: 2
(define program-test-1  
  (a-program (lit-exp 2)))

; program example: v
(define program-test-2  
  (a-program (var-exp 'v)))

; program example: +(v,2)
(define program-test-3
  (a-program (primapp-exp (add-prim) 
					(list (var-exp 'v) (lit-exp 2)))))
; program example: add1 (+(v,3))
(define program-test-4 
	(a-program
  		(primapp-exp
		  	(incr-prim)
		  	  	(list (primapp-exp 
		  	  			(add-prim)  
                                                (list (var-exp 'v)
                                                      (lit-exp 3)))))))

;


;interpreter




(require "environment.rkt");
;;we already define init-env

(define eval-rands
  (lambda  (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))



(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id)
               (apply-env env id) )
      (primapp-exp (prim rands)
                   (let((args(eval-rands rands env)))
                     (apply-primitive prim args))))))

(define apply-primitive
  (lambda (prim args)
    (cases primitive  prim
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      )
  ))

;eval-rands --
;map - to run a list with arguments: (map (lambda (x) (...))list)
;stp1: get arguments from list
;stp2: 
(define eval-rand
 (lambda (rand env)
(eval-expression rand env)))           


;(eval-program (a-program (var-exp 'x)))-- ans: 10 if a - no binding for a

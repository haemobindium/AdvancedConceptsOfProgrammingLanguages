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


;interpreter




(require "environment.rkt");
;;we already define init-env

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
      (primapp-exp (prim rands) '() ))))

;(eval-program (a-program (var-exp 'x)))-- ans: 10 if a - no binding for a


#lang eopl
(define-datatype
  expression expression?
  (variable-expression (identifier symbol?))
  (lambda-expression (identifier symbol?) (body expression?))
  (application-expression(operator expression?)(operand expression?))  
 )
;;define test case 1: (lambda (x) (f (f x)))
(define lambda-expr-testcase1
  (lambda-expression 'x (application-expression (variable-expression 'f)
                                                (application-expression
                                                 (variable-expression 'f)
                                                 (variable-expression 'x))))
  )
;;test case2: (lambda (y) (g y))
(define lambda-expr-tc2
  (lambda-expression 'y (application-expression (variable-expression 'g)
                                                (variable-expression 'y))))
;;test case3: (variable (a))
(define variable-expr-tc3
  (variable-expression 'a))
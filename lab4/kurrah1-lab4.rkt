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

(define parse-expression
  (lambda (expr)
    (cond
      ((symbol? expr) (variable-expression expr))
      ((pair? expr)
       (cond
         ((eqv? (car expr) 'lambda)
          (lambda-expression (caadr expr)(parse-expression (caddr expr))))
         (else (application-expression (parse-expression (car expr))
                                       (parse-expression (cadr expr)) ))
         )
       )
      (else (eopl:error 'parse-expression "Invalid concrete syntax ~s" expr))
    )
  )
  )

;;test cases 1 & 2
;(parse-expression '(lambda (x) (x y)))

;Unparsing

(define unparse-expression
  (lambda (expr)
    (cases expression expr
      (variable-expression (identifier) identifier)
      (lambda-expression (identifier body)
                         (list 'lambda (list identifier) (unparse-expression body))
      )
      (application-expression (operator operand) (list (unparse-expression operator)
                                                       (unparse-expression operand)))
      )
  )
)

;(variable-expression 'a)---- (parse-expression 'a) : same
;(unparse-expression (variable-expression 'a))---------- (unparse-expression (parse-expression 'a)): same

;tc1
;(define expr-test-1
 ;   (lambda-expression 'x (application-expression
  ;                         (variable-expression 'x)
   ;                        (variable-expression 'y))))
;(unparse-expression expr-test-1)
; ans(lambda (x) (x y))

(define occurs-free?
  (lambda (var exp)
    (cases expression exp
      (variable-expression (id) (eqv? id var))
      (lambda-expression (id body)
                         (and(not (eqv? id var))
                             (occurs-free? var body)))
      (application-expression (E1 E2)
                              (or (occurs-free? var E1)
                                  (occurs-free? var E2)))
      )
    ))

;testcase1: ff(x)
(define lambda-exp-test
  (lambda-expression 'x
                     (application-expression
                      (variable-expression 'f)
                      (application-expression
                       (variable-expression 'f) (variable-expression 'x)))))

(define lambda-exp-test1
  (lambda-expression 'x
                     (application-expression
                      (variable-expression 'f)
                      (application-expression
                       (variable-expression 'f)
                       (application-expression  (variable-expression 'x)
                       (variable-expression 'y))))))

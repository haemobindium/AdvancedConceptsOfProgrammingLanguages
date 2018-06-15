#lang eopl


;; defining an expression for datatype

(define-datatype
  expression expression?
  (variable-expression (identifier symbol?))
  (number-expression (literal_tag number?))
  (lambda-expression (identifiers (list-of symbol?)) (body expression?))
  (application-expression(operator expression?)(operands (list-of expression?)))  
 )

;; defining expression for parsing

(define parse-expression
  (lambda (expr)
    (cond
      ((symbol? expr) (variable-expression expr))
      ((number? expr) (number-expression expr))
      ((null? expr) '())
      ((pair? expr)
       (cond
         ((eqv? (car expr) 'lambda)
          (lambda-expression (cadr expr)(parse-expression (caddr expr))))
         (else (application-expression (parse-expression (car expr))
                                       (map parse-expression (cdr expr))))
         )
       )
      (else (eopl:error 'parse-expression "Invalid concrete syntax ~s" expr))
    )
  )
  )


;testcase 1

;(parse-expression '(a b c d (1 2 4 5)))

;tc2

;(parse-expression '())

;;tc3
;(parse-expression '(3 5 (s 4 y)(k l & @ !)))

;;tc4
;(parse-expression '( hello (world)))

;;tc5
;(parse-expression '(& * $ @ _ + !))


;Unparsing

(define unparse-expression
  (lambda (expr)
    (cases expression expr
      (variable-expression (identifier) identifier)
      (number-expression (identifier) identifier)
      (lambda-expression (identifier body)
                         (list 'lambda identifier (unparse-expression body))
      )
      (application-expression (operator operand) (map unparse-expression
                                                       (cons operator operand)))
      )
  )
)

;testcase 1

;(unparse-expression (parse-expression '(a b c d (1 2 4 5))))

;tc2

;(unparse-expression (parse-expression '(3 5 (s 4 y)(k l & @ !))))

;;tc3
;(unparse-expression (parse-expression '( hello (world))))

;;tc4
;(unparse-expression (parse-expression '(& * $ @ _ + !)))

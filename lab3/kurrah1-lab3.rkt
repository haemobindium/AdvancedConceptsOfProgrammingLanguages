#lang eopl
(define-datatype
  bintree bintree?
  (leaf-node
   (datum number?)
   )
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)
   )
  )
;(interior-node 'biz (interior-node 'foo(leaf-node 2)(leaf-node 3)) (interior-node 'bar(leaf-node 4)(leaf-node 5)) )
(define mytree1 (interior-node 'biz (interior-node 'foo(leaf-node 2)(leaf-node 3))
                               (interior-node 'bar(leaf-node 4)(leaf-node 5)))
 )

;leaf-sum tc1
(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
                     (+ (leaf-sum left)(leaf-sum right))
                     )
      )
    )
 )
;tc1
(define new-tree (interior-node 'x
                         (leaf-node 6)(interior-node 'y
                             (leaf-node  4)(leaf-node 9))))

;tc2
(define tree (interior-node 'x (leaf-node 6)(leaf-node 9)))

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




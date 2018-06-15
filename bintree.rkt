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
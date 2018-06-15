
#lang eopl

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(require "lexical-grammar-v1.rkt")


(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)
   )
  )

(define apply-procval
  (lambda (proc args)
  (cases procval proc
  (closure (ids body env)
           (eval-expression body
                            (extend-env ids args env))))))


;;;;;;;;;;;;;;;; the core interpreter ;;;;;;;;;;;;;;;;

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
        (eval-expression body (init-env))))))

(define eval-expression 
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-expr false-expr)
              (if (true? (eval-expression test-exp env))
                         (eval-expression true-expr env)
                         (eval-expression false-expr env)
                         )
              )
      (proc-exp (ids body) (closure ids body env))
      
      (app-exp (rator rands)
              (let ((proc (eval-expression rator env))
                    (args (eval-rands rands env)))
                (if (procval? proc)
                    (apply-procval proc args)
                    (eopl:error 'eval-expression
                                "Attempt to apply non-porcegure ~s" proc))))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                    (extend-env ids args env))))
      
      (else (eopl:error 'eval-expression "Not here:~s" exp))
      )))

(define true?
  (lambda (x)
    (not (zero? x))
    )
  )

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
)))


;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(require "environment.rkt")

(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env ))))

(define my-interpreter
  (lambda (program)
    (eval-program (parse program))
    ))

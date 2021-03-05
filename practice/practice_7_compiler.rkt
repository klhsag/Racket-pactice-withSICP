#lang racket

(define my-eval-table
  (list '[(self-emul? exp) exp]
        '[(var? exp) (lookup-var exp env)]
        '[(assign? exp) (eval-assign exp env)]
        '[(def? exp) (eval-def exp env)]
        '[(lambda? exp) (eval-lambda exp env)]
        '[(application? exp)
         (my-apply (my-eval (operator exp) env)
                   (list-of-values (operands exp) env))]))

(define (self-emul? exp)
  (or (number? exp)
      (boolean? exp)
      (char? exp)
      (string? exp)))
  

(define (my-eval-exec table exp env)
  (if (empty? table)
      (error "unknown expression.")
      (begin (displayln (car (car table)))
             (displayln exp)
             (displayln (self-emul? exp))
             (let ([m (caar table)])
             (displayln (eval (caar table))))            ;sad, eval只能看全局作用域。（因为不这么做就不能是一个编译期确定的函数）
      (if (eval (car (car table)))
          (eval (cdr (car table)))
          (my-eval-exec (cdr table) exp env)))))


#|
(define (my-eval exp env)
  (cond [(self-emul? exp) exp]
        [(var? exp) (lookup-var exp env)]
        [(assign? exp) (eval-assign exp env)]
        [(def? exp) (eval-def exp env)]
        [(lambda? exp) (eval-lambda exp env)]
        [(application? exp)
         (my-apply (my-eval (operator exp) env)
                   (list-of-values (operands exp) env))]
        [else (error "unknown expression.")]))
|#
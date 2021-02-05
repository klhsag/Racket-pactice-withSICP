#lang racket

(define (id x) x)

(define (prime? x)
  (define (prime?-iter x i)
    (if (< x (sqr i))
        #t
        (if (= (remainder x i) 0)
            #f
            (prime?-iter x (+ i 1)))))
  ;(if (= x 1)
  ;   #f
  (prime?-iter x 2))

(define (%? op r)
  (define (%?-unit r x y)
    (modulo (op x y) r))
  (define (iter list res)
    (if (empty? list)
        res
        (iter (cdr list)
              (%?-unit r
                       (car list)
                       res))))
  (lambda params
    (iter params (op))))

(define my+ (%? + 10))        ; 定义my+为模10加法
(define my* (%? * 10))        ; 定义my*为模10乘法


(define (accumulate op)
  (define (iter f a next b res)
    (if (= a b)
        (op res a)
        (iter f
              (next a)
              next
              b
              (op res a))))
   (lambda (f a next b)
     (iter f a next b (op))))

(define (my-filter f list)
  (if (empty? list)
      '()
      (let ([x (car list)]
            [xs (cdr list)])
        (if (f x)
            (cons x (my-filter f xs))
            (my-filter f xs)))))

(define (filtered-accumulate op filter)
  (define (iter f a next b res)
    (define (filtered-fresh x)
      (if (filter x)
          (op res x)
          res))
    (if (= a b)
        (filtered-fresh a)
        (iter f
              (next a)
              next
              b
              (filtered-fresh a))))
   (lambda (f a next b)
     (iter f a next b (op))))

(define (accu-last-apply n)
  (define (filter i)
    (= 1 (gcd i n)))
  (define func
    (filtered-accumulate * filter))
  (func id
        1
        (lambda (x) (+ x 1))
        (- n 1)))

(define (my-nearly-eq? x y)
  (if (= y 0)
      (= x 0)
      (< (abs (- (/ x y) 1.0)) 0.00000001)))

(define (make-prox iter x)
  (define enough? my-nearly-eq?)
  (if (< (abs x) 0.00000001)
      0.0
      (let ([x* (iter x)])
        (if (enough? x* x)
            x*
            (make-prox iter x*)))))

(define (derive f)         
  (lambda (x)
    (define (derive* dx)
      (/ (- (f (+ x dx))
            (f (- x dx)))
         (* 2.0 dx)))
    (define eps 0.00000001)
    (derive* eps)))
             
    
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derive g) x)))))

(define (newton-method g x0)
  (define f (newton-transform g))
  (make-prox f x0))
    

(newton-method (lambda (x) (- (sqr x) 361)) 1.0)

(define (my-compose f g)
  (lambda params
    (f (apply g params))))

(define (repeated f n)
  (define (f-iter n g)
    (if (= n 0)
        g
        (f-iter (- n 1) (compose f g))))
  (f-iter n id))

(define (simple-repeated f n)
  (if (= 0 n)
      id
      (compose f
               (simple-repeated f (- n 1)))))

(define (iter--improve utility-eval make-improve)
  (lambda (x)
    (if (utility-eval x)
        x
        ((iter--improve utility-eval make-improve) (make-improve x)))))

(define (my-new-sqrt x)
  (let ([res ((iter--improve (lambda (y)
                               (my-nearly-eq? x (sqr y)))
                             (lambda (y)
                               ((y . + . (x . / . y)) . / . 2)))
              x)])
    (if (integer? res)
        res
        (+ 0.0 res)))) ; 解除可能的分数表现形式  
      
#lang racket
(define (abs x)
  (if (< x 0) (- x)
      x))
(define (max3 x y z)
  (if (and (> x y) (> x z))
      x
      (if (> y z)
          y
          z)))

(define (max3to2 x y z)
  (if (and (< x y) (< x z))
      (list y z)
      (if (< y z)
          (list x z)
          (list x y))))

(define (sum-square x y)
  (+ (sqr x) (sqr y)))

(define (ans-for-1-3 x y z)
  (apply sum-square (max3to2 x y z)))

(let* ([x 1]
       [y (* x 2)])
  (+ x y))

(define (my-sqrt x)
   (define (my-sqrt-int-iter x y)
     (if (= y (sqr x))
         x
         (if (> y (sqr x))
             (my-sqrt-int-iter (* x 2) y)
             (if (<= y (sqr (- x 1)))
                 (my-sqrt-int-iter (- x 1) y)
                 (my-sqrt-nt-iter x y)))))
   (define (my-sqrt-nt-iter x y)
     (let ([nx (/ (+ x (/ y x)) 2.0)])
       (if (my-nearly-eq? x nx)
           x
           (my-sqrt-nt-iter nx y))))
   (cond ((>= x 0) (my-sqrt-int-iter 1 x))))

(define (my-nearly-eq? x y)
  (if (= y 0)
      (= x 0)
      (< (abs (- (/ x y) 1.0)) 0.00000001)))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* y 2))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

    
(define (fib x)
  (define (f k x y)
    (if (= k 0)
        x
        (f (- k 1) y (+ x y))))
  (cond [(< x 0) '()]
        [(= x 0) 0]
        [(= x 1) 1]
        [else (f x 0 1)]))

(define (my-expt x y)
  (cond [(= y 0) 1]
        [else (* (my-expt x (- y 1))
                 x)]))

(define (my-expt-2 x y)
  (define (expt-iter x y r)
    (if (= y 0)
        r
        (expt-iter x (- y 1) (* x r))))
  (expt-iter x y 1))

(define (my-expt-3 x y)
  (if (= y 0)
      1
      (if (even? y)
          (my-expt-3 (* x x) (/ y 2))
          (* x
             (my-expt-3 x (- y 1))))))



(define (my-expt-4 x y)
  (define (expt-iter x y r)
    (if (= y 0)
        r
        (if (even? y)
            (expt-iter (* x x) (/ y 2) r)
            (expt-iter x (- y 1) (* x r)))))
  (expt-iter x y 1))

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

(define (+r r)                          ; 返回模r加法
  (define (+r-unit r x y)
    (modulo (+ x y) r))
  (define (+r-iter list sum)
    (if (empty? list)
        sum
        (+r-iter (cdr list)
                 (+r-unit r
                          (car list)
                          sum))))
  (lambda params
    (+r-iter params 0)))


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
              

(define (my-foldr f-x-r-r r list)
  (if (empty? list)
      r
      (f-x-r-r (car list)
               (my-foldr f-x-r-r r (cdr list)))))

(define (my-foldl f-r-x-r r list)
  (if (empty? list)
      r
      (my-foldl f-r-x-r
                (f-r-x-r r (car list))
                (cdr list))))

(define (my-new-foldl f r list)
  (define foldr my-foldr)
  (define (id x) x)
  (define (step x g)
    (lambda (z)
      (g (f z x))))
  ((foldr step id list) r))


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


(syntax-case #'(+ 1 2 3) ()
   [(op n1 n2) #'(- n1 n2)]
   [(op a b c) #'(* a b c)])

(define x '(1 2))
(define y '(2 7))

`(,@x ,@y)

(define (dot-product v w)
  (foldl + 0 (map * v w)))

(define (flat list)                   ; 错误实现
  (foldl append '() list))

(define (flatmap proc seq)
  (flat (map proc seq)))

(define (prime-list? list)
    (prime? (foldl + 0 list)))

(let* ([n 8]
       [list (flatmap (lambda (i)
                        (map (lambda (j)
                               (list i j))
                             (stream->list (in-range 1 i))))
                      (stream->list (in-range 1 n)))])
  (filter prime-list? list))

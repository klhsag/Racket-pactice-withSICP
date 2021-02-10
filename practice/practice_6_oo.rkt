#lang racket

(define make-balance
  (lambda params
    (define balance 0)
    (hash 'get-balance
          (lambda ()
            balance)
          'set-balance
          (lambda (new-balance)
            (set! balance new-balance)))))

#|    garbage! 纯函数式写不出循环链表
(define make-circle
  (lambda (depth)
    (define circle `(,circle))
    (define (pack-circle! tag)
      (set! circle
            (cons tag circle)))
    (for ([i (in-range depth)])
      (pack-circle! i))
    circle))
|#

(define (make-node tag)
  (cons tag (list)))

(define (add-side node1 node2)    ; bad, 依然不能循环引用
  (cons (car node1)
        (cons node2 (cdr node1))))

#|             Y组合子是对正则序来说的，应用序下发散，程序死循环
(define (my-y-c f)
  ((lambda (x)
     (f (x x)))
   (lambda (x)
     (f (x x)))))
|#

(define Z
  (lambda (F)
    ((lambda (self)
       (F (lambda (x)((self self) x))))
     (lambda (self)
       (F (lambda (x)((self self) x)))))))


(define my-f
    (Z (lambda (f)
         (lambda (n)
           (if (= 0 n)
               1
               (* n (f (- n 1))))))))

(define my-call
  (Z (lambda (f)
       (lambda (x) (display x)))))

   

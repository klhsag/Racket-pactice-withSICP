#lang racket

(define (id x) x)

(define/contract (make-rational n [d 1])        ; 得到 n/d 或者 n （默认分母取1）
  (case-> (integer? integer?  . -> . (cons/c integer? integer?))
          (integer?  . -> . (cons/c integer? integer?)))             ; 合约-限制类型
  (if (< d 0)
      (cons (- n) (- 1.0))
      (cons n d)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (rational-reduce x)
  (let* ([n (numer x)]
         [d (denom x)]
         [gcd* (gcd n d)])
    (make-rational (/ n gcd*)
                   (/ d gcd*))))

(define (+r x y)
  (let* ([n1 (numer x)][d1 (denom x)][n2 (numer y)][d2 (denom y)])
    (rational-reduce (make-rational (+ (* n1 d2) (* n2 d1))
                                    (* d1 d2)))))
(define (-r x y)
  (let* ([n1 (numer x)][d1 (denom x)][n2 (numer y)][d2 (denom y)])
    (rational-reduce (make-rational (- (* n1 d2) (* n2 d1))
                                    (* d1 d2)))))
(define (*r x y)
  (let* ([n1 (numer x)][d1 (denom x)][n2 (numer y)][d2 (denom y)])
    (rational-reduce (make-rational (* n1 n2)
                                    (* d1 d2)))))
(define (/r x y)
  (let* ([n1 (numer x)][d1 (denom x)][n2 (numer y)][d2 (denom y)])
    (rational-reduce (make-rational (* n1 d2)
                                    (* d1 n2)))))

(define (rational-eq? x y)
  (let* ([n1 (numer x)][d1 (denom x)][n2 (numer y)][d2 (denom y)])
    (= (* n1 d2) (* n2 d1))))

(define my-cons (lambda (h t) (lambda (f) (f h t)) ))
(define my-car
     (lambda (c)
       (c (lambda (h t)
            h))))
(define my-cdr
     (lambda (c)
       (c (lambda (h t)
            t))))
(define (my-pair-display c)
  (define (show-cons c)
    (if (procedure? c)
        (begin
          (display "(")
          (show-cons (my-car c))
          (display " . ")
          (show-cons (my-cdr c))
          (display ")"))
        (display c)))
  (display "'")
  (show-cons c)
  (newline))

(define my-zero
  (lambda (f) id))

(define (my-succ n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define my-one
  (lambda (f)
    (lambda (x)
      (f x))))

(define (my-add a b)
  (lambda (f)
    (lambda (x)
      ((compose (a f) (b f)) x))))

(define my-nil 'nil)
(define my-nil?
  (lambda (x) (= x 'nil)))

(define my-list
  (lambda list
    (if (empty? list)
        my-nil
        (my-cons (car list)
                 (apply my-list (cdr list))))))


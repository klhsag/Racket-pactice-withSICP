#lang racket

(define (id x) x)

(define (flat src-list)          ; flat 实现须用foldr
  (foldr (lambda (v rest)
           (if (list? v)
               (append v rest)
               (append (list v) rest)))
         '()
         src-list))   ; <append> ::= (append lst ... [v])


(define var? symbol?)
(define (same-var? x y)
  (and (var? x)
       (var? y)
       (eq? x y)))
(define (=d x y)
  (and (number? x)
       (number? y)
       (= x y)))
(define (+d x y)
  (cond [(and (number? x) (number? y))
         (+ x y)]
        [(=d x 0) y]
        [(=d y 0) x]
        [else (list '+ x y)]))
(define (*d x y)
  (cond [(and (number? x) (number? y))
         (* x y)]
        [(=d x 1) y]
        [(=d y 1) x]
        [else (list '* x y)]))
(define (+d? lst)
  (and (pair? lst)
       (eq? '+ (car lst))))
(define (*d? lst)
  (and (pair? lst)
       (eq? '* (car lst))))
(define (d-ele-1 lst)
  (cadr lst))
(define (d-ele-2 lst)
  (caddr lst))

(define (derive expr x)
  (cond [(number? expr) 0]
        [(var? expr)
         (if (same-var? x expr) 1 0)]
        [(+d? expr)
         (+d (derive (d-ele-1 expr) x)
             (derive (d-ele-2 expr) x))]
        [(*d? expr)
         (+d (*d (d-ele-1 expr)
                 (derive (d-ele-2 expr) x))
             (*d (derive (d-ele-1 expr) x)
                 (d-ele-2 expr)))]
        [else (error "not derived")]))
  



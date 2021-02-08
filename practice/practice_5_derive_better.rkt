#lang racket

(define (id x) x)

(define (flat src-list)          ; flat 实现须用foldr
  (foldr append '() src-list))   ; <append> ::= (append lst ... [v])


(define var? symbol?)
(define (same-var? x y)
  (and (var? x)
       (var? y)
       (eq? x y))) 
(define (+d? lst)
  (and (pair? lst)
       (eq? '+ (car lst))))
(define (*d? lst)
  (and (pair? lst)
       (eq? '* (car lst))))
(define (just-brackets? lst)
  (and (pair? lst)
       (empty? (cdr lst))))
(define (d-first lst)
  (cadr lst))
(define (d-rest lst)
  (cons (car lst) (cddr lst)))

(define +d
  (lambda params
    (let*-values ([(number-lst rest-lst) (partition number? params)]
                  [(constant) (apply + number-lst)])
      (if (empty? rest-lst)
          constant
          (if (= constant 0)
              (append '(+) rest-lst)
              (append `(+ ,constant) rest-lst))))))

(define *d
  (lambda params
    (let*-values ([(number-lst rest-lst) (partition number? params)]
                  [(constant) (apply * number-lst)])
      (if (empty? rest-lst)
          constant
          (cond [(= constant 0) 0]
                [(= constant 1) (append '(*) rest-lst)]
                [else (append `(* ,constant) rest-lst)])))))

(define (->simple expr)
  (define (flat-op op lst)                          ; e.g. (flatop '+ '(1 (+ 2 3)) ) => '(1 2 3)
    (if (empty? lst)
        lst
        (let ([v (car lst)]
              [vs (cdr lst)])
          (if (and (pair? v)
                   (eq? op (car v)))
              (flat-op op (append (cdr v) vs))
              (cons v (flat-op op vs))))))     
      
  (cond [(number? expr) expr]
        [(var? expr) expr]
        [(just-brackets? expr) (car expr)]
        [(and (or (+d? expr)
                  (*d? expr))
              (empty? (cddr expr)))
         (->simple (cdr expr))]
        [(+d? expr)
         (if (empty? (cddr expr))
             (->simple (cdr expr))
             (let ([new-expr
                    (let* ([src (map ->simple expr)]
                           [op (car src)]
                           [lst (cdr src)])
                      (cons op (flat-op op lst)))])
               (if (equal? expr new-expr)
                   expr
                   (->simple new-expr))))]
        [(*d? expr)
         (if (empty? (cddr expr))
             (->simple (cdr expr))
             (let ([new-expr
                    (let* ([src (map ->simple expr)]
                           [op (car src)]
                           [lst (cdr src)])
                      (cons op (flat-op op lst)))])
               (if (equal? expr new-expr)
                   expr
                   (->simple new-expr))))]
        [else expr]))


(define (derive expr x)
  (cond [(number? expr) 0]
        [(var? expr)
         (if (same-var? x expr) 1 0)]
        [(just-brackets? expr)
         (derive (car expr) x)]
        [(and (pair? expr)
              (pair? (cdr expr))
              (empty? (cddr expr)))
         (derive (cdr expr) x)]
        [(+d? expr)
         (+d (derive (d-first expr) x)
             (derive (d-rest expr) x))]
        [(*d? expr)
         (+d (*d (d-first expr)
                 (derive (d-rest expr) x))
             (*d (derive (d-first expr) x)
                 (d-rest expr)))]
        [else (displayln expr)]))
  



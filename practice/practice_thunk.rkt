#lang racket

(define (x) (cons (lambda () 1) x))

(define y
  (stream-cons 1 (lambda () y)))

(define z
  (stream 1 (lambda () y)))
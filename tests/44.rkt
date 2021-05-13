#lang racket
(begin (define (f x) x)
       (+ (f 3) ((lambda (x) (f x)) 4)))
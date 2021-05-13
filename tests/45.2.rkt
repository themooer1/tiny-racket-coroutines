#lang racket ; Expected output: 7
(begin (define (f x) x)
       (define (g x) (f (lambda (y z) x)))
       ((g 4) 1000))
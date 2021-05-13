#lang racket ; Expected output: 'err
(begin (define (f x) x)
       (define (g x) (f (lambda (y z) x)))
       (+ (f 3) (g 4)))
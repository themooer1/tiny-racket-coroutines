#lang racket
(begin (define (f x) x)
       (lambda (x) (f x)))
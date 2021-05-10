#lang racket
(begin (define (f x) x)
       ((lambda (x) x) 4))
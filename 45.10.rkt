#lang racket ; Expected output: 7
(begin 
       (define (g x) (lambda (y z) x))
       (g 4))

#lang racket ; Expected output: 7
(begin 
       (define (g x) (lambda (x) x))
       ((g 4) 9))
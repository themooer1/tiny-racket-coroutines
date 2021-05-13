#lang racket ; Expected output: 'err
(begin 
       (define (g) (lambda (y) 5))
       ((g 4))) ; Should err (wrong arity)
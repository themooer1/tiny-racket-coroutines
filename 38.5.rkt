#lang racket ; Expected output: 7
(begin 
       (define (x) 4)
       (define (y) 63)

       (+
              ((lambda () (x)))
              ((lambda () (x)))
              ))

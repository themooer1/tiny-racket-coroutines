#lang racket ; Expected output: 7
(begin 
       (define (x) 4)
       (define (y) 63)

       (begin
              (gather 
                     (lambda () (x))
                     ; (lambda () (x))
                     ; (lambda () (x))
                     ; (lambda () (c 4))
                     ; (lambda () (d 4))
                     ; (lambda () (e 4))
                     ; (lambda () (e 4))
                     ; (lambda () (e 4))
                     ; (lambda () (l 4))
                     )
              (+ 1 41)))

#lang racket ; Expected output: 7
(begin 
       (define (g x) 
              (if (zero? x) 
                     65 
                     (begin
                            (begin
                                   (write-byte 65)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (g (- x 1))))))
       (define (h x) 
              (if (zero? x) 
                     66 
                     (begin
                            (begin
                                   (write-byte 66)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (h (- x 1))))))

       (begin
              (gather 
                     (lambda () (
                         (gather 
                            (lambda () (g 5)))))
                     (lambda () (
                         (gather 
                            (lambda () (h 5))))))
              (+ 1 41)))
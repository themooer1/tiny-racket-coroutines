#lang racket ; Expected output: Alternating A's and B's.  4 each.
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
                     (lambda () (g 4))
                     (lambda () (h 4)))
              (+ 1 41)))

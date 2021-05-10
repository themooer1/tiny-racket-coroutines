#lang racket ; Expected output: 7
(begin 
       (define (g x) 
              (if (zero? x) 
                     64 
                     (begin
                            (begin
                                   (write-byte 65)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (g (- x 1))))))

       (g 4))

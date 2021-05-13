#lang racket ; Expected output: A\nA\nA\nA\n
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

       (gather (lambda () (g 4))))

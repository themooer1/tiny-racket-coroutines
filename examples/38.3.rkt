#lang racket ; Expected output: 7
(begin 
       (define (a x) 
              (if (zero? x) 
                     65 
                     (begin
                            (begin
                                   (write-byte 65)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (a (- x 1))))))
       (define (b x) 
              (if (zero? x) 
                     66 
                     (begin
                            (begin
                                   (write-byte 66)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (b (- x 1))))))
       (define (c x) 
              (if (zero? x) 
                     67 
                     (begin
                            (begin
                                   (write-byte 67)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (c (- x 1))))))
       (define (d x) 
              (if (zero? x) 
                     68 
                     (begin
                            (begin
                                   (write-byte 68)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (d (- x 1))))))
       (define (e x) 
              (if (zero? x) 
                     69 
                     (begin
                            (begin
                                   (write-byte 69)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (e (- x 1))))))
       (define (f x) 
              (if (zero? x) 
                     70 
                     (begin
                            (begin
                                   (write-byte 70)
                                   (write-byte 10))
                            (begin
                                   (yield)
                                   (f (- x 1))))))




       (begin
              (gather 
                     (lambda () (c 4))
                     (lambda () (c 4))
                     ; (lambda () (c 4))
                     ; (lambda () (d 4))
                     ; (lambda () (e 4))
                     ; (lambda () (e 4))
                     ; (lambda () (e 4))
                     ; (lambda () (l 4))
                     )
              (+ 1 41)))

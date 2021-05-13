#lang racket ; Expected output: 4 each from [ABCDEF] interlaced
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
       (define (l)
              (a 4))

       (define (m)
              (b 4))

       (define (n)
              (c 4))

       (define (o)
              (d 4))

       (define (p)
              (e 4))


       (begin
              (gather 
                     l
                     m
                     n
                     o
                     p
                     l
                     l
                     ; (lambda () (l 4))
                     )
              (+ 1 41)))

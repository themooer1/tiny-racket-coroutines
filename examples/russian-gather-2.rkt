#lang racket ; A gather in a gather
(begin 
       ; (define (g x) 3)
       (define (cr1) 4)
       (define (cr2) 5)
       (gather 
              (lambda ()
                     (begin (gather cr1 cr2 cr1)
                            ; ^^ Blocked here until all children finish
                            (begin 
                                   (write-byte 65)
                                   (write-byte 10))))))
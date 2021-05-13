#lang racket ; Expected output: 'err
(begin 
       (define (g x y) (lambda (z) (+ x 1)))
       ((g 4 5))) ; Should err (wrong arity)
       ; ^^ will segfault misaligned stack if arity check jumps straight to 'raise-err without realigning stack
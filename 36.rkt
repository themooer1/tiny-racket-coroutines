#lang racket ; Expected output: 42
(begin (gather (lambda () 42) (lambda () 43)))

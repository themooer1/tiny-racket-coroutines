#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed program?
(define (prog? x)
  (match x
    [(list 'begin ds ... e)
     (and (andmap defn? ds)
          (expr? e))]
    [_ (expr? x)]))

;; Any -> Boolean
(define (defn? x)
  (match x
    [`(define ,(list f xs ...) ,e)
     (and (variable? f)
          (andmap variable? xs)
          (unique? (cons f xs))
          (expr? e))]
    [`(define ,(list* f xs ... r) ,e)
     (and (variable? f)
          (variable? r)
          (andmap variable? xs)
          (unique? (cons r (cons f xs)))
          (expr? e))]
    [_ #f]))

;; (Listof Any) -> Boolean
(define (unique? xs)
  (match xs
    ['() #t]
    [(cons x xs)
     (and (not (memq x xs))
          (unique? xs))]))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [''() #t]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(,(? prim1?) ,x) (expr? x)]
    [`(,(? prim2?) ,x ,y) (and (expr? x) (expr? y))]
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
    [(? variable? x) #t]
    [`(let ,x ,y)
     (and (bindings? x)
          (unique? (map first x))
          (expr? y))]
    [`(apply ,f ,x)
     (and (variable? f)
          (expr? x))]
    [`(,f . ,xs)
     (and (variable? f)
          (andmap expr? xs))]
    [_ #f]))

;; Program -> Boolean
;; Is p a closed program?
(define (closed? p)
  (match p
    [(list 'begin `(define (,fs . ,xss) ,es) ... e)
     (and (andmap
           (lambda (xs e)
             (match xs
               [(list xs ...)
                (closed-expr?/env e (append fs xs))]
               [(list* xs ... r)
                (closed-expr?/env e (append fs xs (list r)))]))
           xss es)
          (closed-expr?/env e fs))]
    [e (closed-expr?/env e '())]))

(define (closed-expr?/env e bvs)
  (match e
    [(? integer?) #t]
    [(? boolean?) #t]
    [(? char?) #t]
    [(? string?) #t]
    [(? symbol?) (and (memq e bvs) #t)]
    [''() #t]
    [`(if . ,es) (andmap (λ (e) (closed-expr?/env e bvs)) es)]
    [`(,(? prim1?) ,e) (closed-expr?/env e bvs)]
    [`(,(? prim2?) ,e0 ,e1)
     (and (closed-expr?/env e0 bvs)
          (closed-expr?/env e1 bvs))]
    [(list 'cond `(,e0s ,e1s) ... `(else ,en))
     (andmap (λ (e) (closed-expr?/env e bvs))
             (append e0s e1s (list en)))]
    [`(let ,(list `(,xs ,e0s) ...) ,e)
     (and (andmap (λ (e) (closed-expr?/env e bvs)) e0s)
          (closed-expr?/env e (append xs bvs)))]
    [`(apply ,f ,e)
     (and (memq f bvs)
          (closed-expr?/env e bvs))]
    [`(,f . ,es)
     (and (memq f bvs)
          (andmap (lambda (e) (closed-expr?/env e bvs)) es))]))

;; Any -> Boolean
;; Is x a unary primitive?
(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      car cdr length box? string? cons? empty?
                      box unbox string-length char? integer? boolean? zero?))))

;; Any -> Boolean
;; Is x a binary primitive?
(define (prim2? x)
  (and (symbol? x)
       (memq x '(+ cons string-ref make-string char=?
                   = <= < char=? boolean=? -))))

;; Any -> Boolean
;; Is x a well-formed list of bindings?
(define (bindings? x)
  (match x
    ['() #t]
    [`((,x ,y) . ,z)
     (and (variable? x)
          (expr? y)
          (bindings? z))]
    [_ #f]))

;; Any -> Boolean
(define (keyword? x)
  (and (symbol? x)
       (memq x '(cond else if let))))

;; Any -> Boolean
(define (variable? x)
  (and (symbol? x)
       (not (prim1? x))
       (not (prim2? x))
       (not (keyword? x))))

(module+ test
  (require rackunit)

  (check-true (expr? 1))
  (check-true (expr? #\c))
  (check-true (expr? #t))
  (check-true (expr? '(add1 1)))
  (check-true (expr? '(add1 #t)))
  (check-true (expr? '(let () 1)))
  (check-true (expr? '(let ((x 1)) 1)))
  (check-true (expr? '(let ((x 1)) x)))

  (check-false (expr? '(let ((x 1) (x 2)) x)))

  (check-false (expr? '(let ((x)) 0)))
  (check-false (expr? '(let 0 0)))
  (check-false (expr? '(let (x) 0)))
  (check-false (expr? '(let (()) 0)))
  (check-false (expr? '(let x 0)))
  (check-false (expr? '(let x)))
  (check-false (expr? '(let ((x 0))))))

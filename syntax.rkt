#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; Prog -> Boolean
(define (prog? p)
  (match p
   [(prog ds e)
     (and (expr? e)
          (andmap (match-lambda [(fundef n as e) (expr? e)]) ds))]))

;; Expr -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(int-e i)    #t]
    [(bool-e b)   #t]
    [(var-e v)    #t]
    [(char-e c)   #t]
    [(string-e s) #t]
    [(nil-e)      #t]
    [(if-e p t f)
     (and (expr? p)
          (expr? t)
          (expr? f))]
    [(prim-e p es) (and
                    (prim? p)
                    (andmap expr? es))]
    [(cond-e cs f)
       (and (andmap expr? (get-preds cs))
            (andmap expr? (get-bods cs))
            (expr? f))]
    [(let-e bs b)
     (and (bindings? bs)
          (not (check-duplicates (get-vars bs) eq?))
          (expr? b))]
    [(app-e f es)
     (and (symbol? f)
          (andmap expr? es))]
    [_ #f]))

;; Prog -> Boolean
;; Are all the expressions in `p` closed?
(define (closed? p)
  (match p
   [(prog ds e)
    (let ((fs (get-fun-names ds)))
     (and (closed?/env e '() (get-fun-names ds))
          (closed-funs? ds fs)))]))

(define (closed-funs? ds names)
  (define (closed-fun? f)
   (match f
    [(fundef n as e) (closed?/env e as names)]))
  (andmap closed-fun? ds))


(define (closed?/env e bvs fs)
  (match e
    [(int-e i)     #t]
    [(bool-e b)    #t]
    [(char-e c)    #t]
    [(string-e s)  #t]
    [(var-e v)     (and (memq v bvs) #t)]
    [(nil-e)       #t]
    [(if-e p t f)  (and
                     (closed?/env p bvs fs)
                     (closed?/env t bvs fs)
                     (closed?/env f bvs fs))]
    [(prim-e (? prim1? p) (list e))
      (closed?/env e bvs fs)]
    [(prim-e (? prim2? p) (list e0 e1))
      (and (closed?/env e0 bvs fs)
           (closed?/env e1 bvs fs))]
    [(cond-e cs f) (and
                     (closed-clauses?/env cs bvs fs)
                     (closed?/env f bvs fs))]
    [(let-e bs e)  (let ((vs (get-vars bs)))
                        (and
                          (closed-bindings?/env bs bvs fs)
                          (closed?/env e (append vs bvs) fs)))]
    [(app-e f es) (and (memq f fs) (andmap (lambda (e) (closed?/env e bvs fs)) es))]))

(define (closed-clauses?/env cs bvs fs)
  (match cs
    ['() #t]
    [(cons (clause e b) cs) (and
                              (closed?/env e bvs fs)
                              (closed?/env b bvs fs)
                              (closed-clauses?/env cs bvs fs))]))

(define (closed-bindings?/env bs bvs fs)
  (match bs
    ['() #t]
    [(cons (binding v def) bs) (and
                                 (closed?/env def bvs fs)
                                 (closed-bindings?/env bs bvs fs))]))

;; Any -> Boolean
;; Is x a well-formed list of bindings?
(define (bindings? xs)
  (match xs
    ['() #t]
    [(cons (binding v e) bs) (and
                               (expr? e)
                               (symbol? v)
                               (bindings? bs))]))

;; Any -> Boolean
(define (keyword? x)
  (and (symbol? x)
       (memq x '(cond else if))))

; SExpr -> Prog
(define (sexpr->prog s)
  (match s
    [(list 'begin defs ... e) (prog (map sexpr->fundef defs) (sexpr->expr e))]
    [e                        (prog '() (sexpr->expr e))]))

; SExpr -> FunDef
(define (sexpr->fundef def)
  (match def
    [`(define (,f . ,as) ,body) (fundef f as (sexpr->expr body))]))

; SExpr -> Expr
; Parse the s-expr into our Expr AST
; This should be a one-to-one mapping for now.
(define (sexpr->expr s)
  (match s
    [(? symbol? v)  (var-e v)]
    [(? integer? s) (int-e s)]
    [(? char? c)    (char-e c)]
    [(? string? s)  (string-e s)]
    [(? boolean? b) (bool-e b)]
    [''()           (nil-e)]
    [`(if ,p ,t ,f) (if-e (sexpr->expr p) (sexpr->expr t) (sexpr->expr f))]
    [`(cond ,@cs)
      ; collect the clauses in a pair where the car
      ; is the list of clauses the cdr is the 'else'
      (let ((p (clauses->expr '() cs))) 
        (cond-e (car p) (cdr p)))]
    [`(let ,bs ,b)          (let-e (map binding->expr bs) (sexpr->expr b))]
    [`(,(? prim1? p) ,e)
      (prim-e p (list (sexpr->expr e)))]
    [`(,(? prim2? p) ,e1 ,e2)
      (prim-e p (list (sexpr->expr e1) (sexpr->expr e2)))]
    [`(let ,bs)     (error "malformed let expression")]
    [`(apply ,f ,e)
      (apply-e f (sexpr->expr e))]
    [`(,f . ,as)
      (app-e f (map sexpr->expr as))]
    [`(let ,bs ...) (error "malformed let expression")]
    [_              (error "operation not supported")]))

(define (clauses->expr acc cs)
  (match cs
    [`((else ,f))
      (cons (reverse acc) (sexpr->expr f))]
    [(cons `(,e ,b) rest)
      (let ((c (clause (sexpr->expr e) (sexpr->expr b))))
           (clauses->expr (cons c acc) rest))]))

(define (binding->expr bs)
  (match bs
    [`(,(? symbol? v) ,e) (binding v (sexpr->expr e))]
    [_                    (error "bound name must be a symbol")]))


(module+ test
  (require rackunit)

  (check-true (expr?  (sexpr->expr 1)))
  (check-true (expr?  (sexpr->expr #\c)))
  (check-true (expr?  (sexpr->expr #t)))
  (check-true (expr?  (sexpr->expr '(add1 1))))
  (check-true (expr?  (sexpr->expr '(add1 #t))))
  (check-true (expr?  (sexpr->expr '(let () 1))))
  (check-true (expr?  (sexpr->expr '(let ((x 1)) 1))))
  (check-true (expr?  (sexpr->expr '(let ((x 1)) x))))

  (check-false (expr? (sexpr->expr '(let ((x 1) (x 2)) x))))

  (check-exn exn:fail? (lambda () (expr? (sexpr->expr '(let ((x)) 0)))))
  (check-exn exn:fail? (lambda () (expr? (sexpr->expr '(let 0 0)))))
  (check-exn exn:fail? (lambda () (expr? (sexpr->expr '(let (x) 0)))))
  (check-exn exn:fail? (lambda () (expr? (sexpr->expr '(let (()) 0)))))
  (check-exn exn:fail? (lambda () (expr? (sexpr->expr '(let x 0)))))
  (check-exn exn:fail? (lambda () (expr? (sexpr->expr '(let x)))))
  (check-exn exn:fail? (lambda () (expr? (sexpr->expr '(let ((x 0))))))))

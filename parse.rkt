#lang racket
(provide parse parse-e)
(require "ast.rkt")

;; S-Expr -> Prog
(define (parse s)
  (match s
    [(list 'begin (and ds (list 'define _ _)) ... e)
     (Prog (map parse-d ds) (parse-e e))]
    [e (Prog '() (parse-e e))]))

;; S-Expr -> Defn
(define (parse-d s)
  (match s
    [(list 'define (list (? symbol? f) (? symbol? xs) ...) e)
     (Defn f xs (parse-e e))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(list-rest 'gather ls)
     (Gather (map parse-e ls))]
    [(list 'letrec bs e1)
     (LetRec (parse-bindings bs) (parse-e e1))]
    [(list 'λ (? symbol-list? as) e1)      (Lam '() as (parse-e e1))]
    [(list 'lambda (? symbol-list? as) e1) (Lam '() as (parse-e e1))]
    [(cons f es)
     (App (parse-e f) (map parse-e es))]
    [_ (error "Parse error" s)]))

(define (parse-bindings bs)
  (match bs
    ['() '()]
    [(cons (list (? symbol? x) e1) rest)
           (cons (list x (parse-e e1)) (parse-bindings rest))]))


(define (symbol-list? xs)
  (match xs
    [(list (? symbol?) ...) xs]))

(define op0
  '(read-byte peek-byte void yield))
(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer box unbox empty? car cdr
         string? string-length
         procedure-arity))
(define op2
  '(+ - eq? cons string-ref make-string))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))

#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | String
;; | (Box Value)
;; | (Cons Value Value)

;; type Answer = Value | 'err

;; type REnv = (Listof (List Variable Value))

;; Prog -> Answer
(define (interp p)
  (match p
    [(prog ds e)
     (interp-env e '() ds)]
    [e (interp-env e '() '())]))

;; Expr REnv (Listof Defn) -> Answer
(define (interp-env e r ds)
  (match e
    [(? value? v) (get-val v)]
    [(var-e x)
     (lookup r x)]
    [(nil-e) '()]
    [(prim-e (? prim? p) es)
     (let ((as (interp-env* es r ds)))
       (interp-prim p as r ds))]    
    [(if-e e0 e1 e2)
     (match (interp-env e0 r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(let-e bs body)
     (match (interp-env* (get-defs bs) r ds)
       ['err 'err]
       [vs
        (interp-env body (append (zip (get-vars bs) vs) r) ds)])]
    [(cond-e cs el)
     (interp-cond-env cs el r ds)]
    [(apply-e f e)   (let ((vs (interp-env e r ds)))
                             (if (list? vs)
                                 (apply-fun f vs ds)
                                 'err))]
    [(app-e f es)    (apply-fun f (interp-env* es r ds) ds)]
    [_ 'err]))

;; Variable (Listof Value) (Listof Defn) -> Answer
(define (apply-fun f vs ds)
  (match (defns-lookup ds f)
    [(fundef f `(,xs ...) e)
     (if (= (length xs) (length vs))
         (interp-env e (zip xs vs) ds)
         'err)]
    [(fundef f `(,xs ... . ,r) e)
     (if (<= (length xs) (length vs))
         (interp-env e (zip/remainder xs vs r) ds)
         'err)]))


;; (Listof Defn) Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(fundef g _ _) (eq? f g)])
         ds))

;; (Listof Expr) REnv (Listof Defn) -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (cons v (interp-env* es r ds))])]))

;; (Listof (List Expr Expr)) Expr REnv (Listof Defn) -> Answer
(define (interp-cond-env cs en r ds)
  (match cs
    ['() (interp-env en r ds)]
    [(cons (clause eq ea) cs)
     (match (interp-env eq r ds)
       ['err 'err]
       [v
        (if v
            (interp-env ea r ds)
            (interp-cond-env cs en r ds))])]))

;; Any -> Boolean
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 zero? abs - char? boolean? integer? integer->char char->integer
                      string? box? empty? cons cons? box unbox car cdr string-length
                      make-string string-ref = < <= char=? boolean=? +))))

;; Any -> Boolean
(define (value? x)
  (or (int-e? x)
      (bool-e? x)
      (char-e? x)
      (string-e? x)))

;; Prim (Listof Answer) -> Answer
(define (interp-prim p as r ds)
  (match (cons p as)
    [(list p (? value?) ... 'err _ ...) 'err]
    [(list '- (? integer? i0)) (- i0)]
    [(list '- (? integer? i0) (? integer? i1)) (- i0 i1)]
    [(list 'abs (? integer? i0)) (abs i0)]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list 'char? v0) (char? v0)]
    [(list 'integer? v0) (integer? v0)]
    [(list 'boolean? v0) (boolean? v0)]
    [(list 'integer->char (? codepoint? i0)) (integer->char i0)]
    [(list 'char->integer (? char? c)) (char->integer c)]
    [(list '+ (? integer? i0) (? integer? i1)) (+ i0 i1)]
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'car (? cons? v0)) (car v0)]
    [(list 'cdr (? cons? v0)) (cdr v0)]
    [(list 'string? v0) (string? v0)]
    [(list 'box? v0) (box? v0)]
    [(list 'empty? v0) (empty? v0)]
    [(list 'cons? v0) (cons? v0)]
    [(list 'cons v0 v1) (cons v0 v1)]
    [(list 'box v0) (box v0)]
    [(list 'unbox (? box? v0)) (unbox v0)]
    [(list 'string-length (? string? v0)) (string-length v0)]
    [(list 'make-string (? natural? v0) (? char? v1)) (make-string v0 v1)]
    [(list 'string-ref (? string? v0) (? natural? v1))
     (if (< v1 (string-length v0))
         (string-ref v0 v1)
         'err)]
    [(list '= (? integer? v0) (? integer? v1)) (= v0 v1)]
    [(list '< (? integer? v0) (? integer? v1)) (< v0 v1)]
    [(list '<= (? integer? v0) (? integer? v1)) (<= v0 v1)]
    [(list 'char=? (? char? v0) (? char? v1)) (char=? v0 v1)]
    [(list 'boolean=? (? boolean? v0) (? boolean? v1)) (boolean=? v0 v1)]
    [_ 'err]))

;; REnv Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y v) env)
     (match (symbol=? x y)
       [#t v]
       [#f (lookup env x)])]))

;; REnv Variable Value -> Value
(define (ext r x v)
  (cons (list x v) r))

;; Any -> Boolean
(define (codepoint? x)
  (and (integer? x)
       (<= 0 x #x10FFFF)
       (not (<= #xD800 x #xDFFF))))

;; (Listof A) (Listof B) -> (Listof (List A B))
(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y) (zip xs ys))]))

;; like zip but ys can be longer and remainder is associated with r
(define (zip/remainder xs ys r)
  (match* (xs ys)
    [('() ys) (list (list r ys))]
    [((cons x xs) (cons y ys))
     (cons (list x y) (zip/remainder xs ys r))]))

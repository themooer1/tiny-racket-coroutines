#lang racket
(provide (all-defined-out))

;; type Prog = [FunDefs] Expr

;; type FunDef = Variable [Variable] Expr

;; type Expr =
;; | Integer
;; | Boolean
;; | Variable
;; | Char
;; | String
;; | Prim1 Expr
;; | Prim2 Expr Expr
;; | App Expr Expr
;; | If Expr Expr Expr
;; | Cond (Clauses list_ Expr
;; | Let (Binding list) Expr
;; | Nil

;; type Binding = Variable Expr

;; type Variable = Symbol (except 'add1 'sub1 'if, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The represenation of top-level programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct prog (ds e) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The represenation of a function definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A FunDef has a symbol for the function's name,
;; a list of symbols representing the names of the function's
;; arguments, and one expression that forms the body of the function.
(struct fundef (name args body) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The Expr data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Expr can be viewed as having 'kinds' of nodes.
;;
;; * The nodes that represnt an expression themselves
;;
;; * The nodes that are part of an expression, but no an expression themselves

;; The below are the former:

(struct int-e  (i)     #:transparent)
(struct bool-e (b)     #:transparent)
(struct var-e  (v)     #:transparent)
(struct prim-e (p es)  #:transparent)
(struct char-e (c)     #:transparent)
(struct string-e (s)   #:transparent)
(struct cond-e (cs el) #:transparent)
(struct app-e  (f es)  #:transparent)
(struct apply-e (f e)  #:transparent)
(struct if-e   (e t f) #:transparent)
(struct let-e  (bs b)  #:transparent)
(struct nil-e  ()      #:transparent)

;; The next two are the latter:

;; a clause now takes an _arbitrary_ expression and a body.
;; This is different than assignment 3! If you want to understand
;; why, look at the lecture notes for Dupe.
(struct clause  (e body) #:transparent)


;; A binding holds a symbol representing the bound variable and
;; Expr that represents the value that will be bound to that variable
(struct binding (v e) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (predicates)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Any -> Boolean
;; Is x a unary primitive?
(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                 car cdr length box?  string?  cons?  empty?
                 box unbox string-length
                 char? integer? boolean? zero?))))

;; Any -> Boolean
;; Is x a binary primitive?
(define (prim2? x)
  (and (symbol? x)
       (memq x '(+ cons string-ref make-string char=?
                 = <= < char=? boolean=? -))))

(define (prim? x)
  (or (prim1? x)
      (prim2? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (getters)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It will sometimes be useful to get the list of all the variables that are
;; introduced by a `let`
;; Expr -> Value
(define (get-val v)
  (match v
    [(int-e x) x]
    [(bool-e x) x]
    [(char-e x) x]
    [(string-e x) x]
    [(nil-e) '()]))

;; It will sometimes be useful to get the list of all the variables that are
;; introduced by a `let`
;; [Binding] -> [Symbol]
(define (get-vars bs)
  (match bs
    ['() '()]
    [(cons (binding v _) bs) (cons v (get-vars bs))]))

;; Get all of the _definitions_ from a list of bindings
;; [Binding] -> [Expr]
(define (get-defs bs)
  (match bs
    ['() '()]
    [(cons (binding _ def) bs) (cons def (get-defs bs))]))

;; Get all of the predicate expressions from a list of clauses
;; [Clause] -> [Expr]
(define (get-preds cs)
  (match cs
    ['() '()]
    [(cons (clause p _) cs) (cons p (get-preds cs))]))

;; Get all of the bodies expressions from a list of clauses
;; [Clause] -> [Expr]
(define (get-bods cs)
  (match cs
    ['() '()]
    [(cons (clause _ b) cs) (cons b (get-bods cs))]))

;; Get all of the function names
;; Prog -> [Symbol]
(define (get-fun-names p)
  (let ((f (match-lambda [(fundef n as e) n])))
    (match p
     [(prog ds e) (map f ds)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (printers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have switched to using `#:transparent` above, so this should only be
;; necessary if you're desperate when debugging :'(

;; Given a Program, construct an sexpr that has the same shape
(define (prog-debug p)
  (match p
    [(prog ds e) `(prog ,(map fundef-debug ds) ,(ast-debug e))]))

;; Given a FunDef, construct an sexpr that has the same shape
(define (fundef-debug def)
  (match def
    [(fundef name args body) `(fundef ,name ,args ,(ast-debug body))]))

;; Given an AST, construct an sexpr that has the same shape
(define (ast-debug a)
  (match a
    [(int-e i)     `(int-e ,i)]
    [(bool-e b)    `(bool-e ,b)]
    [(var-e v)     `(var-e ,v)]
    [(nil-e)       ''()]
    [(char-e b)    `(char-e ,b)]
    [(string-e s)  `(string-e ,s)]
    [(prim-e p es) `(prim-e ,p ,@(map ast-debug es))]
    [(app-e f es)  `(app-e ,f ,@(map ast-debug es))]
    [(apply-e f e) `(apply-e ,f ,@(ast-debug e))]
    [(if-e e t f)  `(if-e ,(ast-debug e)
                          ,(ast-debug t)
                          ,(ast-debug f))]
    [(let-e bs b)  `(let-e ,(binding-debug bs) ,(ast-debug b))]))

(define (binding-debug bnds)
  (match bnds
    ['() '()]
    [(cons (binding v e) bnds) `((,v ,(ast-debug e)) ,@(binding-debug bnds))]))

(define (clauses-debug cs)
  (match cs
    ['() '()]
    [(cons (clause e b) cs) `((,(ast-debug e) ,(ast-debug b)) ,@(clauses-debug cs))]))

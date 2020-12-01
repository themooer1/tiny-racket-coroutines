#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; This assignment should be completed individually.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I pledge on my honor that I have not given or received any
;; unauthorized assistance on this assignment.
;;
;; Name: TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An immediate is anything ending in #b0000
;; All other tags in mask #b111 are pointers

(define result-shift     3)
(define result-type-mask (sub1 (arithmetic-shift 1 result-shift)))
(define type-imm         #b000)
(define type-box         #b001)
(define type-pair        #b010)
(define type-string      #b011)

(define imm-shift        (+ 2 result-shift))
(define imm-type-mask    (sub1 (arithmetic-shift 1 imm-shift)))
(define imm-type-int     (arithmetic-shift #b00 result-shift))
(define imm-type-bool    (arithmetic-shift #b01 result-shift))
(define imm-type-char    (arithmetic-shift #b10 result-shift))
(define imm-type-empty   (arithmetic-shift #b11 result-shift))

(define imm-val-false    imm-type-bool)
(define imm-val-true     (bitwise-ior (arithmetic-shift 1 (add1 imm-shift)) imm-type-bool))

;; Allocate in 64-bit (8-byte) increments, so pointers
;; end in #b000 and we tag with #b001 for boxes, etc.

;; type CEnv = (Listof (Maybe Variable))
;; type Imm = Integer | Boolean | Char | ''()


;; For the first parts for the assignment, Formals is a synonym for (Listof Variable)
;; type Formals = (Listof Variable)

;; The last part, the definition is extended to:
;; type Formals =
;; | '()
;; | Variable
;; | (Cons Variable Formals)

;; Prog -> Asm
(define (compile p)
  (match p
    [(prog ds e)
     (let ((ds (compile-defines ds))
           (c0 (compile-entry e)))
       `(,@c0
         ,@ds))]))

;; Expr -> Asm
;; Compile e as the entry point
(define (compile-entry e)
  `(entry
    (push rbp)
    (mov rbp rsp)
    ,@(compile-e e '())
    (mov rsp rbp)
    (pop rbp)
    ret
    err
    (mov rsp rbp)
    (pop rbp)
    (jmp error)))

;; (Listof Variable) (Listof (Listof Variable)) (Listof Expr) -> Asm
(define (compile-defines defs)
  (append-map compile-define defs))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? imm? i)              (compile-imm i)]
    [(var-e v)               (compile-var v c)]
    [(string-e s)            (compile-string s)]
    [(cond-e (cons (clause p b) cs) f)
                             (compile-e (if-e p b (cond-e cs f)) c)]
    [(cond-e '() f)          (compile-e f c)]
    [(prim-e (? prim? p) es) (compile-prim p es c)]
    [(if-e p t f)            (compile-if p t f c)]
    [(let-e bs body)         (compile-let bs body c)]
    [(apply-e f e)           (compile-apply f e c)]
    [(app-e f es)            (compile-call f es c)]))

(define (compile-prim p es c)
  (match (cons p es)
    [`(box ,e0)               (compile-box e0 c)]
    [`(unbox ,e0)             (compile-unbox e0 c)]
    [`(cons ,e0 ,e1)          (compile-cons e0 e1 c)]
    [`(car ,e0)               (compile-car e0 c)]
    [`(cdr ,e0)               (compile-cdr e0 c)]
    [`(add1 ,e0)              (compile-add1 e0 c)]
    [`(sub1 ,e0)              (compile-sub1 e0 c)]
    [`(zero? ,e0)             (compile-zero? e0 c)]
    [`(+ ,e0 ,e1)             (compile-+ e0 e1 c)]
    [`(- ,e0 ,e1)             (compile-binary-- e0 e1 c)]
    [`(char=? ,e0 ,e1)        (compile-char=? e0 e1 c)]
    [`(boolean=? ,e0 ,e1)     (compile-boolean=? e0 e1 c)]
    [`(= ,e0 ,e1)             (compile-= e0 e1 c)]
    [`(< ,e0 ,e1)             (compile-< e0 e1 c)]
    [`(<= ,e0 ,e1)            (compile-<= e0 e1 c)]
    [`(string-length ,e0)     (compile-string-length e0 c)]
    [`(string-ref ,e0 ,e1)    (compile-string-ref e0 e1 c)]
    [`(make-string ,e0 ,e1)   (compile-make-string e0 e1 c)]
    [`(,(? type-pred? p) ,e0) (compile-type-pred p e0 c)]
    [`(char->integer ,e0)     (compile-char->integer e0 c)]
    [`(integer->char ,e0)     (compile-integer->char e0 c)]
    [`(abs ,e0)               (compile-abs e0 c)]
    [`(- ,e0)                 (compile-unary-- e0 c)]))

;; Variable Expr CEnv -> Asm
(define (compile-apply f e c)
  ;; TODO
  '())



;; Variable (Listof Expr) CEnv -> Asm
;; Statically know the function we're calling
(define (compile-call f es c)
  ;; TODO: update to communicate arity information
  (let ((cs (compile-es es (cons #f c)))
        (stack-size (* 8 (length c))))
    `(,@cs
      (sub rsp ,stack-size)
      (call ,(symbol->label f))
      (add rsp ,stack-size))))


;; (Listof Expr) CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c))
           (cs (compile-es es (cons #f c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@cs))]))

;; FunDef -> Asm
(define (compile-define def)
  (match def
    [(fundef name args body)
      (match args
        [(list xs ...)
         (let ((c0 (compile-e body (reverse xs))))
           `(,(symbol->label name)
             ,@c0
             ret))]
        [(list* xs ... r)
        ;; TODO: update to do arity checking
        '()])
             ]))




;; Any -> Boolean
(define (imm? x)
  (or (int-e? x)
      (bool-e? x)
      (char-e? x)
      (nil-e? x)))

;; Any -> Boolean
(define (type-pred? x)
  (memq x '(integer?
            char?
            empty?
            string?
            boolean?
            box?
            cons?)))

;; Imm -> Asm
(define (compile-imm i)
  `((mov rax ,(imm->bits i))))

;; Imm -> Integer
(define (imm->bits i)
  (match i
    [(? integer? i)  (arithmetic-shift i imm-shift)]
    [(? char? c) (+ (arithmetic-shift (char->integer c) imm-shift) imm-type-char)] 
    [(int-e i)  (arithmetic-shift i imm-shift)]
    [(char-e c) (+ (arithmetic-shift (char->integer c) imm-shift) imm-type-char)]
    [(bool-e b) (if b imm-val-true imm-val-false)]
    [nil-e      imm-type-empty]))

;; Expr Expr CEnv -> Asm
(define (compile-char=? e0 e1 c)
  (compile-type-equiv 'char? 'je e0 e1 c))
(define (compile-boolean=? e0 e1 c)
  (compile-type-equiv 'boolean? 'je e0 e1 c))
(define (compile-= e0 e1 c)
  (compile-type-equiv 'integer? 'je e0 e1 c))
(define (compile-< e0 e1 c)
  (compile-type-equiv 'integer? 'jg e0 e1 c))
(define (compile-<= e0 e1 c)
  (compile-type-equiv 'integer? 'jge e0 e1 c))

;; TypePred Op Expr Expr CEnv -> Asm
(define (compile-type-equiv t j e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i  (- (add1 (length c))))
        (l0 (gensym)))
    `(,@c0
      ,@(assert-type t)
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@(assert-type t)
      (cmp rax (offset rsp ,i))
      (mov rax ,imm-val-true)
      (,j ,l0)
      (mov rax ,imm-val-false)
      ,l0)))

;; Expr CEnv -> Asm
(define (compile-string-length e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-string
      (xor rax ,type-string)
      (mov rax (offset rax 0)))))

;; Expr Expr CEnv -> Asm
(define (compile-string-ref e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (i (- (add1 (length c)))))
    `(,@c0
      ,@assert-string
      (xor rax ,type-string)
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@assert-integer
      ,@(assert-valid-index i)
      (sar rax 2) ;; hold i * 8
      (add rax 8) ;; skip past length
      (add rax (offset rsp ,(- (add1 (length c)))))
      (mov rax (offset rax 0)))))

;; Expr Expr CEnv -> Asm
(define (compile-make-string e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c)))
        (done (gensym 'make_string_done))
        (loop (gensym 'make_string_loop))
        (i (- (add1 (length c)))))
    `(,@c0
      ,@assert-natural
      (mov (offset rsp ,i) rax)
      ,@c1
      ,@assert-char
      (mov rbx (offset rsp ,i))
      (mov (offset rdi 0) rbx)
      (mov rcx rdi)
      (sar rbx ,(- imm-shift 3)) ; rbx = len*8
      (add rdi 8)
      (add rbx rdi)
      ,loop
      (cmp rbx rdi)
      (je ,done)
      (mov (offset rdi 0) rax)
      (add rdi 8)
      (jmp ,loop)
      ,done
      (mov rax rcx)
      (or rax ,type-string))))

;; String -> Asm
(define (compile-string s)
  (let ((c (compile-string-chars (string->list s) 1)))
    `(,@c
      (mov rax ,(imm->bits (string-length s)))
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (add rax ,type-string)
      (add rdi ,(* 8 (add1 (string-length s)))))))

;; (Listof Char) Natural -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() '()]
    [(cons c cs)
     `((mov rax ,(imm->bits c))
       (mov (offset rdi ,i) rax)
       ,@(compile-string-chars cs (add1 i)))]))

;; Variable CEnv -> Asm
(define (compile-var x c)
  (let ((i (lookup x c)))
    `((mov rax (offset rsp ,(- (add1 i)))))))

;; Expr CEnv -> Asm
(define (compile-box e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-box)
      (add rdi 8)))) ; allocate 8 bytes

;; Expr CEnv -> Asm
(define (compile-unbox e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-box
      (xor rax ,type-box)
      (mov rax (offset rax 0)))))

;; Expr Expr CEnv -> Asm
(define (compile-cons e0 e1 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 (cons #f c))))
    `(,@c0
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c1
      (mov (offset rdi 1) rax)
      (mov rax (offset rsp ,(- (add1 (length c)))))
      (mov (offset rdi 0) rax)
      (mov rax rdi)
      (or rax ,type-pair)
      (add rdi 16))))

;; Expr CEnv -> Asm
(define (compile-car e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 0)))))

;; Expr CEnv -> Asm
(define (compile-cdr e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-pair
      (xor rax ,type-pair) ; untag
      (mov rax (offset rax 1)))))

;; Expr CEnv -> Asm
(define (compile-add1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (add rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-sub1 e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (sub rax ,(arithmetic-shift 1 imm-shift)))))

;; Expr CEnv -> Asm
(define (compile-zero? e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      ,@assert-integer
      (cmp rax 0)
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e0 e1 e2 c)
  (let ((c0 (compile-e e0 c))
        (c1 (compile-e e1 c))
        (c2 (compile-e e2 c))
        (l0 (gensym))
        (l1 (gensym)))
    `(,@c0
      (cmp rax ,imm-val-false)
      (je ,l0)
      ,@c1
      (jmp ,l1)
      ,l0
      ,@c2
      ,l1)))

;; Expr Expr CEnv -> Asm
(define (compile-+ e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (add rax (offset rsp ,(- (add1 (length c))))))))

;; Expr Expr CEnv -> Asm
(define (compile-binary-- e0 e1 c)
  (let ((c1 (compile-e e1 c))
        (c0 (compile-e e0 (cons #f c))))
    `(,@c1
      ,@assert-integer
      (mov (offset rsp ,(- (add1 (length c)))) rax)
      ,@c0
      ,@assert-integer
      (sub rax (offset rsp ,(- (add1 (length c))))))))

;; (Listof (List Variable Expr)) Expr CEnv -> Asm
(define (compile-let bs e1 c)
  (let ((c0 (compile-let-rhs (get-defs bs) c))
        (c1 (compile-e e1 (append (reverse (get-vars bs)) c))))
    `(,@c0
      ,@c1)))

;; (Listof Expr) CEnv -> Asm
;; Compile the RHSs of a let
(define (compile-let-rhs  es c)
  (match es
    ['() '()]
    [(cons e es)
     (let ((c0 (compile-e e c)))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@(compile-let-rhs es (cons (gensym) c))))]))

;; TypePred Expr CEnv -> Asm
(define (compile-type-pred p e0 c)
  (let ((c0 (compile-e e0 c))
        (l0 (gensym)))
    `(,@c0
      (and rax ,(type-pred->mask p))
      (cmp rax ,(type-pred->tag p))
      (mov rax ,imm-val-false)
      (jne ,l0)
      (mov rax ,imm-val-true)
      ,l0)))

;; TypePred -> Integer
(define (type-pred->mask p)
  (match p
    [(or 'box? 'cons? 'string?) result-type-mask]
    [_ imm-type-mask]))

;; TypePred -> Integer
(define (type-pred->tag p)
  (match p
    ['box?     type-box]
    ['cons?    type-pair]
    ['string?  type-string]
    ['integer? imm-type-int]
    ['empty?   imm-type-empty]
    ['char?    imm-type-char]
    ['boolean? imm-type-bool]))

;; Expr CEnv -> Asm
(define (compile-char->integer e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-char
      (xor rax ,imm-type-char))))

;; Expr CEnv -> Asm
(define (compile-integer->char e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer-codepoint
      (xor rax ,imm-type-char))))

;; Expr CEnv -> Asm
(define (compile-abs e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (mov rbx rax)
      (neg rax)
      (cmovl rax rbx))))

;; Expr CEnv -> Asm
(define (compile-unary-- e0 c)
  (let ((c0 (compile-e e0 c)))
    `(,@c0
      ,@assert-integer
      (neg rax))))

;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "unbound variable" x)]
    [(cons y cenv)
     (match (eq? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

;; TypePred -> Asm
(define (assert-type p)
  `((mov rbx rax)
    (and rbx ,(type-pred->mask p))
    (cmp rbx ,(type-pred->tag p))
    (jne err)))

(define assert-integer (assert-type 'integer?))
(define assert-box     (assert-type 'box?))
(define assert-pair    (assert-type 'cons?))
(define assert-string  (assert-type 'string?))
(define assert-char    (assert-type 'char?))

;; Asm
(define assert-natural
  `(,@assert-integer
    (cmp rax -1)
    (jle err)))

;; Asm
(define assert-integer-codepoint
  `((mov rbx rax)
    (and rbx ,imm-type-mask)
    (cmp rbx 0)
    (jne err)
    (cmp rax ,(arithmetic-shift -1 imm-shift))
    (jle err)
    (cmp rax ,(arithmetic-shift #x10FFFF imm-shift))
    (mov rbx rax)
    (sar rbx ,(+ 11 imm-shift))
    (cmp rbx #b11011)
    (je err)))

;; Asm
(define (assert-valid-index i)
  `((cmp rax ,(arithmetic-shift -1 imm-shift))
    (jle err)
    (mov rbx (offset rsp ,i))
    (mov rbx (offset rbx 0))
    (cmp rbx rax)
    (jle err)))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

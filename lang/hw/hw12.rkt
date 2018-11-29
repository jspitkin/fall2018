#lang plait

(define-type Value
  (litV [n : 'a])
  (closV [arg : Symbol]
         [body : (Exp 'a)]
         [env : Env]))

(define-type (Exp 'a)
  (litE [n : 'a])
  (idE [s : Symbol])
  (plusE [l : (Exp 'a)] 
         [r : (Exp 'a)])
  (multE [l : (Exp 'a)]
         [r : (Exp 'a)])
  (lamE [n : Symbol]
        [body : (Exp 'a)])
  (appE [fun : (Exp 'a)]
        [arg : (Exp 'a)]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp] [pat : S-Exp] [s-exp-> : (S-Exp -> 'a)]) : (Exp 'a)
  (cond
    [(s-exp-match? pat s) (litE (s-exp-> s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)) pat s-exp->)
            (parse (third (s-exp->list s)) pat s-exp->))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)) pat s-exp->)
            (parse (third (s-exp->list s)) pat s-exp->))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s)) pat s-exp->))
             (parse (second bs) pat s-exp->)))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s)) pat s-exp->))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)) pat s-exp->)
           (parse (second (s-exp->list s)) pat s-exp->))]
    [else (error 'parse "invalid input")]))

(define (parse/num [s : S-Exp]) : (Exp Number)
  (parse s `NUMBER s-exp->number))

(module+ test
  (test (parse/num `2)
        (litE 2))
  (test (parse/num `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse/num `{+ 2 1})
        (plusE (litE 2) (litE 1)))
  (test (parse/num `{* 3 4})
        (multE (litE 3) (litE 4)))
  (test (parse/num `{+ {* 3 4} 8})
        (plusE (multE (litE 3) (litE 4))
               (litE 8)))
  (test (parse/num `{let {[x {+ 1 2}]}
                      y})
        (appE (lamE 'x (idE 'y))
              (plusE (litE 1) (litE 2))))
  (test (parse/num `{lambda {x} 9})
        (lamE 'x (litE 9)))
  (test (parse/num `{double 9})
        (appE (idE 'double) (litE 9)))
  (test/exn (parse/num `{{+ 1 2}})
            "invalid input")
  (test/exn (parse/num `"a")
            "invalid input"))

(define (parse/str [s : S-Exp]) : (Exp String)
  (parse s `STRING s-exp->string))

(module+ test
  (test (parse/str `"a")
        (litE "a"))
  (test (parse/str `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse/str `{+ "b" "a"})
        (plusE (litE "b") (litE "a")))
  (test (parse/str `{* "c" "d"})
        (multE (litE "c") (litE "d")))
  (test (parse/str `{+ {* "c" "d"} "e"})
        (plusE (multE (litE "c") (litE "d"))
               (litE "e")))
  (test (parse/str `{let {[x {+ "a" "b"}]}
                      y})
        (appE (lamE 'x (idE 'y))
              (plusE (litE "a") (litE "b"))))
  (test (parse/str `{lambda {x} "g"})
        (lamE 'x (litE "g")))
  (test (parse/str `{double "g"})
        (appE (idE 'double) (litE "g")))
  (test/exn (parse/str `{{+ "a" "b"}})
            "invalid input")
  (test/exn (parse/str `1)
            "invalid input"))

;; interp ----------------------------------------
(define interp : ((Exp 'a) Env -> Value)
  (lambda (a env)
    (type-case (Exp 'a) a
      [(litE n) (litV n)]
      [(idE s) (lookup s env)]
      [(plusE l r) (num+ (interp l env) (interp r env))]
      [(multE l r) (num* (interp l env) (interp r env))]
      [(lamE n body)
       (closV n body env)]
      [(appE fun arg) (type-case Value (interp fun env)
                        [(closV n body c-env)
                         (interp body
                                 (extend-env
                                  (bind n
                                        (interp arg env))
                                  c-env))]
                        [else (error 'interp "not a function")])])))

(define (interp/num [a : (Exp Number)] [env : Env]) : Value
  (interp a env))

(module+ test
  (test (interp/num (parse/num `2) mt-env)
        (litV 2))
  (test/exn (interp/num (parse/num `x) mt-env)
            "free variable")
  (test (interp/num (parse/num `x) 
                    (extend-env (bind 'x (litV 9)) mt-env))
        (litV 9))
  (test (interp/num (parse/num `{+ 2 1}) mt-env)
        (litV 3))
  (test (interp/num (parse/num `{* 2 1}) mt-env)
        (litV 2))
  (test (interp/num (parse/num `{+ {* 2 3} {+ 5 8}})
                    mt-env)
        (litV 19))
  (test (interp/num (parse/num `{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {+ x x}})
                    mt-env)
        (litV 10))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {let {[x {+ 1 x}]}
                                    {+ x x}}})
                    mt-env)
        (litV 12))
  (test (interp/num (parse/num `{let {[x 5]}
                                  {let {[y 6]}
                                    x}})
                    mt-env)
        (litV 5))
  (test (interp/num (parse/num `{{lambda {x} {+ x x}} 8})
                    mt-env)
        (litV 16))

  (test/exn (interp/num (parse/num `{1 2}) mt-env)
            "not a function")
  (test/exn (interp/num (parse/num `{+ 1 {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/num (parse/num `{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y 5]}
                                        {bad 2}}})
                        mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define num-op : ((Number Number -> Number)
                  Value
                  Value
                  -> Value)
  (lambda (op l r)
    (cond
      [(and (litV? l) (litV? r))
       (litV (op (litV-n l) (litV-n r)))]
      [else
       (error 'interp "not a literal")])))

(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (litV 1) (litV 2))
        (litV 3))
  (test (num* (litV 2) (litV 3))
        (litV 6)))

;; lookup ----------------------------------------
(define lookup : (Symbol Env -> Value)
  (lambda (n env)
    (cond
      [(empty? env) (error 'lookup "free variable")]
      [else (cond
              [(symbol=? n (bind-name (first env)))
               (bind-val (first env))]
              [else (lookup n (rest env))])])))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (litV 8)) mt-env))
        (litV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'x (litV 8)) mt-env)))
        (litV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (litV 9))
                    (extend-env (bind 'y (litV 8)) mt-env)))
        (litV 8)))
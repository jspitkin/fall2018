#lang plait

(define-type Value
  (numV [n : Number])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (contV [k : Cont]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [ns : (Listof Symbol)]
        [body : Exp])
  (appE [f : Exp]
        [args : (Listof Exp)])
  (let/ccE [n : Symbol]
           [body : Exp])
  (negE [a : Exp])
  (avgE [x : Exp]
        [y : Exp]
        [z : Exp])
  (if0E [tst : Exp]
       [thn : Exp]
       [els : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)
(define extend-env* append)

(define-type Cont
  (doneK)
  (plusSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doPlusK [v : Value]
           [k : Cont])
  (multSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doMultK [v : Value]
           [k : Cont])
  (appArgK [args : (Listof Exp)]
           [env : Env]
           [k : Cont])
  (appArgRecK [f : Value]
              [args : (Listof Exp)]
              [vals : (Listof Value)]
              [env : Env]
              [k : Cont])
  (doAppK [f : Value]
          [k : Cont])
  (doNegK [k : Cont])
  (avgSecondK [y : Exp]
              [z : Exp]
              [e : Env]
              [k : Cont])
  (avgThirdK [x : Value]
             [z : Exp]
             [e : Env]
             [k : Cont])
  (doAvgK [x : Value]
          [y : Value]
          [k : Cont])
  (doIf0K [thn : Exp]
          [els : Exp]
          [e : Env]
          [k : Cont]))

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (second bs)))))]
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s)
     (lamE (map s-exp->symbol (s-exp->list 
                               (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let/cc SYMBOL ANY} s)
     (let/ccE (s-exp->symbol (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{neg ANY} s)
     (negE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{avg ANY ANY ANY} s)
     (avgE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {x} 9})
        (lamE (list 'x) (numE 9)))
  (test (parse `{let/cc k 0})
        (let/ccE 'k (numE 0)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test/exn (parse `{})
            "invalid input"))

;; interp & continue ----------------------------------------
(define (interp [a : Exp] [env : Env] [k : Cont]) : Value
  (type-case Exp a
    [(numE n) (continue k (numV n))]
    [(idE s) (continue k (lookup s env))]
    [(plusE l r) (interp l env
                         (plusSecondK r env k))]
    [(multE l r) (interp l env
                         (multSecondK r env k))]
    [(lamE ns body)
     (continue k (closV ns body env))]
    [(appE f args) (interp f env
                             (appArgK args env k))]
    [(let/ccE n body)
     (interp body
             (extend-env (bind n (contV k))
                         env)
             k)]
    [(negE e) (interp e env
                      (doNegK k))]
    [(avgE x y z) (interp x env
                          (avgSecondK y z env k))]
    [(if0E tst thn els) (interp tst env
                                (doIf0K thn els env k))]))

(define (continue [k : Cont] [v : Value]) : Value
  (type-case Cont k
    [(doneK) v]
    [(plusSecondK r env next-k)
     (interp r env
             (doPlusK v next-k))]
    [(doPlusK v-l next-k)
     (continue next-k (num+ v-l v))]
    [(multSecondK r env next-k)
     (interp r env
             (doMultK v next-k))]
    [(doMultK v-l next-k)
     (continue next-k (num* v-l v))]
    [(appArgK args env next-k)
     (type-case Value v ; v - the function being applied
       [(closV c-args body c-env)
        (type-case (Listof Exp) args
          [(cons arg rst-args)
           (interp arg env (appArgRecK v rst-args empty env next-k))] ; 1 or more args - interp args recursively
          [empty
           (interp body c-env next-k)])] ; 0 args - just interp
       [(contV k-v) (interp (first args) env
                            (doAppK v next-k))]
       [else (error 'interp "not a function")])]
    [(appArgRecK f args vals env next-k)
     (type-case (Listof Exp) args
       [(cons arg rst-args)
        (interp arg env ; interp next arg, add prev arg to values, and recurse
                (appArgRecK f rst-args (append vals (list v)) env next-k))]
       [empty ; all args interped
        (type-case Value f
          [(closV ns body c-env) ; interp function
           (interp body
                   (extend-env*
                    (map2 bind ns (append vals (list v)))
                    c-env)
                   next-k)]
          [else (error 'interp "not a function")])])]
    [(doAppK v-f next-k)
     (type-case Value v-f
       [(closV ns body c-env)
        (interp body
                (extend-env*
                 (map2 bind ns (list v))
                 c-env)
                next-k)]
       [(contV k-v) (continue k-v v)]
       [else (error 'interp "not a function")])]
    [(doNegK next-k) (continue next-k (num* v (numV -1)))]
    [(avgSecondK y z env next-k)
     (interp y env
             (avgThirdK v z env next-k))]
    [(avgThirdK x-v z env next-k)
     (interp z env
             (doAvgK x-v v next-k))]
    [(doAvgK x-v y-v next-k)
     (continue next-k (avgThree x-v y-v v))]
    [(doIf0K thn els env next-k)
     (continue next-k (if (equal? v (numV 0))
                          (interp thn env next-k)
                          (interp els env next-k)))]))

(module+ test
  (test (interp (parse `2) mt-env (doneK))
        (numV 2))
  (test/exn (interp (parse `x) mt-env (doneK))
            "free variable")
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env (doneK))
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env (doneK))
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                (doneK))
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                (doneK))
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK))
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                (doneK))
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK))
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK))
        (numV 16))

  (test (interp (parse `{let/cc k {+ 1 {k 0}}})
                mt-env
                (doneK))
        (numV 0))
  (test (interp (parse `{let {[f {let/cc k k}]}
                          {f {lambda {x} 10}}})
                mt-env
                (doneK))
        (numV 10))

  (test/exn (continue (doAppK (numV 1) (doneK)) (numV 1))
            "not a function")
  (test/exn (continue (appArgRecK (numV 1) (list (numE 1)) empty mt-env (doneK)) (numV 1))
            "not a function")
  (test/exn (interp (parse `{1 2}) mt-env (doneK))
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK))
            "free variable")
  ;; Eager:
  (test/exn (interp (parse `{{lambda {x} 0} {1 2}}) mt-env (doneK))
            "not a function")

  (test (continue (doneK) (numV 5))
        (numV 5))
  (test (continue (plusSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 11))
  (test (continue (doPlusK (numV 7) (doneK)) (numV 5))
        (numV 12))
  (test (continue (multSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 30))
  (test (continue (doMultK (numV 7) (doneK)) (numV 5))
        (numV 35))
  (test (continue (appArgK (list (numE 5)) mt-env (doneK)) (closV (list 'x) (idE 'x) mt-env))
        (numV 5))
  (test (continue (doAppK (closV (list 'x) (idE 'x) mt-env) (doneK)) (numV 8))
        (numV 8)))

;; interp-expr -----------------------------------------
(define (interp-expr [a : Exp]) : S-Exp
  (type-case Value (interp a mt-env (doneK))
    [(numV n) (number->s-exp n)]
    [else `function]))

(module+ test
  (test (interp-expr (parse `{neg 2}))
        `-2)
  (test (interp-expr (parse `{avg 0 6 6}))
        `4)
  (test (interp-expr (parse `{let/cc k {neg {k 3}}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg 0 {k 3} 0}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg {k 2} {k 3} 0}}))
        `2)
  (test (interp-expr (parse `{if0 1 2 3}))
        `3)
  (test (interp-expr (parse `{if0 0 2 3}))
        `2)
  (test (interp-expr (parse `{let/cc k {if0 {k 9} 2 3}}))
        `9)
  (test (interp-expr (parse `{{lambda {x y} {+ y {neg x}}} 10 12}))
        `2)
  (test (interp-expr (parse `{lambda {} 12}))
        `function)
  (test (interp-expr (parse `{{lambda {} 12}}))
        `12)
  (test (interp-expr (parse `{lambda {x} {lambda {} x}}))
        `function)
  (test (interp-expr (parse `{{{lambda {x} {lambda {} x}} 13}}))
        `13)

  (test (interp-expr (parse `{let/cc esc {{lambda {x y} x} 1 {esc 3}}}))
        `3)
  (test (interp-expr (parse `{{let/cc esc {{lambda {x y} {lambda {z} {+ z y}}}
                                           1 
                                           {let/cc k {esc k}}}}
                              10}))
        `20))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (bind-val b)]
                        [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; Avg -------------------------------------------------
(define (avgThree [a : Value] [b : Value] [c : Value]) : Value
  (type-case Value a
    [(numV n-a) (type-case Value b
                  [(numV n-b) (type-case Value c
                                [(numV n-c) (numV (/ (+ (+ n-a n-b) n-c) 3))]
                                [else (error 'interp "not a number")])]
                  [else (error 'interp "not a number")])]
    [else (error 'interp "not a number")]))

(module+ test
  (test (avgThree (numV 1) (numV 5) (numV 3))
        (numV 3))
  (test (avgThree (numV 0) (numV -10) (numV 4))
        (numV -2))
  (test/exn (avgThree (contV (doneK)) (numV 1) (numV 1))
            "not a number")
  (test/exn (avgThree (numV 1) (contV (doneK)) (numV 1))
            "not a number")
  (test/exn (avgThree (numV 1) (numV 1) (contV (doneK)))
            "not a number"))
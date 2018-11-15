#lang plait

(define-type Value
  (numV [n : Number])
  (trueV)
  (falseV)
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (pairV [fst-v : Value]
         [snd-v : Value]))

(define-type Exp
  (numE [n : Number])
  (trueE)
  (falseE)
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [ns : (Listof Symbol)]
        [arg-types : (Listof Type)]
        [body : Exp])
  (equalE [l : Exp]
          [r : Exp])
  (ifE [cnd : Exp]
       [thn : Exp]
       [els : Exp])
  (pairE [fst : Exp]
         [snd : Exp])
  (fstE [pair : Exp])
  (sndE [pair : Exp])
  (appE [fun : Exp]
        [args : (Listof Exp)]))

(define-type Type
  (numT)
  (boolT)
  (arrowT [args : (Listof Type)]
          [result : Type])
  (crossT [fst-t : Type]
         [snd-t : Type]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `true s) (trueE)]
    [(s-exp-match? `false s) (falseE)]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (list (parse-type (third bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (fourth bs)))))]
    [(s-exp-match? `{lambda {[SYMBOL : ANY] ...} ANY} s)
     (lamE (map s-exp->symbol
                (map first
                     (map s-exp->list
                          (s-exp->list (second
                                        (s-exp->list s))))))
           (map parse-type
                (map third
                     (map s-exp->list
                          (s-exp->list (second
                                        (s-exp->list s))))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)
     (equalE (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{pair ANY ANY} s)
     (pairE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{fst ANY} s)
     (fstE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{snd ANY} s)
     (sndE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]
   [(s-exp-match? `(ANY ... -> ANY) s)
    (arrowT (map parse-type (get-args (s-exp->list s)))
            (parse-type (first (reverse (s-exp->list s)))))]
   [(s-exp-match? `(ANY * ANY) s)
    (crossT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
   [else (error 'parse-type "invalid input")]))

(define (get-args [args : (Listof S-Exp)]) : (Listof S-Exp)
  (type-case (Listof S-Exp) args
    [empty empty]
    [(cons arg rst-args)
     (if (equal? (length args) 2)
         empty
         (cons arg (get-args rst-args)))]))

(module+ test
  (test (get-args empty)
        empty)
  (test (get-args (list `-> `num))
        empty)
  (test (get-args (list `num `-> `num))
        (list `num))
  (test (get-args (list `num `bool `-> `num))
        (list `num `bool)))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x : num {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (list (numT)) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {[x : num]} 9})
        (lamE (list 'x) (list (numT)) (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test (parse `{{+ 1 2}})
        (appE (plusE (numE 1) (numE 2)) '()))

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (list (numT)) (boolT)))
  (test/exn (parse `{})
            "invalid input")
  (test/exn (parse-type `1)
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
 (type-case Exp a
   [(numE n) (numV n)]
   [(trueE) (trueV)]
   [(falseE) (falseV)]
   [(idE s) (lookup s env)]
   [(plusE l r) (num+ (interp l env) (interp r env))]
   [(multE l r) (num* (interp l env) (interp r env))]
   [(lamE ns t body) (closV ns body env)]
   [(equalE l r) (if (equal? (interp l env) (interp r env))
                     (trueV)
                     (falseV))]
   [(ifE cnd thn els) (if (equal? (interp cnd env) (trueV))
                          (interp thn env)
                          (interp els env))]
   [(pairE fst snd) (pairV (interp fst env) (interp snd env))]
   [(fstE pair)
    (local [(define pair-v (interp pair env))]
      (pairV-fst-v pair-v))]
   [(sndE pair)
    (local [(define pair-v (interp pair env))]
      (pairV-snd-v pair-v))]
   [(appE fun args)
    (type-case Value (interp fun env)
      [(closV ns body c-env)
       (interp body (extend-env-list (map2 bind ns (map (lambda (arg)
                                          (interp arg env))
                                        args))
                                     c-env))]
      [else (error 'interp "not a function")])]))

(define (extend-env-list [new-env : Env] [env : Env]) : Env
  (type-case (Listof Binding) new-env
    [empty env]
    [(cons b rst-env) (extend-env-list rst-env (cons b env))]))
  

(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test (interp (parse `true) mt-env)
        (trueV))
  (test (interp (parse `false) mt-env)
        (falseV))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))
  (test (interp (parse `{if true 4 5})
                mt-env)
         (numV 4))
  
  (test (interp (parse `{if false 4 5})
                mt-env)
         (numV 5))
  (test (interp (parse `{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
         (numV 5))
  (test (interp (parse `{pair 10 8})
                mt-env)
        (pairV (numV 10) (numV 8)))
  
  (test (interp (parse `{fst {pair 10 8}})
                mt-env)
        (numV 10))
  
  (test (interp (parse `{snd {pair 10 8}})
                mt-env)
        (numV 8))
  
  (test (interp (parse `{let {[p : (num * num) {pair 10 8}]}
                          {fst p}})
                mt-env)
        (numV 10))
  (test (interp (parse `{{lambda {}
                           10}})
                mt-env)
        (numV 10))
  
  (test (interp (parse `{{lambda {[x : num] [y : num]} {+ x y}}
                         10
                         20})
                mt-env)
        (numV 30))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

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
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
    (type-case (Listof 'a) vals
      [empty (error 'find "free variable")]
      [(cons val rst-vals) (if (equal? name (name-of val))
                               (val-of (first vals))
                               ((make-lookup name-of val-of) name rst-vals))])))

(define lookup
  (make-lookup bind-name bind-val))

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

;; typecheck ----------------------------------------
(define (typecheck [a : Exp] [tenv : Type-Env])
  (type-case Exp a
    [(numE n) (numT)]
    [(trueE) (boolT)]
    [(falseE) (boolT)]
    [(plusE l r) (typecheck-nums l r tenv)]
    [(multE l r) (typecheck-nums l r tenv)]
    [(idE n) (type-lookup n tenv)]
    [(lamE ns arg-types body)
     (arrowT arg-types
             (typecheck body (extend-tenv-list (map2 tbind ns arg-types) tenv)))]
    [(equalE l r) (typecheck-equal l r tenv)]
    [(ifE cnd thn els) (typecheck-if cnd thn els tenv)]
    [(pairE fst snd) (crossT (typecheck fst tenv) (typecheck snd tenv))]
    [(fstE pair)
     (type-case Type (typecheck pair tenv)
       [(crossT fst-t snd-t) fst-t]
       [else (type-error pair "pair")])]
    [(sndE pair)
     (type-case Type (typecheck pair tenv)
       [(crossT fst-t snd-t) snd-t]
       [else (type-error pair "pair")])]
    [(appE fun args)
     (type-case Type (typecheck fun tenv)
       [(arrowT arg-types result-type)
        (if (typecheck-list arg-types args tenv)
            result-type
            (type-error (first args)
                        (to-string arg-types)))]
       [else (type-error fun "function")])]))

(define (extend-tenv-list [new-env : Type-Env] [env : Type-Env]) : Type-Env
  (type-case (Listof Type-Binding) new-env
    [empty env]
    [(cons b rst-env) (extend-tenv-list rst-env (cons b env))]))

(define (typecheck-nums l r tenv)
  (type-case Type (typecheck l tenv)
    [(numT)
     (type-case Type (typecheck r tenv)
       [(numT) (numT)]
       [else (type-error r "num")])]
    [else (type-error l "num")]))

(define (typecheck-equal l r tenv)
  (type-case Type (typecheck l tenv)
    [(numT)
     (type-case Type (typecheck r tenv)
       [(numT) (boolT)]
       [else (type-error r "num")])]
    [else (type-error l "num")]))

(define (typecheck-if con thn els tenv)
  (type-case Type (typecheck con tenv)
    [(boolT)
     (local [(define thn-type (typecheck thn tenv))]
       (if (equal? thn-type
                   (typecheck els tenv))
           thn-type
           (type-error con "matching")))]
    [else (type-error con "boolean")]))

(define (typecheck-list arg-types args tenv)
  (type-case (Listof Type) arg-types
    [empty #t]
    [(cons arg-type rst-arg-types) (if (equal? arg-type (typecheck (first args) tenv))
                                       (typecheck-list rst-arg-types (rest args) tenv)
                                       #f)]))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test
  (test (typecheck (parse `10) mt-env)
        (numT))
  (test (typecheck (parse `false) mt-env)
        (boolT))
  (test (typecheck (parse `{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{lambda {[x : num]} 12}) mt-env)
        (arrowT (list (numT)) (numT)))
  (test (typecheck (parse `{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (list (numT)) (arrowT (list (boolT))  (numT))))

  (test (typecheck (parse `{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))
  (test (typecheck (parse `{= 13 {if {= 1 {+ -1 2}}
                                     12
                                     13}})
                   mt-env)
         (boolT))
  
  (test (typecheck (parse `{if {= 1 {+ -1 2}}
                               {lambda {[x : num]} {+ x 1}}
                               {lambda {[y : num]} y}})
                   mt-env)
        (arrowT (list (numT)) (numT)))
  (test (typecheck (parse `{pair 10 8})
                   mt-env)
        (crossT (numT) (numT)))
  
  (test (typecheck (parse `{fst {pair 10 8}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{+ 1 {snd {pair 10 8}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{lambda {[x : (num * bool)]}
                             {fst x}})
                   mt-env)
        (arrowT (list (crossT (numT) (boolT))) (numT)))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {fst x}}
                            {pair 1 false}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {snd x}}
                            {pair 1 false}})
                   mt-env)
        (boolT))
  (test (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
                            10
                            false})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
                                false
                                10})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{fst 10})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{+ 1 {fst {pair false 8}}})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{lambda {[x : (num * bool)]}
                                 {if {fst x}
                                     1
                                     2}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {if true true false}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{= 1 true})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{= true 1})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{if 1 1 1})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{if true 1 false})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{fst 1})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{snd 1})
                       mt-env)
            "no type"))
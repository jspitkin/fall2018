#lang plait

(define-type Exp
  (numE [n : Number])
  (plusE [lhs : Exp]
         [rhs : Exp])
  (multE [lhs : Exp]
         [rhs : Exp])
  (argE)
  (thisE)
  (newE [class-name : Symbol])
  (getE [obj-expr : Exp]
        [field-name : Symbol])
  (sendE [obj-expr : Exp]
         [method-name : Symbol]
         [arg-expr : Exp])
  (ssendE [obj-expr : Exp]
          [class-name : Symbol]
          [method-name : Symbol]
          [arg-expr : Exp])
  (castE [cast-type : Symbol]
         [obj-expr : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp])
  (nullE)
  (setE [obj-expr : Exp]
        [field-name : Symbol]
        [set-expr : Exp]))

(define-type Class
  (classC [super-name : Symbol]
          [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * Exp))]))

(define-type Value
  (numV [n : Number])
  (objV [class-name : Symbol]
        [field-values : (Listof (Boxof Value))])
  (nullV))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
  (type-case (Listof (Symbol * 'a)) l
    [empty
     (error 'find (string-append "not found: " (symbol->string name)))]
    [(cons p rst-l)
     (if (symbol=? (fst p) name)
         (snd p)
         (find rst-l name))]))

(module+ test
  (test (find (list (values 'a 1)) 'a)
        1)
  (test (find (list (values 'a 1) (values 'b 2)) 'b)
        2)
  (test/exn (find empty 'a)
            "not found: a")
  (test/exn (find (list (values 'a 1)) 'x)
            "not found: x"))

;; ----------------------------------------

(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(thisE) this-val]
        [(argE) arg-val]
        [(newE class-name)
         (objV class-name
               (create-zero-list (length (classC-field-names (find classes class-name)))
                                 empty))]
         [(getE obj-expr field-name)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC super-name field-names methods)
               (unbox (find (map2 (lambda (n v) (values n v))
                                  field-names
                                  field-vals)
                            field-name))])]
           [else (error 'interp "not an object")])]
        [(sendE obj-expr method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (call-method class-name method-name classes
                           obj arg-val)]
             [else (error 'interp "not an object")]))]
        [(ssendE obj-expr class-name method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (call-method class-name method-name classes
                        obj arg-val))]
        [(castE cast-name obj-expr)
         (local [(define obj (recur obj-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (if (is-instance-of? cast-name class-name classes)
                  obj
                  (error 'interp "not a subclass"))]
             [else (error 'interp "not an object")]))]
        [(if0E tst thn els)
         (local [(define tst-val (recur tst))]
           (type-case Value tst-val
             [(numV n)
              (if (equal? 0 n)
                  (recur thn)
                  (recur els))]
             [else (error 'interp "not a number")]))]
        [(nullE) (nullV)]
        [(setE obj-expr field-name set-expr)
         (local [(define set-val (recur set-expr))]
           (type-case Value (recur obj-expr)
             [(objV class-name field-vals)
              (type-case Class (find classes class-name)
                [(classC super-name field-names methods)
                 (begin
                   (set-box! (find (map2 (lambda (n v) (values n v))
                                         field-names
                                         field-vals)
                                   field-name)
                             set-val)
                   set-val)])]
             [else (error 'interp "not an object")]))]))))

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC super-name field-names methods)
     (let ([body-expr (find methods method-name)])
       (interp body-expr
               classes
               obj
               arg-val))]))

(define (num-op [op : (Number Number -> Number)]
                [op-name : Symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

(define (is-instance-of? [class-name : Symbol] [search-class-name : Symbol] [classes : (Listof (Symbol * Class))]) : Boolean
  (if (eq? search-class-name class-name)
      #t
      (if (eq? class-name 'Object)
          #f
          (is-instance-of? (classC-super-name (find classes class-name)) search-class-name classes))))

(define (create-zero-list [length : Number] [list : (Listof (Boxof Value))])
  (if (eq? length 0)
      list
      (create-zero-list (- length 1) (cons (box (numV 0)) list))))

(module+ test
  (test (create-zero-list 0 empty)
        empty)
  (test (create-zero-list 1 empty)
        (list (box (numV 0))))
  (test (create-zero-list 3 empty)
        (list (box (numV 0)) (box (numV 0)) (box (numV 0)))))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (values 'Posn
            (classC
             'Object
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn))))))
    
  (define posn3D-class
    (values 'Posn3D
            (classC
             'Posn
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE)))))))

  (define dummy-class 
    (values 'Dummy
            (classC
             'Object
             (list 'bin)
             (list (values 'getBin (getE (thisE) 'bin))))))

  (define bin-class
    (values 'Bin
            (classC
             'Object
             (list 'x)
             empty)))

  (define posn27 (newE 'Posn))
  (define posn531 (newE 'Posn3D))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class dummy-class bin-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (nullE)
                empty (objV 'Object empty) (numV 0))
        (nullV))
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0))
        (numV 10))
  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0))
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0))
        (numV 70))
  (test (interp-posn (newE 'Posn))
        (objV 'Posn (list (box (numV 0)) (box (numV 0)))))
  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 0))
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 10))
  (test (interp-posn (sendE (ssendE posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 0))
  (test (interp-posn (sendE posn531 'addDist posn27))
        (numV 0))
  (test (interp-posn (castE 'Posn3D (newE 'Posn)))
        (objV 'Posn (list (box (numV 0)) (box (numV 0)))))
  (test (interp-posn (if0E (numE 0)
                           (newE 'Posn)
                           (newE 'Posn)))
        (objV 'Posn (list (box (numV 0)) (box (numV 0)))))
  (test (interp-posn (if0E (numE 1)
                           (newE 'Posn)
                           (newE 'Posn3D)))
        (objV 'Posn3D (list (box (numV 0)) (box (numV 0)) (box (numV 0)))))
  (test (interp-posn (setE (newE 'Posn) 'x (numE 3)))
        (numV 3))
  (test (interp (sendE (thisE) 'getBin (setE (getE (thisE) 'bin) 'x (numE 1)))
                (list dummy-class bin-class)
                (objV 'Dummy (list (box (objV 'Bin (list (box (numV 0)))))))
                (numV -1))
        (objV 'Bin (list (box (numV 1)))))
  
  (test/exn (interp-posn (if0E (newE 'Posn) (numE 1) (numE 2)))
            "not a number")
  (test/exn (interp-posn (castE 'Dummy (newE 'Posn)))
            "not a subclass")
  (test/exn (interp-posn (castE 'Dummy (numE 1)))
            "not an object")
  (test/exn (interp-posn (plusE (numE 1) posn27))
            "not a number")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (setE (numE 1) 'x (numE 1)))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object"))
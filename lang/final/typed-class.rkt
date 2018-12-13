#lang plait

(require "class.rkt"
         "inherit.rkt")

(define-type ClassT
  (classT [super-name : Symbol]
          [fields : (Listof (Symbol * Type))]
          [methods : (Listof (Symbol * MethodT))]))

(define-type MethodT
  (methodT [arg-type : Type]
           [result-type : Type]
           [body-expr : ExpI]))

(define-type Type
  (numT)
  (objT [class-name : Symbol])
  (nullT))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'Object)
      empty        
      (type-case ClassT (find t-classes class-name)
        [(classT super-name fields methods)
         (append 
          (get-all-field-types super-name t-classes)
          (map snd fields))])))

;; ----------------------------------------

(define (make-find-in-tree class-items)
  (lambda (name class-name t-classes)
    (local [(define t-class (find t-classes class-name))
            (define items (class-items t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'Object)
          (find items name)
          (try (find items name)
               (lambda ()
                 ((make-find-in-tree class-items)
                  name 
                  super-name
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree classT-fields))

(define find-method-in-tree
  (make-find-in-tree classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) #t]
    [(equal? name1 'Object) #f]
    [else
     (type-case ClassT (find t-classes name1)
       [(classT super-name fields methods)
        (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [(objT name1)
     (type-case Type t2 
       [(objT name2)
        (is-subclass? name1 name2 t-classes)]
       [else #f])]
    [else (equal? t1 t2)]))

(define (is-supertype? t1 t2 t-classes)
  (type-case Type t1
    [(objT name1)
     (type-case Type t2
       [(objT name2)
        (is-subclass? name2 name1 t-classes)]
       [else #f])]
    [else (equal? t1 t2)]))

; returns true if t1 is objT and t2 is nullT - false otherwise
(define (is-null-obj? t1 t2)
  (type-case Type t1
    [(objT class-name)
     (type-case Type t2
       [(nullT) #t]
       [else #f])]
    [else #f]))

(module+ test
  (test (is-null-obj? (objT 'Posn) (nullT))
        #t)
  (test (is-null-obj? (numT) (nullT))
        #f)
  (test (is-null-obj? (nullT) (nullT))
        #f)
  (test (is-null-obj? (objT 'Posn) (objT 'Posn))
        #f)
  (test (is-null-obj? (objT 'Posn) (numT))
        #f))

(module+ test
  (define a-t-class (values 'A (classT 'Object empty empty)))
  (define b-t-class (values 'B (classT 'A empty empty)))

  (test (is-subclass? 'Object 'Object empty)
        #t)
  (test (is-subclass? 'A 'B (list a-t-class b-t-class))
        #f)
  (test (is-subclass? 'B 'A (list a-t-class b-t-class))
        #t)

  (test (is-subtype? (numT) (numT) empty)
        #t)
  (test (is-subtype? (numT) (objT 'Object) empty)
        #f)
  (test (is-subtype? (objT 'Object) (numT) empty)
        #f)
  (test (is-subtype? (objT 'A) (objT 'B) (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (objT 'B) (objT 'A) (list a-t-class b-t-class))
        #t)
  (test (is-supertype? (numT) (numT) empty)
        #t)
  (test (is-supertype? (objT 'Object) (numT) empty)
        #f)
  (test (is-supertype? (objT 'A) (objT 'B) (list a-t-class b-t-class))
        #t)
  (test (is-supertype? (objT 'B) (objT 'A) (list a-t-class b-t-class))
        #f)
  (test (is-supertype? (objT 'Object) (objT 'B) (list a-t-class b-t-class))
        #t))

;; ----------------------------------------

(define typecheck-expr : (ExpI (Listof (Symbol * ClassT)) Type Type -> Type)
  (lambda (expr t-classes this-type arg-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes this-type arg-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [(numT)
                 (type-case Type (recur r)
                   [(numT) (numT)]
                   [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExpI expr
        [(numI n) (numT)]
        [(plusI l r) (typecheck-nums l r)]
        [(multI l r) (typecheck-nums l r)]
        [(argI) arg-type]
        [(thisI) this-type]
        [(newI class-name exprs)
         (local [(define arg-types (map recur exprs))
                 (define field-types
                   (get-all-field-types class-name t-classes))]
           (if (and (= (length arg-types) (length field-types))
                    (foldl (lambda (b r) (and r b))
                           #t
                           (map2 (lambda (t1 t2) 
                                   (or (is-subtype? t1 t2 t-classes) (is-null-obj? t2 t1)))
                                 arg-types
                                 field-types)))
               (objT class-name)
               (type-error expr "field type mismatch")))]
        [(getI obj-expr field-name)
         (type-case Type (recur obj-expr)
           [(objT class-name)
            (find-field-in-tree field-name
                                class-name
                                t-classes)]
           [(nullT) (type-error obj-expr "object")]
           [else (type-error obj-expr "object")])]
        [(sendI obj-expr method-name arg-expr)
         (local [(define obj-type (recur obj-expr))
                 (define arg-type (recur arg-expr))]
           (type-case Type obj-type
             [(objT class-name)
              (typecheck-send class-name method-name
                              arg-expr arg-type
                              t-classes)]
             [(nullT) (type-error obj-expr "object")]
             [else (type-error obj-expr "object")]))]
        [(superI method-name arg-expr)
         (local [(define arg-type (recur arg-expr))
                 (define this-class
                   (find t-classes (objT-class-name this-type)))]
           (typecheck-send (classT-super-name this-class)
                           method-name
                           arg-expr arg-type
                           t-classes))]
        [(castI cast-type obj-expr)
         (local [(define obj-type (recur obj-expr))]
           (type-case Type obj-type
             [(objT class-name)
              (typecheck-cast cast-type
                              obj-type
                              t-classes
                              obj-expr)]
             [(nullT) (objT cast-type)]
             [else (type-error obj-expr "object")]))]
        [(if0I tst thn els)
         (local [(define tst-type (recur tst))
                 (define thn-type (recur thn))
                 (define els-type (recur els))]
           (typecheck-if0I tst-type thn-type els-type tst els t-classes))]
        [(nullI) (nullT)]
        [(setI obj-expr field-name set-expr)
         (type-case Type (recur obj-expr)
           [(objT class-name)
            (if (is-subtype? (recur set-expr)
                             (find-field-in-tree field-name class-name t-classes)
                             t-classes)
                (objT class-name)
                (type-error set-expr "object"))]
           [(nullT) (type-error obj-expr "object")]
           [else (type-error obj-expr "object")])]))))

(define (typecheck-if0I [tst-type : Type] [thn-type : Type] [els-type : Type]
                        [tst : ExpI] [els : ExpI] [t-classes : (Listof (Symbol * ClassT))])
  (type-case Type tst-type
    [(numT)
     (type-case Type thn-type
       [(numT)
        (type-case Type els-type
          [(numT) (numT)]
          [else (type-error els "object")])]
       [else
        (type-case Type els-type
          [(numT) (type-error els "number")]
          [else (objT (least-upper-bound (objT-class-name thn-type)
                                         (objT-class-name els-type)
                                         t-classes))])])]
    [else (type-error tst "number")]))

(define (typecheck-cast [cast-type : Symbol]
                        [obj-type : Type]
                        [t-classes : (Listof (Symbol * ClassT))]
                        [obj-expr : ExpI])
  (if (or (is-subtype? (objT cast-type) obj-type t-classes)
          (is-supertype? (objT cast-type) obj-type t-classes))
      (objT cast-type)
      (type-error obj-expr "object")))

(define (typecheck-send [class-name : Symbol]
                        [method-name : Symbol]
                        [arg-expr : ExpI]
                        [arg-type : Type]
                        [t-classes : (Listof (Symbol * ClassT))])
  (type-case MethodT (find-method-in-tree
                      method-name
                      class-name
                      t-classes)
    [(methodT arg-type-m result-type body-expr)
     (if (or (is-subtype? arg-type arg-type-m t-classes) (is-null-obj? arg-type-m arg-type))
         result-type
         (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (Listof (Symbol * ClassT))]) : ()
  (type-case MethodT method
    [(methodT arg-type result-type body-expr)
     (if (is-subtype? (typecheck-expr body-expr t-classes
                                      this-type arg-type)
                      result-type
                      t-classes)
         (values)
         (type-error body-expr (to-string result-type)))]))

(define (check-override [method-name : Symbol]
                        [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (Listof (Symbol * ClassT))])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree method-name
                                  super-name
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string method-name))))))

(define (typecheck-class [class-name : Symbol]
                         [t-class : ClassT]
                         [t-classes : (Listof (Symbol * ClassT))])
  (type-case ClassT t-class
    [(classT super-name fields methods)
     (map (lambda (m)
            (begin
              (typecheck-method (snd m) (objT class-name) t-classes)
              (check-override (fst m) (snd m) t-class t-classes)))
          methods)]))

(define (typecheck [a : ExpI]
                   [t-classes : (Listof (Symbol * ClassT))]) : Type
  (begin
    (map (lambda (tc)
           (typecheck-class (fst tc) (snd tc) t-classes))
         t-classes)
    (typecheck-expr a t-classes (objT 'Object) (numT))))

;; ----------------------------------------

(module+ test
  (define posn-t-class
    (values 'Posn
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'addDist
                                  (methodT (objT 'Posn) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))))))
  
  (define posn3D-t-class 
    (values 'Posn3D
            (classT 'Posn
                    (list (values 'z (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT)
                                           (plusI (getI (thisI) 'z) 
                                                  (superI 'mdist (argI)))))))))
  
  (define posn4D-t-class
    (values 'Posn4D
            (classT 'Posn
                    (list (values 'z (numT)) (values 'v (numT)))
                    empty)))
  
  (define square-t-class 
    (values 'Square
            (classT 'Object
                    (list (values 'topleft (objT 'Posn)))
                    (list))))

  (define dummy-t-class 
    (values 'Dummy
            (classT 'Object
                    (list)
                    (list))))

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class posn4D-t-class square-t-class dummy-t-class)))
  
  (define new-posn27 (newI 'Posn (list (numI 2) (numI 7))))
  (define new-posn531 (newI 'Posn3D (list (numI 5) (numI 3) (numI 1))))

  (test (typecheck-posn (sendI new-posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI new-posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI new-posn531 'addDist new-posn27))
        (numT))  
  (test (typecheck-posn (sendI new-posn27 'addDist new-posn531))
        (numT))
  (test (typecheck-posn (castI 'Posn (newI 'Posn3D (list (numI 0) (numI 1) (numI 3)))))
        (objT 'Posn))
  (test (typecheck-posn (castI 'Posn3D (newI 'Posn (list (numI 0) (numI 1)))))
        (objT 'Posn3D))
  (test (typecheck-posn (if0I (numI 0) (numI 1) (numI 2)))
        (numT))
  (test (typecheck-posn (if0I (numI 3) (numI 1) (numI 2)))
        (numT))
  (test (typecheck-posn (if0I (numI 0)
                              (newI 'Posn3D (list (numI 0) (numI 1) (numI 3)))
                              (newI 'Posn3D (list (numI 2) (numI 4) (numI 5)))))
        (objT 'Posn3D))
  (test (typecheck-posn (if0I (numI 0)
                              (newI 'Posn3D (list (numI 0) (numI 1) (numI 3)))
                              (newI 'Posn4D (list (numI 4) (numI 2) (numI 4) (numI 5)))))
        (objT 'Posn))
  (test (typecheck-posn (if0I (numI 0)
                              (newI 'Posn3D (list (numI 0) (numI 1) (numI 3)))
                              (newI 'Dummy empty)))
        (objT 'Object))
  (test (typecheck-posn (if0I (numI 0)
                              (newI 'Posn (list (numI 0) (numI 1)))
                              (newI 'Posn3D (list (numI 0) (numI 1) (numI 3)))))
        (objT 'Posn))
  (test (typecheck-posn (if0I (numI 0)
                              (newI 'Posn3D (list (numI 0) (numI 1) (numI 3)))
                              (newI 'Posn (list (numI 0) (numI 1)))))
        (objT 'Posn))
  (test (typecheck-posn (newI 'Square (list (nullI))))
        (objT 'Square))
  (test (typecheck-posn (sendI new-posn531 'addDist (nullI)))
        (numT))
  (test (typecheck-posn (castI 'Posn (nullI)))
        (objT 'Posn))
  (test (typecheck-posn (setI (newI 'Posn (list (numI 0) (numI 1)))
                              'x
                              (numI 2)))
        (objT 'Posn))
  (test (typecheck-posn (setI (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1)))))
                              'topleft
                              (newI 'Posn3D (list (numI 2) (numI 3) (numI 4)))))
        (objT 'Square))

  (test (typecheck-posn (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1))))))
        (objT 'Square))
  (test (typecheck-posn (newI 'Square (list (newI 'Posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'Square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  (test/exn (typecheck-posn (setI (numI 1)
                                  'x
                                  (numI 2)))
            "no type")
  (test/exn (typecheck-posn (setI (nullI)
                                  'x
                                  (numI 1)))
            "no type")
  (test/exn (typecheck-posn (setI (newI 'Posn (list (numI 0) (numI 1)))
                                  'x
                                  (newI 'Posn (list (numI 0) (numI 1)))))
            "no type")
  (test/exn (typecheck-posn (setI (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1)))))
                                  'topleft
                                  (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1)))))))
            "no type")
  (test/exn (typecheck-posn (getI (nullI) 'x))
            "no type")
  (test/exn (typecheck-posn (sendI (nullI) 'm (numI 0)))
            "no type")
  (test/exn (typecheck-posn (if0I (nullI) (numI 1) (numI 2)))
            "no type")
  (test/exn (typecheck-posn (newI 'Posn (list (nullI) (numI 7))))
            "no type")
  (test/exn (typecheck-posn (plusI (nullI) (numI 1)))
            "no type")
  (test/exn (typecheck-posn (plusI (numI 1) (nullI)))
            "no type")
  (test/exn (typecheck-posn (multI (nullI) (numI 1)))
            "no type")
  (test/exn (typecheck-posn (multI (numI 1) (nullI)))
            "no type")
  (test/exn (typecheck-posn (if0I (newI 'Posn (list (numI 0) (numI 1))) (numI 1) (numI 2)))
            "no type")
  (test/exn (typecheck-posn (if0I (numI 1) (numI 2) (newI 'Posn (list (numI 0) (numI 1)))))
            "no type")
  (test/exn (typecheck-posn (if0I (numI 1) (newI 'Posn (list (numI 0) (numI 1))) (numI 2)))
            "no type")
  (test/exn (typecheck-posn (castI 'Square (newI 'Posn (list (numI 0) (numI 1)))))
            "no type")
  (test/exn (typecheck-posn (castI 'Square (numI 0))) 
            "no type")
  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI new-posn27 'mdist new-posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'Object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class
                             (values 'Other
                                     (classT 'Posn
                                             (list)
                                             (list (values 'mdist
                                                           (methodT (objT 'Object) (numT)
                                                                    (numI 10))))))))
            "bad override")
  (test/exn (typecheck-method (methodT (numT) (objT 'Object) (numI 0)) (objT 'Object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (values 'Cube
                                     (classT 'Square
                                             empty
                                             (list
                                              (values 'm
                                                      (methodT (numT) (numT)
                                                               ;; No such method in superclass:
                                                               (superI 'm (numI 0)))))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [(classT super-name fields methods)
       (classI
        super-name
        (map fst fields)
        (map (lambda (m)
               (values (fst m)
                       (type-case MethodT (snd m)
                         [(methodT arg-type result-type body-expr)
                          body-expr])))
             methods))])))
  
(define interp-t : (ExpI (Listof (Symbol * ClassT)) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map (lambda (c)
                     (values (fst c) (strip-types (snd c))))
                   t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI new-posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI new-posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI new-posn531 'addDist new-posn27))
        (numV 18))
  (test (interp-t-posn (sendI new-posn27 'addDist new-posn531))
        (numV 18)))

(define (least-upper-bound class1 class2 t-classes) : Symbol
  (if (eq? class1 class2)
      class1
      (if (eq? class1 'Object)
          'Object
          (if (is-subclass? class2 class1 t-classes)
              class1
              (least-upper-bound (classT-super-name (find t-classes class1))
                                 class2
                                 t-classes)))))

(module+ test
  (test (least-upper-bound 'Object
                           'Object
                           (list posn-t-class posn3D-t-class posn4D-t-class dummy-t-class))
        'Object)
  (test (least-upper-bound 'Posn
                           'Posn3D
                           (list posn-t-class posn3D-t-class posn4D-t-class dummy-t-class))
        'Posn)
  (test (least-upper-bound 'Posn3D
                           'Posn4D
                           (list posn-t-class posn3D-t-class posn4D-t-class dummy-t-class))
        'Posn)
  (test (least-upper-bound 'Posn3D
                           'Dummy
                           (list posn-t-class posn3D-t-class posn4D-t-class dummy-t-class))
        'Object))
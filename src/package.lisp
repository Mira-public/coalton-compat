(defpackage #:coalton-compat
  (:use #:coalton #:coalton-prelude
        #:coalton-compat/macros
        #:coalton-compat/boot
        )
  (:local-nicknames
   (#:class #:coalton-library/classes)
   (#:iterator #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:types #:coalton-library/types)
   (#:macros #:coalton-compat/macros)
   )
  (:shadow any cons car cdr)
  (:export assert-equal
           ;; Primitive Types
           Standard-Object
           Keyword
           Symbol
           Any
           Timestamp
           ;; Cons
           Cons
           car
           cdr
           ;; Alist
           Alist
           assoc
           alist-get
           ;; PList
           Plist
           plist-get
           ;; Unsafe
           unsafe-cast

           ;; Derivations
           define-cast
           define-lisp-function
           define-show
           )
  )
(in-package #:coalton-compat)
;; (cl:delete-package 'coalton-compat)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare try-parse-lisp ((types:RuntimeRepr :a) => types:Proxy :a -> Any -> (Optional :a)))
  (define (try-parse-lisp proxy value)
    (let ((repr (types:runtime-repr proxy)))
      (lisp (Optional :a) (repr value)
        (cl:if (cl:typep value repr)
               (Some value)
               None)))))

;; (cl:describe 'boolean)
;; (cl:defmacro keyword (x))
;; (cl:defun cl-value )

(coalton-toplevel
  (declare assert-equal ((Eq :a) => :a -> :a -> Unit))
  (define (assert-equal a b)
    (unless (== a b)
      (lisp Unit (a b) (cl:error (cl:format nil "~s != ~s" a b)))))
  )

(coalton:coalton-toplevel
  (repr :native cl:keyword)
  (define-type Keyword)
  (define-instance (Eq Keyword)
    (define == unsafe-pointer-eq?))
  (define-instance (Eq Symbol)
    (define == unsafe-pointer-eq?))
  )
(define-lisp-function make-keyword alexandria:make-keyword Keyword :a)
(define-show Keyword)

(coalton:coalton-toplevel
  (repr :native cl:symbol)
  (define-type Symbol)
  )
(define-show Symbol)

(coalton:coalton-toplevel
  ;; Primitives
  (repr :native cl:standard-object)
  (define-type Standard-Object)
  (repr :native cl:t)
  (define-type Any)
  (repr :native cl:number)
  (define-type Timestamp)



  )

;; (coalton (try-parse-lisp (the (types:Proxy Keyword) types:Proxy) (lisp Any () :foo)))
;; (coalton (try-parse-lisp (the (types:Proxy Keyword) types:Proxy) (lisp Any () 'foo)))

(coalton:coalton-toplevel
  (repr :native cl:cons)
  (define-type (Cons :a :b))
  )

(define-lisp-function cons cl:cons (Cons :a :b) :a :b)
(define-lisp-function car cl:car :a (Cons :a :b))
(define-lisp-function cdr cl:cdr :b (Cons :a :b))

(coalton:coalton-toplevel
  (define-instance ((Eq :a) (Eq :b) => (Eq (Cons :a :b)))
    (define (== a b) (and (== (car a) (car b))
                          (== (cdr a) (cdr b))))))

(coalton:coalton-toplevel
  (repr :native cl:list)
  (define-type (AList :a :b)))

(define-cast (AList :a :b) (coalton:List (Cons :a :b)))
(define-cast (coalton:List (Cons :a :b)) (AList :a :b))

(coalton:coalton-toplevel
  (define-instance (Into (Alist :a :b) (coalton:List (Tuple :a :b)))
    (define (into alist)
      (map (fn (pair)
             (Tuple (car pair) (cdr pair)))
           (into alist))))
  (define-instance (Into (coalton:List (Tuple :a :b)) (Alist :a :b))
    (define (into alist)
      (into
       (map (fn ((Tuple a b))
              (cons a b))
            alist)))))

(coalton:coalton-toplevel
  (define-instance (iterator:IntoIterator (Alist :a :b) (Cons :a :b))
    (define (iterator:into-iter alist)
      (match (head (into alist))
        ((None) iterator:empty)
        ((Some x)
         (iterator:into-iter (unify (list:singleton x)
                                    (into alist))))))))

(coalton:coalton-toplevel
  (declare alist-get ((Eq :a) => (Alist :a :b) -> :a -> (Optional :b)))
  (define (alist-get alist key) (map cdr (assoc key alist)))
  (declare assoc ((Eq :a) => :a -> (Alist :a :b) -> (Optional (Cons :a :b))))
  (define (assoc key alist)
    (for pair in alist
      (when (== (car pair) key)
        (return (Some (cons (car pair) (cdr pair))))))
    (return None))


  (define-instance (Into coalton:String Keyword)
    (define into make-keyword))
  )


(coalton-toplevel
  (repr :native cl:list)
  (define-type (PList :a :b))

  (define-instance (Into (PList :a :b) (coalton:List (Tuple :a :b)))
    (define (into plist)
      (lisp (coalton:List (Tuple :a :b)) (plist)
        (cl:loop for (k v) on plist by #'cl:cddr
           collect (Tuple k v)))))
  ;; (coalton (as (List (Tuple :a :b)) (lisp (PList Keyword

  (define-instance (Into (coalton:List (Tuple :a :b)) (PList :a :b))
    (define (into tuples)
      (foldr (fn ((Tuple k v) acc)
               (lisp (PList :a :b) (k v acc)
                 (cl:cons k (cl:cons v acc))))
             (unsafe-cast (make-list))
             tuples)))

  (define-instance (iterator:IntoIterator (PList :a :b) (Tuple :a :b))
    (define (iterator:into-iter plist)
      (iterator:into-iter (as (coalton:List (Tuple :a :b)) plist))))
  ;; (coalton (iterator:into-iter


  (declare plist-get ((Eq :a) => (PList :a :b) -> :a -> (Optional :b)))
  (define (plist-get plist key)
    (map .second
         (iterator:find!
          (fn ((Tuple k _))
            (== k key))
          (iterator:into-iter plist))))
  #|
  (declare plist-get2 ((Eq :a) => (PList :a :b) -> :a -> (Optional :b))) ; ;
  (define (plist-get2 plist key)        ; ;
  (let ((equal (fn (x) (== key x))))    ; ;
  (lisp (Optional :b) (equal plist key) ; ;
  (cl:loop                              ; ;
  for (k v) on plist by #'cl:cddr       ; ;
  cl:when (cl:funcall equal key k)      ; ;
  cl:return (Some v)                    ; ;
  finally (cl:return None)))))          ; ;
  |#
  )
;; (coalton (let ((plist (lisp (Plist Keyword Any) () '(:foo 5 :bar "10")))) (plist-get plist (as Keyword "foo"))))


5 #|
(coalton:coalton-toplevel
  (declare slot-value (Any -> String -> (Optional Any)))
  (define (slot-value obj key)
    (coalton:do
     ()
     )
    )
  )
|#

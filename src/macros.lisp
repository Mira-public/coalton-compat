(defpackage :coalton-compat/macros
  (:use :cl)
  (:export define-show
           define-cast
           define-lisp-function
           )
  (:local-nicknames
   (#:class #:coalton-library/classes)
   )
  )
(in-package :coalton-compat/macros)
;; (delete-package 'coalton-compat/macros)

(defmacro define-show (t-new)
  `(coalton:coalton-toplevel
    (coalton:define-instance (class:Into ,t-new coalton:String)
      (coalton:define (class:into x)
        (coalton:lisp coalton:String (x)
          (format nil "~a" x))))))

(defmacro define-cast (t0 t1)
  `(coalton:coalton-toplevel
     (coalton:define-instance (class:Into ,t0 ,t1)
       (coalton:define class:into coalton-compat/boot:unsafe-cast)))
  )

(defun make-coalton-function-type (t-out t-ins)
  (if (null t-ins)
      (list t-out)
      `(,(car t-ins) coalton:-> ,@(make-coalton-function-type t-out (cdr t-ins)))
      ))
;; (make-coalton-function-type 'Foo '(Bar Baz Qux))

(defmacro define-lisp-function (symbol-here symbol t-out &rest t-ins)
  (flet ((make-fv (x)
           (gensym
            (symbol-name
             (etypecase x
               (symbol x)
               (list (car x)))))))
    (let* ((fvs (mapcar #'make-fv t-ins)))
      `(coalton:coalton-toplevel
         (coalton:declare ,symbol-here ,(make-coalton-function-type t-out t-ins))
         (coalton:define (,symbol-here ,@fvs)
           (coalton:lisp ,t-out (,@fvs)
             (,symbol ,@fvs)))))))
;; (define-lisp-function car cl:car :a (Cons :a :b))
;; (define-lisp-function cdr cl:cdr :b (Cons :a :b))
;; (defmacro define-eq (t0 cl-predicate))

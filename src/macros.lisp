(defpackage :coalton-compat/macros
  (:use :cl)
  (:export define-show
           define-cast
           define-lisp-function
           define-eq
           add-coalton-nicknames
           )
  (:import-from #:sb-ext add-package-local-nickname)
  )
(in-package :coalton-compat/macros)
;; (delete-package 'coalton-compat/macros)

;; (sb-ext:add-package-local-nickname '#:big-float (find-package '#:coalton-library/types))

(defmacro add-coalton-nicknames ()
  (let ((nicks '(;;(#:big-float . #:coalton-library/big-float)
                 (#:bits . #:coalton-library/bits)
                 (#:builtin . #:coalton-library/builtin)
                 (#:cell . #:coalton-library/cell)
                 (#:char . #:coalton-library/char)
                 (#:class . #:coalton-library/classes)
                 (#:classes . #:coalton-library/classes)
                 ;;(#:computable-reals . #:coalton-library/computable-reals)
                 (#:file . #:coalton-library/file)
                 (#:functions . #:coalton-library/functions)
                 (#:hash . #:coalton-library/hash)
                 (#:hasht . #:coalton-library/hashtable)
                 (#:hashtable . #:coalton-library/hashtable)
                 (#:iterator . #:coalton-library/iterator)
                 (#:lisparray . #:coalton-library/lisparray)
                 (#:list . #:coalton-library/list)
                 (#:arith . #:coalton-library/math/arith)
                 (#:bounded . #:coalton-library/math/bounded)
                 (#:complex . #:coalton-library/math/complex)
                 (#:dual . #:coalton-library/math/dual)
                 (#:dyadic . #:coalton-library/math/dyadic)
                 (#:elementary . #:coalton-library/math/elementary)
                 (#:fraction . #:coalton-library/math/fraction)
                 (#:integral . #:coalton-library/math/integral)
                 (#:real . #:coalton-library/math/real)
                 (#:free . #:coalton-library/monad/free)
                 (#:state . #:coalton-library/monad/state)
                 (#:optional . #:coalton-library/optional)
                 (#:ord-map . #:coalton-library/ord-map)
                 (#:ord-tree . #:coalton-library/ord-tree)
                 (#:queue . #:coalton-library/queue)
                 (#:randomaccess . #:coalton-library/randomaccess)
                 (#:result . #:coalton-library/result)
                 (#:seq . #:coalton-library/seq)
                 (#:slice . #:coalton-library/slice)
                 (#:string . #:coalton-library/string)
                 (#:system . #:coalton-library/system)
                 (#:tuple . #:coalton-library/tuple)
                 (#:types . #:coalton-library/types)
                 (#:vector . #:coalton-library/vector))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;;(add-coalton-nicknames)
       (dolist (nick ',nicks)
         (alexandria:when-let ((pkg (find-package (cdr nick))))
           (sb-ext:add-package-local-nickname (car nick) pkg))))))
(add-coalton-nicknames)

(defmacro define-show (t-new)
  `(coalton:coalton-toplevel
     (coalton:define-instance (class:Into ,t-new coalton:String)
       (coalton:define (class:into x)
         (coalton:lisp coalton:String (x)
           (format nil "~a" x))))))
;; The macro above is supposed to make local nicknames. But the error indicates the nickname didn't stick. I'm just compile + load this whole file.
;; So how do we ensure the nickname is establish during a compile?

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

(defmacro define-eq (type func)
  `(coalton:coalton-toplevel
     (coalton:define-instance (class:Eq ,type)
       (coalton:define class:== ,func))))

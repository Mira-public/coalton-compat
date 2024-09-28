(uiop:define-package :coalton-compat/boot
  (:use #:coalton #:coalton-prelude)
  (:export unsafe-cast
           unify
           )
  )
(in-package :coalton-compat/boot)

(coalton:coalton-toplevel
  (declare unsafe-cast (:a -> :b))
  (define (unsafe-cast x)
    (lisp :b (x) (cl:eval `(coalton ,x))))

  (declare unify (:a -> :a -> :a))
  (define (unify _ x) x)
  )

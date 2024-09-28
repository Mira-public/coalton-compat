(uiop:define-package :coalton-compat/haskell
  (:use #:coalton #:coalton-prelude)
  (:export on)
  )
(in-package :coalton-compat/haskell)

;; Anything stolen from the Haskell ecosystem

(coalton-toplevel
  (declare on ((:b -> :b -> :c) -> (:a -> :b) -> :a -> :a -> :c))
  (define (on g f a b) (g (f a) (f b)))
  )

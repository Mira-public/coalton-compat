(asdf:defsystem #:coalton-compat
  :description "Common Lisp compatibility types & functions for Coalton"
  :author "Mira"
  :version "0.0.1"
  :serial t
  :depends-on (#:coalton)
  :components ((:module "src"
                :components ((:file "boot")
                             (:file "haskell")
                             (:file "macros")
                             (:file "package")))
                )
  :in-order-to ((asdf:test-op (asdf:test-op :coalton-compat/tests)))
  )
(asdf:defsystem #:coalton-compat/tests
  :serial t
  :depends-on (#:coalton-compat #:parachute)
  :components ((:module "tests"
                :components ((:file "package")))
                )
  :perform (asdf:test-op (o c) (uiop:symbol-call :parachute :test :coalton-compat/tests))
  )

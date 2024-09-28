(defpackage :coalton-compat/tests
  (:use #:parachute #:cl
        )
  (:local-nicknames
   (#:compat #:coalton-compat)
   (#:iterator #:coalton-library/iterator)
   )
  (:export)
  )
(in-package #:coalton-compat/tests)

(named-readtables:in-readtable coalton:coalton)

(defmacro is-type (Type expr) `(parachute:true (coalton:coalton (coalton:lisp ,Type () ,expr))))
(defmacro is-not-type (Type expr)
  `(parachute:fail (coalton:coalton (coalton:lisp ,Type () ,expr)) 'cl:type-error))
;; (parachute:fail (coalton:coalton (coalton:lisp ,Type () ,expr)) 'cl:simple-type-error)
;; (is-type coalton-compat:Keyword 5)
;; (is-type coalton-compat:Keyword :foo)
;; (is-not-type coalton-compat:Keyword 5)
;; (is-not-type coalton-compat:Keyword :foo)

(define-test cl-types
  :compile-at :execute
  (is-type compat:Keyword :foo)
  (is-not-type compat:Keyword 5)
  (is-type compat:Symbol 'foo)
  (is-not-type compat:Keyword 'foo)
  (is-type (coalton-compat:Cons coalton-compat:Keyword coalton:Integer) (cl:cons :foo 5)))

(define-test cons
  (coalton:coalton
   (coalton:let ((pair (coalton-compat:cons (coalton:as compat:Keyword "key") 42)))
     (compat:assert-equal (coalton:as compat:Keyword "key") (compat:car pair))
     (compat:assert-equal 42 (compat:cdr pair)))))
;; (test 'cons :report 'interactive)
5 #|
(define-test alist
  (let ((kvs '((:a . 1) (:b . 2) (:c . 3)))
        (k :b)
        )
    (coalton:coalton
     (assert-equal (coalton-prelude:Some (coalton-compat:cons k 2))
                   (coalton-compat:assoc k kvs))
     (assert-equal None (assoc (as Keyword "d" kvs)))
     ))
  )
|#
5                                       #|
    (is = (coalton:Some (coalton-compat:cons :b 2))
        (coalton:coalton (coalton-compat:assoc :b alist)))
    (is = coalton:None
        (coalton:coalton (coalton-compat:assoc :d alist)))

    (let ((items (coalton:coalton
                  (coalton-library/iterator:collect
                      (coalton-library/iterator:into-iter alist)))))
      (is = 3 (coalton:coalton (coalton:lisp coalton:Integer () (coalton:length items))))
      (is = '((:a . 1) (:b . 2) (:c . 3))
          (coalton:coalton (coalton:lisp cl:list () items))))
|#

(define-test plist
  (coalton:coalton
   (coalton:let ((plist (coalton:lisp (compat:PList compat:Keyword compat:Integer) () '(:foo 5 :bar "10"))))
     (iterator:into-iter plist)
     (compat:assert-equal (coalton-prelude:Some 5)
                          (compat:plist-get plist (coalton:as compat:Keyword "FOO")))
     )))
;; (test 'plist :report 'interactive)

;; (is-not-type (coalton-compat:Cons coalton-compat:Keyword coalton-compat:Keyword) (cl:cons :foo 5)) ;; TODO: Incorrectly-accepted

;;(is-not-type coalton-compat:Keyword :foo)

;; (test 'cl-types :report 'interactive)
;; (lisp--run-asdf-tests)

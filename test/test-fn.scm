;;;; Tests of anonymous functions

(run-tests "Lambda"

;;;; Type checking

(assert-equal (type (fn ())) 'procedure     "Type of anonymous function is procedure")

;;;; Scoping of def/set

(do (def x 1)
    (def f (n) (= x n))
    (assert-equal x 1       "Value of x should be 1 (set scoping)")
    (f 2)
    (assert-equal x 2       "Value of x should be 2 (set scoping)"))

(do (def x 1)
    (def f (n) (def x n))
    (assert-equal x 1       "Value of x should be 1 (def scoping)")
    (f 2)
    (assert-equal x 1       "Value of x should still be 1 (def scoping)"))

;;;; Scope of set-car

(do (def x (list 1 2))
    (def f (n)
      (= (car x) n))
    (assert-equal (car x) 1 "Car of x should be 1 (set-car scoping)")
    (f 2)
    (assert-equal (car x) 2 "Car of x should be 2 (set-car scoping)"))

;;;; Scope of set-cdr

(do (def x (list 1 2))
    (def f (n)
      (= (cdr x) (list n)))
    (assert-equal (cdr x) (list 2) "Cdr of x should be (2) (set-car scoping)")
    (f 3)
    (assert-equal (cdr x) (list 3) "Cdr of x should be (3) (set-car scoping)"))

)
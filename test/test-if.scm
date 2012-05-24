;;;; Tests for if statements

(run-tests "If"

;;;; Test truth/falsity values for simple if

(assert-equal (if #t 1 0) 1         "#t has truth-value #t")
(assert-equal (if #f 1 0) 0         "#f has truth-value #f")

(assert-equal (if 1 1 0) 1          "1 has truth-value #t")
(assert-equal (if 0 1 0) 0          "0 has truth-value #f")

(assert-equal (if '(1) 1 0) 1       "Non-empty list is true")
(assert-equal (if nil 1 0) 0        "Empty list is false")

(assert-equal (if $(1) 1 0) 1       "Non-empty vector is true")
(assert-equal (if $() 1 0) 0        "Empty vector is false")

(assert-equal (if #(a 1) 1 0) 1     "Non-empty hash is true")
(assert-equal (if #() 1 0) 0        "Empty hash is false")

;;;; Test advanced if/elseif usage

(do (def x (if 1 "foo"
               2 "bar" "baz"))
    (assert-equal x "foo"           "If statement assigns foo to x"))

(do (def x (if 0 "foo"
               1 "bar" "baz"))
    (assert-equal x "bar"           "If statement assigns bar to x"))

(do (def x (if 0 "foo"
               0 "bar" "baz"))
    (assert-equal x "baz"           "If statement assigns baz to x"))

(do (def x (if 0 "foo"))
    (assert-equal x nil             "If statement assigns nil to x"))

;;;; Test it-bindings

(assert-equal (if 1 it 0) 1         "it bound to 1 inside if statement")
(assert-equal (if 0 1 it) 0         "it bound to 0 inside if statement")

;;;; Test scoping for definitions

(do (if #t 1 0)
    (assert-unbound it              "it should be unbound outside if statement"))

(do (if #t (def x 1))
    (assert-bound x                 "x should be bound outside if statement")
    (assert-equal x 1               "Value of x should be 1"))

(do (if #t (def it 1))
    (assert-bound it                "it should now be bound")
    (assert-equal it 1              "Value of it should be 1"))

)
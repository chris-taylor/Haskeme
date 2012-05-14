;;;; List tests

(run-tests "List"

;;;; Type checking

(assert-equal (type nil) 'nil         "Type of nil is nil")
(assert-equal (type '(1 2 3)) 'pair   "Type of list is pair")

(assert (isa '() 'nil)                "Nil is a nil")
(assert (isa '(1 2 3) 'pair)          "List is a pair")

(assert (list? '())                   "Nil is a list")
(assert (list? '(1 2 3))              "List is a list")

;;;; Cons

(assert-equal (cons 1 nil) '(1)             "Cons onto nil")
(assert-equal (cons 1 '(2)) '(1 2)          "Cons onto a list")
(assert-equal (cons 1 '(2 . 3)) '(1 2 . 3)  "Cons onto a dotted list")

(assert-equal (cons 1 $()) $(1)             "Cons onto empty vector")
(assert-equal (cons 1 $(2)) $(1 2)          "Cons onto vector")

(assert-equal (cons #\a "") "a"             "Cons onto empty string")
(assert-equal (cons #\a "b") "ab"           "Cons onto string")

;;;; Car

(assert-equal (car '(1)) 1                  "Car of a list (1)")
(assert-equal (car '(1 2)) 1                "Car of a list (2)")

(assert-equal (car '(1 . 2)) 1              "Car of a dotted pair (1)")
(assert-equal (car '(1 . (2 . 3))) 1        "Car of a dotted pair (2)")

(assert-equal (car $(1)) 1                  "Car of a vector (1)")
(assert-equal (car $(1 2)) 1                "Car of a vector (2)")

(assert-equal (car "a") #\a                 "Car of a string (1)")
(assert-equal (car "ab") #\a                "Car of a string (2)")

)
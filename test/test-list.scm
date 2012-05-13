;;;; List tests

(run-tests "List"

(assert-equal (type nil) 'nil         "Type of nil is nil")
(assert-equal (type '(1 2 3)) 'pair   "Type of list is pair")

(assert (isa nil nil)         "Nil is a nil")
(assert (isa '(1 2 3) 'list)) "List is a list")




)
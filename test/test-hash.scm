;;;; Tests for hashes

(run-tests "Hash"

(do (def m #(a 1 b 2))
    (assert-equal (m 'a) 1  "Test map lookup (1)")
    (assert-equal (m 'b) 2  "Test map lookup (2)"))

(do (def m #())
    (= (m 'c) 3)
    (assert-equal (m 'c) 3  "Test map lookup (3)"))

(do (def m #(a 1 b 2))
    (def a (hash->alist m))
    (assert-equal a '((a 1) (b 2))  "Test hash -> alist conversion"))

(do (def a '((a 1) (b 2)))
    (def m (alist->hash a))
    (assert-equal m #(a 1 b 2)      "Test alist -> hash conversion"))

)
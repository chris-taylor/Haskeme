;;;; Numeric test-suite

(run-tests "Numeric"

;;;; Type checking

(assert (number? 1)     "1 is a number")
(assert (integer? 1)    "1 is integral")
(assert (rational? 1)   "1 is rational (1/1)")
(assert (real? 1)       "1 is real (1.0)")
(assert (complex? 1)    "1 is complex (1+0i)")

(assert (number? 1/2)   "1/2 is a number")
(assert (rational? 1/2) "1/2 is rational")
(assert (real? 1/2)     "1/2 is real (0.5)")
(assert (complex? 1/2)  "1/2 is complex (0.5+0i)")

(assert (number? 1.0)   "1.0 is a number")
(assert (real? 1.0)     "1.0 is real")
(assert (complex? 1.0)  "1.0 is complex (1.0+0i)")

(assert (number? 1+1i)  "1+1i is a number")
(assert (complex? 1+1i) "1+1i is complex")

;;;; Exactness

(assert (exact? 1)      "1 is exact")
(assert (exact? 1/2)    "1/2 is exact")
(assert (inexact? 1.0)  "1.0 is inexact")
(assert (inexact? 1+1i) "1+1i is inexact")

(assert-false (inexact? 1)      "1 is exact")
(assert-false (inexact? 1/2)    "1/2 is exact")
(assert-false (exact? 1.0)      "1.0 is inexact")
(assert-false (exact? 1+1i)     "1+1i is inexact")

(assert (exact? (as-exact 1.0))     "Convert inexact real to exact")
(assert (exact? (as-exact 1+2i))    "Convert inexact complex to exact")
(assert (inexact? (as-inexact 2))   "Convert exact integer to inexact")
(assert (inexact? (as-inexact 3/2)) "Convert exact rational to inexact")

;;;; Equality and inequality

(assert (== 1 1)        "1 should equal 1")
(assert (== 1.0 1.0)    "1.0 should equal 1.0")
(assert (== 1 1.0)      "1 should equal 1.0")
(assert (== 1 2/2)      "1 should equal 2/2")
(assert (== 5.0 5+0i)   "5.0 should equal 5+0i")

(assert (< 1 2))
(assert (< 1 2.0))
(assert (< 1 2 3 4))
(assert-false (< 1 2 2 3))
(assert-false (< 1 2 3 2))

(assert (<= 1 2.0))
(assert (<= 1 1.0))
(assert (<= 1 2 2 3 3 4))
(assert-false (<= 1 2 3 2 1))

(assert (> 2 1))
(assert (> 3.0 2))
(assert (> 5 4 3 2 1))
(assert-false (> 3 2 2 1))
(assert-false (> 3 2 1 2))

(assert (>= 3.0 2))
(assert (>= 1.0 1.0))
(assert (>= 3 2.0 1))
(assert-false (>= 3 2.0 1 2))

;;;; Test addition

(assert-equal 2 (+ 1 1)         "Test integer addition")
(assert-equal 2 (+ 3 -1)        "Addition of negative numbers")

(assert-equal 5/2 (+ 1 3/2)     "Add integers to rationals")
(assert-equal 1/2 (+ 2 -3/2)    "Adding negative rationals")
(assert-equal 1 (+ 1/3 2/3)     "Rationals correctly pushed down to integers")

(assert-equal 2.3 (+ 1.1 1.2)   "Test floating point addition")
(assert-equal 3.0 (+ 1.5 3/2)   "Add reals to rationals")
(assert-equal 2.5 (+ 1.5 1)     "Add reals to integers")

(assert-equal 1+1i (+ 1+0i 0+1i)    "Test complex addition")
(assert-equal 2+1i (+ 1.0 1+1i)     "Add complex to real")
(assert-equal 2+1i (+ 1 1+1i)       "Add complex to integer")

(assert (exact? (+ 1 1))        "Adding exact number preserves exactness")
(assert (inexact? (+ 2 1.5))    "Inexactness contaminates exactness")
(assert (inexact? (+ 1.0 1.0))  "Can't get exact answers from inexact arguments")

)
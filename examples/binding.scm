(def (binding-test x)
    (with ((f x) (+ 1 (g x))
           (g x) (+ 1 x))
        (f x)))

(binding-test 1)
(def (binding-test x)
    (with (f [+ 1 (g _)])
          (g [+ 1 _])
        (f x)))

(binding-test 1)
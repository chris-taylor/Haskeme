(def pascal (n k)
    (if (is k 0) 1
        (is k n) 1
        (+ (pascal (- n 1) (- k 1))
           (pascal (- n 1) k))))
(def sqrt-abs (x)
    (def improve (guess)
        (average guess (/ x guess)))
    (def good-enough (guess)
        (< (abs (- x (* guess guess))) 0.0001))
    (def iter (guess)
        (if (good-enough guess) guess
            (iter (improve guess))))
    (iter 1.0))

(def sqrt-rel (x)
    (def improve (guess)
        (average guess (/ x guess)))
    (def good-enough (g1 g2)
        (< (abs (- (/ g1 g2) 1)) 0.0001))
    (def iter (g1 g2)
        (if (good-enough g1 g2) g1
            (iter (improve g1) g1)))
    (iter 1.0 2.0))

(def average (a b) (/ (+ a b) 2))
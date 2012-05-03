(def inc (x) (+ x 1))
(def dec (x) (- x 1))

(def add (a b)
    (if (is a 0) b
        (inc (add (dec a) b))))

(def add* (a b)
    (if (is a 0) b
        (add* (dec a) (inc b))))
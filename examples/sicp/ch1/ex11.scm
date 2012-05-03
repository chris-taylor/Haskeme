(def foo (n)
    (if (< n 3) n
        (+ (foo (- n 1))
           (* 2 (foo (- n 2)))
           (* 3 (foo (- n 3))))))

(def foo* (n)
    (def iter (a b c i)
        (if (is i n) a
            (iter (+ a (* 2 b) (* 3 c))
                  a b (+ i 1))))
    (if (< n 3) n
        (iter 2 1 0 2)))
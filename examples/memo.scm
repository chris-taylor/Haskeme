;; Naive code to compute the nth Fibonacci number
(def fib (n)
  (if (in n 0 1) 1
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; Computes the nth Fibonanni number using an iterative process
(def fib-iter (n)
  (def iter (a b count)
    (if (is count n) a
        (iter (+ a b) a (+ count 1))))
  (iter 1 1 1))

;; Identical to the first definition - exists to provide a comparison
(def fib-memo (n)
  (if (in n 0 1) 1
      (+ (fib-memo (- n 1))
         (fib-memo (- n 2)))))

(memoize fib-memo)

;; Load this file into the evaluator and and compare timings
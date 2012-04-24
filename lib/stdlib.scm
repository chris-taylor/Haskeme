(def (not x)
    (if x #f #t))

(def (null? obj)
    (is obj '()))

(def (list . objs)
    objs)

(def (id obj)
    obj)

(def (flip func)
    (fn (x y) (func y x)))

(def (curry func x)
    (fn (y) (apply func (list x y))))

(def (compose f g)
    (fn (x) (f (g x))))

(def zero?
    (curry == 0))

(def positive?
    (curry < 0))

(def negative?
    (curry > 0))

(def (odd? num)
    (== (mod num 2) 1))

(def (even? num)
    (== (mod num 2) 0))

(def (foldr func end lst)
    (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))

(def (foldl func accum lst)
    (if (null? lst)
        accum
        (foldl func (func accum (car lst)) (cdr lst))))

(def fold foldl)

(def reduce fold)

(def (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

(def (sum . lst)
    (fold + 0 lst))

(def (product . lst)
    (fold * 1 lst))

(def (and . lst)
    (fold && #t lst))

(def (or . lst)
    (fold || #f lst))

(def (max first . rest)
    (fold (fn (old new)
        (if (> old new) old new)) first rest))

(def (min first . rest)
    (fold (fn (old new)
        (if (< old new) old new)) first rest))

(def (length lst)
    (fold (fn (x y) (+ x 1)) 0 lst))

(def (reverse lst)
    (fold (flip cons) '() lst))

(def (map func lst)
    (foldr (fn (x y) (cons (func x) y)) '() lst))

(def (filter pred lst)
    (foldr (fn (x y)
        (if (pred x)
            (cons x y)
            y)) '() lst))

(def (caar x)
    (car (car x)))

(def (cadr x)
    (car (cdr x)))

(def (cdar x)
    (cdr (car x)))

(def (cddr x)
    (cdr (cdr x)))

(def (replicate n val)
    (if (is n 0) '()
        (cons val (replicate (- n 1) val))))

(def (make-vector n val)
    (apply vector (replicate n val)))

(def (xrange a b)
    (if (>= a b) '()
        (cons a (xrange (+ 1 a) b))))

(def (range a . b)
    (if (null? b)
        (xrange 0 a)
        (xrange a (car b))))

(def (snoc x lst)
    (if (null? lst)
        (list x)
        (cons (car lst) (snoc x (cdr lst)))))

(def (append a b)
    (if (null? b)
        a
        (append (snoc (car b) a) (cdr b))))

(macro (when test . body)
    `(if ,test (do ,@body)))

(macro (unless test . body)
    `(if (not ,test) (do ,@body)))

(macro (pop lst)
    `(do
        (def res (car ,lst))
        (= ,lst (cdr ,lst))
        res))

(macro (push val lst)
    `(do
        (= ,lst (cons ,val ,lst))
        'ok))
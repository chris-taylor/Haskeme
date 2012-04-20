(def (not x)
    (if x #f #t))

(def (null? obj)
    (if (eqv? obj '()) #t #f))

(def (list . objs)
    objs)

(def (id obj)
    obj)

(def (flip func)
    (fn (x y) (func y x)))

(def (curry func x)
    (fn (y) (func (list x y))))

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
    (foldr (fn (x y) (cons (apply func x) y)) '() lst))

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
    (if (= n 0) '()
        (cons val (replicate (- n 1) val))))

(def (make-vector n val)
    (apply vector (replicate n val)))

(def (range a . b)
    (if (eq? b '())
        (range 0 a)
        (if (>= a b) '()
            (cons a (range (+ 1 a) b)))))
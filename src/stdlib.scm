(define (not x)
    (if x #f #t))

(define (null? obj)
    (if (eqv? obj '()) #t #f))

(define (list . objs)
    objs)

(define (id obj)
    obj)

(define (flip func)
    (lambda (x y) (func y x)))

(define (curry func x)
    (lambda (y) (func (list x y))))

(define (compose f g)
    (lambda (x) (f (g x))))

(define zero?
    (curry = 0))

(define positive?
    (curry < 0))

(define negative?
    (curry > 0))

(define (odd? num)
    (= (mod num 2) 1))

(define (even? num)
    (= (mod num 2) 0))

(define (foldr func end lst)
    (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
    (if (null? lst)
        accum
        (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)

(define reduce fold)

(define (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

(define (sum . lst)
    (fold + 0 lst))

(define (product . lst)
    (fold * 1 lst))

(define (and . lst)
    (fold && #t lst))

(define (or . lst)
    (fold || #f lst))

(define (max first . rest)
    (fold (lambda (old new)
        (if (> old new) old new)) first rest))

(define (min first . rest)
    (fold (lambda (old new)
        (if (< old new) old new)) first rest))

(define (length lst)
    (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst)
    (fold (flip cons) '() lst))

(define (map func lst)
    (foldr (lambda (x y) (cons (func x) y)) '() lst))

(define (filter pred lst)
    (foldr (lambda (x y)
        (if (pred x)
            (cons x y)
            y)) '() lst))

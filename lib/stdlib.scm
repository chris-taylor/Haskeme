; NIL returns the empty list

(def nil '())

; NOT is unary negation of values

(def (not x)
    (if x #f #t))

; NULL returns true if the object is the empty list

(def (null obj)
    (is obj nil))

; LIST returns a list containing all of its arguments

(def (list . objs)
    objs)

; ID is the identity function

(def (id obj)
    obj)

; FLIP returns a function show arguments are flipped as compared to the input
; function

(def (flip func)
    (fn (x y) (func y x)))

; CURRY binds the first argument of a two-argument function. The result is a
; function of one argument.

(def (curry func x)
    (fn y (apply func (cons x y))))

; UNCURRY accepts a function that returns another function when given an
; argument, and returns a function that can be called on all parameters at once.

(def (uncurry func)
    (fn (x . rest) (apply (func x) rest)))

; COMPOSE is function composition - the result is a function which applies g,
; then applies f.

(def (compose f g)
    (fn (x) (f (g x))))

;;;; Numeric functions

; ZERO, POSITIVE, NEGATIVE, ODD and EVEN are self-explanatory.

(def zero
    (curry == 0))

(def positive
    (curry < 0))

(def negative
    (curry > 0))

(def (odd num)
    (== (mod num 2) 1))

(def (even num)
    (== (mod num 2) 0))

;;;; Higher-order functions

; FOLDR and FOLDL perform left and right folds with an initial value
; FOLD and REDUCE are synonyms for FOLDL

(def (foldr func end lst)
    (if (null lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))

(def (foldl func accum lst)
    (if (null lst)
        accum
        (foldl func (func accum (car lst)) (cdr lst))))

(def fold foldl)

(def reduce fold)

; UNFOLD takes an initial value and an argument, and keeps applying the function
; to previous result until the predicate is satisfied. It returns a list of
; all result generated.

(def (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

; MAP applies a function to all of the elements of a list.

(def (map func lst)
    (foldr (fn (x y) (cons (func x) y)) '() lst))

; KEEP returns a list containing only the elements that satisfy the predicate.
; REMOVE returns a list without the elements that satisfy the given predicate.

(def (keep pred lst)
    (if (procedure? pred)
        (foldr (fn (x y)
            (if (pred x)
                (cons x y)
                y)) '() lst)
        (keep [is _ pred] lst)))

(def (remove pred lst)
    (if (procedure? pred)
        (keep ~pred lst)
        (keep [not (is _ pred)] lst)))

; SUM and PRODUCT of a list of numbers.

(def (sum . lst)
    (fold + 0 lst))

(def (product . lst)
    (fold * 1 lst))

; AND returns true if all its arguments are true, else it returns false
; #TODO: this should short-circuit evaluation.

(def (and . lst)
    (fold && #t lst))

; OR returns true if at least one of its arguments is true
; #TODO: this should short-circuit evaluation.

(def (or . lst)
    (fold || #f lst))

; MAX and MIN return the maximum/minimum of their arguments.

(def (max first . rest)
    (fold (fn (old new)
        (if (> old new) old new)) first rest))

(def (min first . rest)
    (fold (fn (old new)
        (if (< old new) old new)) first rest))

; REVERSE returns a list containing the elements of its input list, with
; order reversed.

(def (reverse lst)
    (fold (flip cons) '() lst))

; CAAR, CADR, CDAR and CDDR are combinations of CAR and CDR.

(def (caar x)
    (car (car x)))

(def (cadr x)
    (car (cdr x)))

(def (cdar x)
    (cdr (car x)))

(def (cddr x)
    (cdr (cdr x)))

; REPLICATE creates a list containing n copies of val.

(def (replicate n val)
    (if (is n 0) '()
        (cons val (replicate (- n 1) val))))

; MAKE-VECTOR is the same as REPLICATE, but returns a vector instead of a list.

(def (make-vector n val)
    (apply vector (replicate n val)))

; XRANGE returns the numbers [a, a+1 ... b-2, b-1]
; RANGE is a variant whose second argument is optional - when only one argument
; is given, it returns [0, 1 ... a-2, a-1]

(def (xrange a b)
    (if (>= a b) '()
        (cons a (xrange (+ 1 a) b))))

(def (range a . b)
    (if (null b)
        (xrange 0 a)
        (xrange a (car b))))

; SNOC is like cons, but appends elements to the end of a list, string or vector
; rather than the start

(def (snoc x lst)
    (if (null lst)
        (list x)
        (cons (car lst) (snoc x (cdr lst)))))

; APPEND appends the list, string or vector b to the end of a

(def (append a b)
    (if (null b)
        a
        (append (snoc (car b) a) (cdr b))))

;;;; Macro definitions

; WHEN executes a list of statements only if the test returns a non-false value
; UNLESS executes the statements only if the test returns false

(macro (when test . body)
    `(if ,test (do ,@body)))

(macro (unless test . body)
    `(if (not ,test) (do ,@body)))

; PUSH and POP treat a list as a stack, appending or removing elements from
; the front. Note that because they are implemented using =, you get the
; same flexibility as with =, so you can write:
;   haskeme> (def x '(1 3 4)) ==> (1 3 4)
;   haskeme> (push 2 (cdr x)) ==> 'ok
;   haskeme> x                ==> (1 2 3 4)

(macro (pop lst)
    `(let res (car ,lst)
        (do (= ,lst (cdr ,lst))
            res)))

(macro (push val lst)
    `(do (= ,lst (cons ,val ,lst))
         'ok))

; REPEAT executes a block of code n times, returning the final value

(macro (repeat n block)
    `(if (== ,n 1) ,block
         (do ,block
             (repeat (- ,n 1) ,block))))

;;;; INCREMENT (++) and DECREMENT (--)
; These modify their argument, and return the modified result

; As with PUSH and POP, these are implemented using =, so they can reach inside
; structures. For example:
;   haskeme> (def m #(a 1 b 2)) ==> #(a 1 b 2)
;   haskeme> (++ (m 'b))        ==> 3
;   haskeme> m                  ==> #(a 1 b 3)

(macro (++ x)
    `(do
        (= ,x (+ ,x 1))
        ,x))

(macro (-- x)
    `(do
        (= ,x (- ,x 1))
        ,x))

(macro (zap func x)
    `(let res (,func ,x)
        (do (= ,x res)
            res)))
; Combinations of CAR and CDR.

(def (caar x) (car (car x)))
(def (cadr x) (car (cdr x)))
(def (cdar x) (cdr (car x)))
(def (cddr x) (cdr (cdr x)))
(def (caaar x) (car (caar x)))
(def (caadr x) (car (cadr x)))
(def (cadar x) (car (cdar x)))
(def (caddr x) (car (cddr x)))
(def (cdaar x) (cdr (caar x)))
(def (cdadr x) (cdr (cadr x)))
(def (cddar x) (cdr (cdar x)))
(def (cdddr x) (cdr (cddr x)))

; NIL returns the empty list

(def nil '())

; Various tests for negatives:
; NO   returns true only for the empty list
; NULL returns true for any empty data structure (list, string, vector, hash)
; NOT  returns the boolean negation of its argument

(def (no obj) (is obj nil))

(def (null obj)
    (or (is obj nil)
        (is obj "")
        (is obj $())
        (is obj #())))

(def (not x)
    (if x #f #t))

; LIST returns a list containing all of its arguments

(def (list . objs) objs)

(def (alist obj)
    (or (is obj nil)
        (is (type obj) 'pair)))

; ID is the identity function

(def (id obj) obj)

; COMPLEMENT returns the boolean complement of a function.
; The syntax ~func is desugared into (complement func)

(def (complement f)
    (fn args (not (apply f args))))

; RFN Generates recursive lambdas by taking an additional parameter to use as
; the name of the generated lambda.
; AFN Works the same, but automatically binds the symbol `self to the lambda

(macro (rfn name params . body)
    `(let ,name nil
        (= ,name (fn ,params ,@body))))

(macro (afn params . body)
    `(let self nil
        (= self (fn ,params ,@body))))

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

(def zero [== _ 0])

(def positive [> _ 0])

(def negative [< _ 0])

(def odd [== (mod _ 2) 1])

(def even [== (mod _ 2) 0])

;;;; Higher-order functions

; FOLDR and FOLDL perform left and right folds with an initial value
; FOLDR1 and FOLDL1 use the car of the list as the initial value
; FOLD and REDUCE are synonyms for FOLDL

(def (foldr func end lst)
    (if (null lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))

(def (foldl func accum lst)
    (if (null lst)
        accum
        (foldl func (func accum (car lst)) (cdr lst))))

(def (foldr1 func lst)
    (foldr func (car lst) (cdr lst)))

(def (foldl1 func lst)
    (foldl func (car lst) (cdr lst)))

(def fold foldl)
(def reduce fold)

(def fold1 foldl1)
(def reduce1 fold1)

; SCANR and SCANL perform as fold, but return a list of the intermediate results.
; SCANR1 and SCANL1 use the car of the lst as the initial value
; SCAN is a synonym for SCANL

(def (scanr func end lst)
    (if (null lst)
        (list end)
        (let result (scanr func end (cdr lst))
            (cons (func (car lst) (car result))
                  result))))

(def (scanl func result lst)
    (if (null lst)
        (list result)
        (cons result
              (scanl func
                     (func (car lst) result)
                     (cdr lst)))))

(def (scanr1 func lst)
    (scanr func (car lst) (cdr lst)))

(def (scanl1 func lst)
    (scanl func (car lst) (cdr lst)))

(def scan scanl)
(def scan1 scanl1)

; UNFOLD takes an initial value and an argument, and keeps applying the function
; to previous result until the predicate is satisfied. It returns a list of
; all result generated.

(def (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

; MAP1 applies a unary function to each element of xs

(def (map1 func xs)
    (if (no xs)
        '()
        (cons (func (car xs)) (map1 func (cdr xs)))))

(def (pair xs)
    (if (no xs)
            nil
        (no (cdr xs)) 
            (list (list (car xs)))
        (cons (list (car xs) (cadr xs))
              (pair (cddr xs)))))

; MAP applies a function to each element of xs
; #TODO make this accept multivalent functions

(def (map func xs)
    (foldr (fn (x y) (cons (func x) y)) '() xs))

; TESTIFY If given a function, return the function. Else return a function that
; compares for equality with the argument

(def (testify arg)
    (if (procedure? arg) arg
        (fn (x) (is x arg))))

; KEEP returns a list containing only the elements that satisfy the predicate.
; REMOVE returns a list without the elements that satisfy the given predicate.

(def (keep pred lst)
    (let test (testify pred)
        (foldr
            (fn (x y) (if (test x) (cons x y) y))
            '()
            lst)))

(def (remove pred lst)
    (keep (complement (testify pred)) lst))

; SUM and PRODUCT of a list of numbers.

(def (sum lst) (fold + 0 lst))

(def (product lst) (fold * 1 lst))

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
    (if (no b)
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

(def (join . args)
    (foldr append '() args))

;;;; CONTROL FLOW

; DO performs its actions in sequence

(macro (do . args)
    `((fn () ,@args)))

; WHEN executes a list of statements only if the test returns a non-false value
; UNLESS executes the statements only if the test returns false

(macro (when test . body)
    `(if ,test (do ,@body)))

(macro (unless test . body)
    `(if (not ,test) (do ,@body)))

; CASE evaluates its first argument. It then walks down the key/value pairs
; to compare for equality with the keys, returning the value of the first match.

(macro (caselet var expr . args)
    (let ex (afn (args)
                (if (no (cdr args))
                    (car args)
                    `(if (is ,var ',(car args))
                            ,(cadr args)
                            ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(macro (case expr . args)
    `(caselet ,(uniq) ,expr ,@args))

; AND returns true if all its arguments are true, else it returns false
; OR returns true if at least one of its arguments is true

(macro (and . args)
    (if (no args) #t
        `(if ,(car args) (and ,@(cdr args))
             #f)))

(macro (or . args)
    (if (no args) #f
        `(if ,(car args) #t
             (or ,@(cdr args)))))

; LOOP Executes start, and then runs a recursive program that repeatedly
; executes the body and the update step until the test is satisfied.

(macro (loop start test update . body)
    (w/uniq gparm
        `(do ,start
            ((afn (,gparm)
                (if ,gparm
                    (do ,@body ,update (self ,test))))
            ,test))))

; FOR binds the variable v to init, and on every step it increments v by 1 and
; executes the body, until v exceeds max.

(macro (for v init max . body)
    ;(w/uniq (gi gm)
        `(with (,v nil gi ,init gm (+ ,max 1))
            (loop (= ,v gi) (< ,v gm) (++ ,v)
                ,@body)))

(macro (forlen var s . body)
    `(for ,var 0 (- (len ,s) 1) ,@body))

; EACH binds the variable x to each element of xs, and executes the body.

(def (walk seq func)
    (if (alist seq)
        ((afn (l)
            (when (pair? l)
                (func (car l))
                (self (cdr l)))) seq)
        ; else
        (forlen i seq
            (func (seq i)))))

(macro (each x xs . body)
    `(walk ,xs (fn (,x) ,@body)))

; REPEAT executes a block of code n times, returning the final value

(macro (repeat n . body)
    `(for i 1 ,n ,@body))

;;;; LOCAL BINDINGS

; WITH Creates a new environment, within which each VAR evaluates to the
; corresponding VAL, and uses this environment to evaluate EXPR
; LET Short form of WITH that only binds one var, but doesn't need parentheses.

(macro (with bindings . body)
    `((fn ,(map1 car (pair bindings))
          ,@body)
        ,@(map1 cadr (pair bindings))))

(macro (let var val . body)
    `(with (,var ,val) ,@body))

(macro (w/uniq params . body)
    (if (pair? params)
        `(with ,(apply join (map (fn (n) (list n '(uniq)))
                                params))
            ,@body)
        `(let ,params (uniq) ,@body)))

;;;; EQUALITY TESTING

; IN tests if its first argument is equal (in the sense of IS) to any if its
; later arguments

(macro (in x . xs)
    (w/uniq g
        `(let ,g ,x
            (or ,@(map1 (fn (c) `(is ,g ,c)) xs)))))

;;;; ASSIGNMENT

; ZAP modifies X to have the value obtained by applying func to X. Again, it
; can modify values inside structures.

(macro (zap func x)
    (w/uniq result
        `(let ,result (,func ,x)
            (do (= ,x ,result)
                ,result))))

; INCREMENT (++) and DECREMENT (--)
; These modify their argument, and return the modified result.
; As with PUSH and POP, ++ and -- are implemented using =, so they can reach
; inside structures.

(macro (++ x) `(zap [+ _ 1] ,x))

(macro (-- x) `(zap [- _ 1] ,x))

; PUSH and POP treat a list as a stack, appending or removing elements from
; the front. Because they are implemented using =, you get the same
; flexibility as with =, so you can write:
;   haskeme> (def x '(1 3 4)) ==> (1 3 4)
;   haskeme> (push 2 (cdr x)) ==> 'ok
;   haskeme> x                ==> (1 2 3 4)

(macro (pop lst)
    (w/uniq elem
        `(let ,elem (car ,lst)
            (do (= ,lst (cdr ,lst))
                ,elem))))

(macro (push val lst)
    `(do (= ,lst (cons ,val ,lst))
         'ok))
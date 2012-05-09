;;;; Combinations of CAR and CDR.

(def caar (x) (car (car x)))
(def cadr (x) (car (cdr x)))
(def cdar (x) (cdr (car x)))
(def cddr (x) (cdr (cdr x)))
(def caaar (x) (car (caar x)))
(def caadr (x) (car (cadr x)))
(def cadar (x) (car (cdar x)))
(def caddr (x) (car (cddr x)))
(def cdaar (x) (cdr (caar x)))
(def cdadr (x) (cdr (cadr x)))
(def cddar (x) (cdr (cdar x)))
(def cdddr (x) (cdr (cddr x)))

;;;; Fundamental operations

(def nil '())

(def no (obj) (is obj nil))

(def isa (x y) (is (type x) y))

(def isnt (x y) (not (is x y)))

(def id (obj) obj)

(def not (x) (if x #f #t))

(macro and args
    (if (no args) #t
        `(if ,(car args) (and ,@(cdr args))
             #f)))

(macro or args
    (if (no args) #f
        `(if ,(car args) #t
             (or ,@(cdr args)))))

;;;; Type checking

(def symbol? (x)    (isa x 'symbol))
(def pair? (x)      (isa x 'pair))
(def boolean? (x)   (isa x 'boolean))
(def char? (x)      (isa x 'char))
(def string? (x)    (isa x 'string))
(def number? (x)    (isa x 'number))
(def vector? (x)    (isa x 'vector))
(def hash? (x)      (isa x 'hash))
(def procedure? (x) (isa x 'procedure))
(def macro? (x)     (isa x 'macro))
(def port? (x)      (isa x 'port))

;;;; Fundamental list operations

(def list objs objs)

(def alist (obj)
    (or (is obj nil)
        (is (type obj) 'pair)))

(def null (obj)
    (or (is obj nil)
        (is obj "")
        (is obj $())
        (is obj #())))

(def pair (xs)
    (if (no xs)
            nil
        (no (cdr xs))
            (list (list (car xs)))
        (cons (list (car xs) (cadr xs))
              (pair (cddr xs)))))

(def map1 (f xs)
    (if (no xs)
        nil
        (cons (f (car xs)) (map1 f (cdr xs)))))

(def all (test xs)
    (let f (testify test)
        (if (no xs) #t
            (f (car xs)) (all test (cdr xs))
            #f)))

(def any (test xs)
    (let f (testify test)
        (if (no xs) #f
            (f (car xs)) #t
            (any test (cdr xs)))))

(def snoc (x lst)
    (if (null lst)
        (list x)
        (cons (car lst) (snoc x (cdr lst)))))

(def append (a b)
    (if (null b)
        a
        (append (snoc (car b) a) (cdr b))))

(def take (n xs)
    (if (is n 0)
        nil
        (cons (car xs) (take (- n 1) (cdr xs)))))

(def drop (n xs)
    (if (is n 0)
        xs
        (drop (- n 1) (cdr xs))))

(def splitat (n xs)
    (list (take n xs) (drop n xs)))

(def nthcdr (n xs)
    (if (is n 0) xs
        (nthcdr (- n 1) (cdr xs))))

(def tuples (xs n)
    (if (no xs)
        nil
        (cons (take n xs)
              (tuples (nthcdr n xs) n))))

;;;; Control flow

(macro do args
    `((fn () ,@args)))

(macro when (test . body)
    `(if ,test (do ,@body)))

(macro unless (test . body)
    `(if (not ,test) (do ,@body)))

;;;; Multiple definitions

(macro defs args
    `(do ,@(map1 [cons 'def _] (pair args))))

;;;; Local bindings

; WITH Creates a new environment, within which each VAR evaluates to the
; corresponding VAL, and uses this environment to evaluate EXPR
; LET Short form of WITH that only binds one var, but doesn't need parentheses.
; WITHS Like with, but binds variables sequentially, so that later params
; can refer to earlier ones.

(macro with (params . body)
    `((fn ,(map1 car (pair params))
          ,@body)
        ,@(map1 cadr (pair params))))

(macro let (var val . body)
    `(with (,var ,val) ,@body))

(macro withs (params . body)
    (if (no params)
        `(do ,@body)
        `(let ,(car params) ,(cadr params)
            (withs ,(cddr params) ,@body))))

;;;; Recursive lambda functions (required for many macros)

(macro rfn (name params . body)
    `(let ,name nil
        (= ,name (fn ,params ,@body))))

(macro afn (params . body)
    `(let self nil
        (= self (fn ,params ,@body))))

;;;; Control flow (case statement)

(macro caselet (var expr . args)
    (let ex (afn (args)
                (if (no args) 'nil
                    (no (cdr args)) (car args)
                    `(if (is ,var ',(car args))
                            ,(cadr args)
                            ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(macro case (expr . args)
    `(caselet ,(uniq) ,expr ,@args))

;;;; Folds and scans

; FOLDR and FOLDL perform left and right folds with an initial value
; FOLDR1 and FOLDL1 use the car of the list as the initial value
; FOLD and REDUCE are synonyms for FOLDL

(def foldr (func end lst)
    (if (null lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))

(def foldl (func accum lst)
    (if (null lst)
        accum
        (foldl func (func accum (car lst)) (cdr lst))))

(def foldr1 (func lst)
    (foldr func (car lst) (cdr lst)))

(def foldl1 (func lst)
    (foldl func (car lst) (cdr lst)))

(def fold foldl)
(def reduce fold)

(def fold1 foldl1)
(def reduce1 fold1)

; More list manipulation

(def replicate (n val)
    (if (is n 0) '()
        (cons val (replicate (- n 1) val))))

(def make-vector (n val)
    (apply vector (replicate n val)))

(def xrange (a b)
    (if (>= a b) '()
        (cons a (xrange (+ 1 a) b))))

(def range (a . b)
    (if (no b)
        (xrange 0 a)
        (xrange a (car b))))

(def join args
    (foldr append '() args))

; W/UNIQ Binds a unique symbol name to each PARAMS (useful in macros)

(macro w/uniq (params . body)
    (if (pair? params)
        `(with ,(apply join (map1 (fn (n) (list n '(uniq)))
                                params))
            ,@body)
        `(let ,params (uniq) ,@body)))

;;;; Membership testing

(macro in (x . xs)
    (w/uniq g
        `(let ,g ,x
            (or ,@(map1 (fn (c) `(is ,g ,c)) xs)))))

;;;; Control flow (execute in a local environment)

(macro local exprs
    (w/uniq g
        `((fn (,g) ,@exprs) nil)))

;;;; Control flow (loops)

(macro while (test . body)
    (w/uniq gp
        `((afn (,gp)
            (when ,gp ,@body (self ,test)))
          ,test)))

(macro loop (start test update . body)
    (w/uniq gparm
        `(do ,start
            ((afn (,gparm)
                (if ,gparm
                    (do ,@body ,update (self ,test))))
            ,test))))

(macro for (v init max . body)
    (w/uniq (gi gm)
        `(with (,v nil ,gi ,init ,gm (+ ,max 1))
            (loop (= ,v ,gi) (< ,v ,gm) (++ ,v)
                ,@body))))

(macro forlen (var s . body)
    `(for ,var 0 (- (len ,s) 1) ,@body))

(def walk (seq func)
    (if (alist seq)
            ((afn (l)
                (when (pair? l)
                    (func (car l))
                    (self (cdr l)))) seq)
        ; else
        (forlen i seq
            (func (seq i)))))

(macro each (x xs . body)
    `(walk ,xs (fn (,x) ,@body)))

(macro repeat (n . body)
    `(for i 1 ,n ,@body))

;;;; Higher order functions

(def const (x)
    (fn _ x))

(def complement (f)
    (fn args (not (apply f args))))

(def flip (f)
    (fn (x y) (f y x)))

(def curry (func x)
    (fn y (apply func (cons x y))))

(def uncurry (func)
    (fn (x . rest) (apply (func x) rest)))

(macro compose args
    (let g (uniq)
        `(fn ,g
            ,((afn (fs)
                (if (cdr fs)
                    (list (car fs) (self (cdr fs)))
                    `(apply ,(if (car fs) (car fs) 'id) ,g)))
              args))))

(def applyn (n f)
    (if (is n 0)
        id
        (fn (x)
            (f ((applyn (- n 1) f) x)))))

;;;; Definition of map in terms of map1

(def map (func . args)
    (if (any null args) nil
        (cons
            (apply func (map1 car args))
            (apply (curry map func) (map1 cdr args)))))

;;;; Numeric functions

(def pi (* 4 (atan 1)))
(def e (exp 1))

(def zero [== _ 0])
(def positive [> _ 0])
(def negative [< _ 0])
(def odd [== (mod _ 2) 1])
(def even [== (mod _ 2) 0])

(def inc [+ _ 1])
(def dec [- _ 1])

(def sum (lst) (fold + 0 lst))
(def product (lst) (fold * 1 lst))

(def max (first . rest)
    (fold (fn (old new) (if (> old new) old new))
        first rest))

(def min (first . rest)
    (fold (fn (old new) (if (< old new) old new))
        first rest))

;;;; Polymorphic fold (works on lists, vectors, strings)

(def pfold_ (foldfunc f xs)
    (let init (case (type xs)
                pair '()
                vector $()
                string "")
        (foldfunc f init xs)))

(def pfoldr (curry pfold_ foldr))
(def pfoldl (curry pfold_ foldl))
(def pfold pfoldl)

;;;; Unfold

(def unfold (func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

;;;; Scans (like folds, but keep the list of intermediate results)

(def scanr (func end lst)
    (if (null lst)
        (list end)
        (let result (scanr func end (cdr lst))
            (cons (func (car lst) (car result))
                  result))))

(def scanl (func result lst)
    (if (null lst)
        (list result)
        (cons result
              (scanl func
                     (func (car lst) result)
                     (cdr lst)))))

(def scanr1 (func lst)
    (scanr func (car lst) (cdr lst)))

(def scanl1 (func lst)
    (scanl func (car lst) (cdr lst)))

(def scan scanl)
(def scan1 scanl1)

;;;; List filtering operations

(def testify (arg)
    (if (procedure? arg) arg [is _ arg]))

(def keep (pred lst)
    (let test (testify pred)
        (foldr
            (fn (x y) (if (test x) (cons x y) y))
            '()
            lst)))

(def remove (pred lst)
    (keep (complement (testify pred)) lst))

;;;; More list manipulation

(def reverse (x)
    (pfold (flip cons) x))

;;;; Association Lists

(def zip args
    (if (any null args) '()
        (cons
            (map1 car args)
            (apply zip (map1 cdr args)))))

(def unzip (lst) (apply zip lst))

;;;; Assignment and modification

(macro zap (func x)
    (w/uniq result
        `(let ,result (,func ,x)
            (do (= ,x ,result)
                ,result))))

(macro ++ (x) `(zap [+ _ 1] ,x))
(macro -- (x) `(zap [- _ 1] ,x))

(macro pop (lst)
    (w/uniq elem
        `(let ,elem (car ,lst)
            (do (= ,lst (cdr ,lst))
                ,elem))))

(macro push (val lst)
    `(= ,lst (cons ,val ,lst)))

;;;; Error checking

(macro assert (test . rest)
    `(if ,test 'ok
         (raise "AssertFailed" ,@rest)))

(macro assert-false (test . rest)
    `(assert (not ,test) ,@rest))

(macro assert-equal (fst snd . rest)
    `(assert (is ,fst ,snd) ,@rest))

(macro w/handler (handler . exprs)
    `(try (do ,@exprs) ,handler))

(macro w/handlers (handlers . exprs)
    ((afn (hs)
        (if (no (cdr hs))
            `(w/handler ,(car hs) ,@exprs)
            `(w/handler ,(car hs) ,(self (cdr hs)))))
     handlers))

;;;; Tests

(macro run-tests (name . tests)
    `(do ,@(map1 (fn (c) `(local ,c)) tests)
        (pr "Tests passed: ")
        (prn ,name)))

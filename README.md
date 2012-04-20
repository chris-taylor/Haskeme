# Haskeme

Haskeme is a Scheme interpreter written in Haskell. It's very much a work-in-progress, as well as a way for me to experiment with new ideas and syntax. The skeleton of the interpreter is based on the excellent tutorial found at:

* http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

I've also borrowed some ideas from Arc and Q:

* http://ycombinator.com/arc/tut.txt
* http://en.wikipedia.org/wiki/Q_(programming_language_from_Kx_Systems)

## Features

* Data types
  - Symbol, Pair, Char, String, Boolean
  - Numeric types (Integer, Rational, Float, Complex)
  - Vectors (constant access time lists)
  - Hashes (key-value stores)
  - Functions are first-class values
* Control flow
  - Smart `if` statement: `(if test1 conseq1 ... testn conseqn alternative)`
  - Smart `case` statement: `(case x (val1 result1) ... (valn resultn))`
  - Statement grouping with `do`
* Scope
  - Top-level definitions with `def` and `macro`
  - Local bindings with `let` and `with`
* Assignment
  - Assignment using `=` operator
  - Assignment reaches inside structures: `(= (car x) 1)`
  - Set elements of strings, vectors and hashes too: `(= (vec 0) 1)`
* Functions:
  - User defined functions, lambdas (with `fn` keyword) and lexical closures
  - Notation for unary functions: `[+ x 1]` for `(fn (x) (+ x 1))`
  - Full suite of numeric functions (trigonometry, sqrt, exp/log etc)
  - Can call strings, vectors and hashes like functions:
    + `(str n)` returns then `n`th character of a string
    + `(vec n)` returns the element at the `n`th index of a vector
    + `(hash k)` returns the value associated with key `k` of a hash
* I/O
  - Read from stdin or any file
  - Write to stdout or any file
  - Load libraries from a file
* Basic macro system:
  - Whole language, including user defined functions, available at macro expansion time
* Standard library
 
## Still to come

* Data types
  - Stream?
* Error handling for numeric operations
* Tail call optimization
* Comments
* R5RS compliance
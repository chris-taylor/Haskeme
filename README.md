# Haskeme

Haskeme is a Lisp interpreter written in Haskell. It's very much a work-in-progress, as well as a way for me to experiment with new ideas and syntax. The skeleton of the interpreter is based on the excellent tutorial found at:

* http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

I've also borrowed some ideas from Arc and Q:

* http://ycombinator.com/arc/tut.txt
* http://en.wikipedia.org/wiki/Q_(programming_language_from_Kx_Systems)

## Features

* Data types
  - Symbol, Pair, Char, String, Boolean
  - Full numeric tower (Integer, Rational, Float, Complex)
  - Vectors
  - Hashes
  - Procedures
* Control flow
  - Conditionals (`if`, `case`, `when`)
  - Statement grouping with `do`
* Scope
  - Top-level definitions with `def`
  - Local bindings with `let` and `with`
* Assignment
  - Assignment using `=` operator
  - Assignment reaches inside structures: `(= (car x) 1)`
  - Set elements of strings, vectors and hashes: `(= (vec 0) 1)`
* Functions:
  - User defined functions, lambdas (with `fn` keyword) and lexical closures
  - Notation for unary functions: `[+ _ 1]`
  - Full suite of numeric functions (trigonometry, sqrt, exp/log etc)
  - Basic random number generation with `random`
  - Can call strings, vectors and hashes as if they were functions: `(vec 0)` etc.
* I/O
  - Read/write to stdin/stdout or any file
  - Load libraries from a file
* Flexible macro system:
  - Whole language, including user defined functions, available at macro expansion time
* Standard library
 
## Still to come

* Tail call optimization
* Continuations
* Compiler
* Random numbers
* Streams (lazy lists)
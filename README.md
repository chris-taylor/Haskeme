# Haskeme

Haskeme is a Scheme interpreter written in Haskell. It's very much a work-in-progress, as well as a way for me to experiment with new ideas and syntax. The skeleton of the interpreter is based on the excellent tutorial found at:

  http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

I've also borrowed some ideas from Arc and Q:

  http://ycombinator.com/arc/tut.txt
  http://en.wikipedia.org/wiki/Q_(programming_language_from_Kx_Systems)

## Features

* Data types
  - Symbol, Pair, Char, String, Boolean
  - Numeric types (Integer, Rational, Float, Complex)
  - Vectors (mutable, constant access time lists)
* Functions:
  - User defined functions, lambdas and lexical closures
  - Full suite of numeric functions (trigonometry, sqrt, exp/log etc)
  - Can call strings and vectors like functions defined over the integers: `(obj n)` returns the element in the nth position of obj
* I/O
  - Read from stdin or any file
  - Write to stdout or any file
  - Load libraries from a file
* Basic macro system:
  - Multiple levels of quasiquote/unquote/unquote-splicing
  - Local bindings with 'let' and 'with'
  - Whole language, including user defined functions, available at macro expansion time
* Standard library
 
## Still to come

* Data types
  - Hash
  - Stream
* Error handling for numeric operations
* Tail call optimization
* Truthy values for non-boolean types
* Literal unary functions, e.g. with `[+ _ 1]` notation
* Comments
* R5RS compliance
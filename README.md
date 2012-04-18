Haskeme
=======

A Scheme interpreter written in Haskell. Based on the interpreter at:

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

Aims and next steps:

* Improve numerical handling:
  - Promotion/coercion for numeric types (probably requires a Numeric module)
* Quasiquote/unquote/unquote splicing (parsing and evaluation)
* Data types:
  - Vector
  - Hash
  - Type testing, eg array?, hash?
* Functions:
  - Symbol manipulation
  - String manipulation
  - Numeric functions (trigonometry, sqrt, exp/log etc)
* Macros
* Tail call optimization
* Standard library
* Generic truth and falsity
* Index into strings/arrays/hashes with function application
* Comments
* Make the interpreter R5RS compliant
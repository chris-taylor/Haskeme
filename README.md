Haskeme
=======

A Scheme interpreter written in Haskell. Based on the interpreter at:

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

Aims and next steps:

* Improve numerical handling:
  - Promotion/coercion for numeric types
* Quasiquote/unquote/unquote splicing (parsing and evaluation)
* Assignment (set-car!, set-cdr!)
* Vector/array data type
* Hash data type
* Type testing, eg array?, hash?
* Symbol handling functions
* String functions
* Macros
* Tail call optimization
* Standard library
* Generic truth and falsity
* Index into strings/arrays/hashes with function application notation
* Comments
* Make the interpreter R5RS compliant
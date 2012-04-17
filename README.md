Haskeme
=======

A Scheme interpreter written in Haskell. Based on the interpreter at:

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

Aims and next steps:

* Improve numerical handling:
  - Introduce Ratio, Float and Complex types
  - Parsing for numeric literals
  - Promotion/coercion for numeric types
* Quasiquote/unquote/unquote splicing (parsing and evaluation)
* Control flow (cond/case statements)
* Assignment (set-car!, set-cdr!)
* Vector/array data type
* Hash data type
* Type testing, eg procedure?, array?, hash?
* Symbol handling functions
* String functions
* Macros
* Tail call optimization
* Standard library
* Bindings (let and with - use macro system for these?)
* Generic truth and falsity
* Index into strings/arrays/hashes with function application notation
* Comments
* Make the interpreter R5RS compliant
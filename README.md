# Haskeme

Haskeme is a Scheme interpreter written in Haskell. It's very much a work-in-progress, as well as a way for me to experiment with new ideas and syntax. The skeleton of the interpreter is based on the excellent tutorial found at:

  http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

I've also borrowed some ideas from Arc:

  http://ycombinator.com/arc/tut.txt

## Aims and next steps:

* Improve numerical handling:
  - Promotion/coercion for numeric types (probably requires a Numeric module)
* Data types:
  - Hash
  - Stream
* Functions:
  - Symbol manipulation
  - String manipulation
  - Numeric functions (trigonometry, sqrt, exp/log etc)
* Macros
* Tail call optimization
* Standard library
* Generic truth and falsity
* Comments
* Make the interpreter R5RS compliant
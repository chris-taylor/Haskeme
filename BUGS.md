# Known Bugs

* Cdr of an empty vector returns the empty vector
* Car/cdr return an error message talking about lists
* Doing e.g. `(+ 1 "1")` crashes the interpreter
* No total ordering on LispVals, so hash keys are often overwritten
* When using structure-aware set (e.g. `(= (v 1) 'a)`, we *should* evaluate indices, otherwise you can't use a variable in index position
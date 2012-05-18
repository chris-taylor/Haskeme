# Known Bugs

* Car/cdr return an error message talking about lists

* If a macro is named NAME, you can define a function also called NAME, but you
  can't call it, because the macro expander code prevents it. Possible solutions
  1. Remove the macro in scope when you redefine it
  2. Throw an error if you try to define a function with that name
  3. Show a warning if you define a function with that name
  4. Leave it as it is - you can still use the function as a value (e.g. you
  can supply it as an argument to map or call it with apply) but you can't
  use it in functional position.

* Currently can't use lists, vectors or other data structures as hash keys

* Quasiquote/unquote/unquote-splicing doesn't work in vectors or hashes

* There are several bugs when you use multiple levels of quasiquote/unquote

* `string->symbol` can be used to create illegal symbols (e.g. ones starting with #)
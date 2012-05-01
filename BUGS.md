# Known Bugs

* Cdr of an empty vector returns the empty vector
* Car/cdr return an error message talking about lists
* Currently can't use lists, vectors or other data structures as hash keys
* Quasiquote/unquote/unquote-splicing doesn't work in vectors or hashes
* There are several bugs when you use multiple levels of quasiquote/unquote
* `string->symbol` can be used to create illegal symbols (e.g. ones starting with #)
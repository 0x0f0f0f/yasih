## Roadmap

- [ ] Basic Unit Test Suite
- [ ] Multi line parsing in REPL
- [ ] Implement the full [R5RS](https://www.gnu.org/software/guile/docs/docs-1.6/guile-ref/R5RS-Index.html)
- [ ] Quasiquotation
- [ ] ~~begin~~ while and do blocks
- [ ] Correct Char parsing (extra characters)
- [X] Throw error when redefining a reserved keyword
- [X] Standard Library (Chapter 10)
- [X] Add let statement
- [X] Parse comments
- [X] Import path and autoload stdlib
- [X] File I/O Primitives (Chapter 9)
- [X] Implement the rest of string functions (Exercise 5.4)
- [X] Implement numerical operators for the whole numerical tower
- [X] Solve circular import
- [X] Implement `showVal` for indexed vectors
- [X] Closures and Environments (Chapter 8)
- [X] Mutable State (Chapter 7)
- [x] Implement a REPL
- [x] Implement `cond` and `case` expressions (Exercise 5.3)
- [x] Implement `eval` for complex numbers, ratios, lists and vectors
- [x] Monadic error handling
- [x] Primitive Type Checking functions
- [x] Primitive Numerical Operations
- [x] Expression Parser

## Bugs

- [ ] Handle Exit code on error correctly.
- [X] Fix parser for unexpected trailing parens
- [X] ~~Fix parser to correctly parse (and show) complex numbers with a negative real/imaginary part~~
- [X] ~~Ignore trailing and leading whitespace in lists.~~
- [X] ~~`equal?` does not recurse over lists. (Exercise 5.2)~~

# haskell-toy-scheme

A small scheme interpreter written following the tutorial book ["Write Yourself a Scheme in 48 Hours"](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) to learn both Haskell and Scheme in detail.

## Installation

Clone the repo
```sh
git clone https://github.com/0x0f0f0f/haskell-toy-scheme
cd haskell-toy-scheme
```

Install

```
stack install
```

## Usage

The binary `toy-scheme` accepts an expression as the first
argument, if no argument is passed then a REPL will be started.

```sh 
toy-scheme "(+ 2 3)"
5
```

Usage of [rlwrap](https://github.com/hanslub42/rlwrap) when using the REPL is suggested, it enables 
readline features such as arrow keys in the interpreter and bash-like keybindings 

```lisp
rlwrap toy-scheme
λ> (define (map f l) (if (eq? l '()) l (cons (f (car l)) (map f (cdr l)))))
(lambda ("f" "l") ...)
λ> (map (lambda (x) (+ 1 x)) '(1 2 3 4))
(2 3 4 5)
```
# Examples

To try the examples in the `examples` directory run:

```
cd examples
rlwrap toy-scheme < example.scm
```

## Roadmap

- [ ] Implement `numericBinop` for complex numbers and ratios
- [ ] Basic Unit Test Suite
- [ ] Standard Library (Chapter 10)
- [ ] Fold and Unfold (Chapter 10)
- [ ] File I/O Primitives (Chapter 9)
- [ ] Implement the rest of string functions (Exercise 5.4)
- [ ] Fix parser to correctly parse (and show) complex numbers with a negative real/imaginary part
- [ ] Parse comments
- [ ] Multiline parsing
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
- [X] Ignore trailing and leading whitespace in lists.
- [x] ~~`equal?` does not recurse over lists. (Exercise 5.2)~~

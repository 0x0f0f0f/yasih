# haskell-toy-scheme

## Roadmap

- [ ] Implement `showVal` for indexed vectors
- [ ] Implement `numericBinop` for complex numbers and ratios
- [ ] Basic Unit Test Suite
- [ ] Implement vectors using `vector-sized` instead of `Data.Array`
- [ ] Standard Library (Chapter 10)
- [ ] Fold and Unfold (Chapter 10)
- [ ] File I/O Primitives (Chapter 9)
- [ ] Closures and Environments (Chapter 8)
- [X] Mutable State (Chapter 7)
- [ ] Implement the rest of string functions (Exercise 5.4)
- [ ] Fix parser to correctly parse (and show) complex numbers with a negative real/imaginary part
- [x] Implement a REPL
- [x] Implement `cond` and `case` expressions (Exercise 5.3)
- [x] Implement `eval` for complex numbers, ratios, lists and vectors
- [x] Monadic error handling
- [x] Primitive Type Checking functions
- [x] Primitive Numerical Operations
- [x] Expression Parser

## Bugs

- [ ] Handle Exit code on error correctly.
- [x] ~~`equal?` does not recurse over lists. (Exercise 5.2)~~

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

A small scheme interpreter written following the tutorial book ["Write Yourself a Scheme in 48 Hours"](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) to learn both Haskell and Scheme in detail.

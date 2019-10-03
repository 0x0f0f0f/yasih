# yasih - Yet Another Scheme in Haskell

![screenshot.png](Screenshot)

A small scheme interpreter written following the tutorial book ["Write Yourself a Scheme in 48 Hours"](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) to learn both Haskell and Scheme in detail.

## Installation

Clone the repo
```sh
git clone https://github.com/0x0f0f0f/yasih
cd yasih
```

Install

```sh
# Compile and the binary
stack install
# Copy the standard library to an include path
mkdir -p ~/.local/lib/yasih/
cp stdlib/stdlib.scm ~/.local/lib/yasih/stlib.scm
```


## Usage

The binary `yasih` accepts a program as the first
argument, if no argument is passed then a REPL will be started.
If `-e` is passed as an option the first argument will be evaluated as an expression.

```sh 
yasih -e "(+ 2 3)"
5
```

Usage of [rlwrap](https://github.com/hanslub42/rlwrap) when using the REPL is suggested, it enables 
readline features such as arrow keys in the interpreter and bash-like keybindings 

```lisp
rlwrap yasih
λ> (define (map f l) (if (null? l) l (cons (f (car l)) (map f (cdr l)))))
(lambda ("f" "l") ...)
λ> (map (lambda (x) (+ 1 x)) '(1 2 3 4))
(2 3 4 5)
```
# Examples

To try the examples in the `examples` directory run:

```
cd examples
rlwrap yasih file.scm
```

# Testing

There are work in progress integration tests.

To test the R5RS coverage status run
```
SILENT=true ./integration-tests/r5rs-coverage.sh
```

To run general integration tests run
```
SILENT=true ./integration-tests/integration-test.sh
```

## How to forkbomb :)
```
((lambda (x) (x (x x))) (lambda (x) (x (x x))))
```
#!/usr/bin/env bash

source "`dirname \"$0\"`/framework.sh"


describe "R5RS Arithmetic Coverage"
t "*" "<primitive>"
t "+" "<primitive>"
t "-" "<primitive>"
t "/" "<primitive>"
t "abs" "<primitive>"
t "ceiling" "<primitive>"
t "floor" "<primitive>"
t "round" "<primitive>"
t "truncate" "<primitive>"
t "max" "(lambda (\"first\" . num-list) ...)"
t "min" "(lambda (\"first\" . num-list) ...)"

describe "R5RS Boolean Coverage"
t "boolean?" "<primitive>"
t "not" "(lambda (\"x\") ...)"

describe "R5RS Scientific Coverage"
t "acos" "<primitive>"
t "asin" "<primitive>"
t "atan" "<primitive>"
t "cos" "<primitive>"
t "sin" "<primitive>"
t "exp" "<primitive>"
t "expt" "<primitive>"
t "log" "<primitive>"
t "sqrt" "<primitive>"
t "tan" "<primitive>"

describe "R5RS Complex Coverage"
t "angle" "<primitive>"
t "complex?" "<primitive>"
t "imag-part" "<primitive>"
t "real-part" "<primitive>"
t "magnitude" "<primitive>"
t "make-polar" "<primitive>"
t "make-rectangular" "<primitive>"

describe "R5RS Append/Reverse Coverage"
t "append" "(lambda (\"lst\" . lsts) ...)"
t "reverse" "(lambda (\"lst\") ...)"

describe "R5RS Pairs Coverage"
t "car" "<primitive>"
t "cdr" "<primitive>"
t "cons" "<primitive>"
t "pair?" "<primitive>"
t "set-car!" "<primitive>"
t "set-cdr!" "<primitive>"

describe "R5RS Comparison Coverage"
t "positive?" "(lambda ( . args) ...)"
t "negative?" "(lambda ( . args) ...)"
t "zero?" "(lambda ( . args) ...)"

describe "R5RS Integers Coverage"
t "integer?" "<primitive>"

describe "R5RS Integer Operators Coverage"
t "lcm" "<primitive>"
t "gcd" "<primitive>"
t "modulo" "<primitive>"
t "remainder" "<primitive>"
t "quotient" "<primitive>"
t "odd?" "(lambda (\"num\") ...)"
t "even?" "(lambda (\"num\") ...)"

describe "R5RS Characters Coverage"
t "char->integer" "<primitive>"
t "char-alphabetic?" "<primitive>"
t "char-ci<=?" "<primitive>"
t "char-ci<?" "<primitive>"
t "char-ci=?" "<primitive>"
t "char-ci>=?" "<primitive>"
t "char-ci>?" "<primitive>"
t "char-downcase" "<primitive>"
t "char-lower-case?" "<primitive>"
t "char-numeric?" "<primitive>"
t "char-upcase" "<primitive>"
t "char-upper-case?" "<primitive>"
t "char-whitespace?" "<primitive>"
t "char<=?" "<primitive>"
t "char<?" "<primitive>"
t "char=?" "<primitive>"
t "char>=?" "<primitive>"
t "char>?" "<primitive>"
t "char?" "<primitive>"
t "integer->char" "<primitive>"

describe "R5RS String Constructors Coverage"
t "list->string" "<primitive>"
t "make-string" "<primitive>"
t "string" "<primitive>"

describe "R5RS String Comparison Coverage"
t "string-ci<?" "<primitive>"
t "string-ci=?" "<primitive>"
t "string-ci>=?" "<primitive>"
t "string-ci>?" "<primitive>"
t "string<=?" "<primitive>"
t "string<?" "<primitive>"
t "string=?" "<primitive>"
t "string>=?" "<primitive>"
t "string>?" "<primitive>"

describe "R5RS Equality Coverage"
t "eq?" "<primitive>"
t "equal?" "<primitive>"
t "eqv?" "<primitive>"

describe "R5RS List Mapping Coverage"
t "for-each" "(lambda (\"f\" \"lst\") ...)"
t "map" "(lambda (\"f\" \"lst\") ...)"

describe "R5RS Symbol Primitives Coverage"
t "string->symbol" "<primitive>"
t "symbol->string" "<primitive>"
t "symbol?" "<primitive>"

describe "R5RS Appending Strings Coverage"
t "string-append" "<primitive>"

describe "R5RS String Selection Coverage"
t "string-copy" "<primitive>"
t "string-length" "<primitive>"
t "string-ref" "<primitive>"
t "substring" "<primitive>"

describe "R5RS String Modification Coverage"
t "string-fill!" "<primitive>"
t "string-set!" "<primitive>"

describe "R5RS String Predicates Coverage"
t "string?" "<primitive>"

describe "R5RS Loading Coverage"
t "(load \"`dirname \"$0\"`/../examples/fibonacci.scm\")" "(lambda (\"n\") ...)"


# TODO R5RS coverage tests
# apply: Fly Evaluation
# assoc: Retrieving Alist Entries
# assq: Retrieving Alist Entries
# assv: Retrieving Alist Entries
# call-with-current-continuation: Continuations
# call-with-input-file: File Ports
# call-with-output-file: File Ports
# call-with-values: Multiple Values

# char-ready?: Reading
# close-input-port: Closing
# close-output-port: Closing
# current-input-port: Default Ports
# current-output-port: Default Ports
# display: Writing
# dynamic-wind: Dynamic Wind
# eof-object?: Reading
# eval: Fly Evaluation
# exact->inexact: Exactness
# exact?: Exactness
# 
# force: Delayed Evaluation
# inexact->exact: Exactness
# inexact?: Exactness
# input-port?: Ports
# interaction-environment: Fly Evaluation
# list: List Constructors
# list->vector: Vector Creation
# length: List Selection
# list-ref: List Selection
# list-tail: List Selection
# list?: List Predicates



# make-vector: Vector Creation
# member: List Searching
# memq: List Searching
# memv: List Searching
# newline: Writing
# null?: List Predicates
# number->string: Conversion
# number?: Numerical Tower
# open-input-file: File Ports
# open-output-file: File Ports
# output-port?: Ports
# peek-char?: Reading
# procedure?: Procedure Properties
# rational?: Reals and Rationals
# read: Scheme Read
# read-char?: Reading
# real?: Reals and Rationals

# string->list: List/String Conversion
# string->number: Conversion




# values: Multiple Values
# vector: Vector Creation
# vector->list: Vector Creation
# vector-fill!: Vector Accessors
# vector-length: Vector Accessors
# vector-ref: Vector Accessors
# vector-set!: Vector Accessors
# vector?: Vector Creation
# with-input-from-file: File Ports
# with-output-to-file: File Ports
# write-char: Writing

printf "\n\n$tests_run tests run\n$tests_passed PASSED\n$tests_failed FAILED\n"
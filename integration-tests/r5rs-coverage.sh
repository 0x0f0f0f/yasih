#!/usr/bin/env bash

source "`dirname \"$0\"`/framework.sh"

# Test if a procedure is a p
function p {
    t $1 "<primitive>"
}

# Describe an R5RS coverage test2
function cover {
    describe "R5RS $1 Coverage"
}

cover "Arithmetic"
assert "`$INTERPRETER_NAME \*`" "<primitive>"
p "+"
p "-"
p "/"
p "abs"
p "ceiling"
p "floor"
p "round"
p "truncate"
t "max" "(lambda (first . num-list) ...)"
t "min" "(lambda (first . num-list) ...)"

cover "Boolean"
p "boolean?"
t "not" "(lambda (x) ...)"

cover "Scientific"
p "acos"
p "asin"
p "atan"
p "cos"
p "sin"
p "exp"
p "expt"
p "log"
p "log10"
p "sqrt"
p "tan"
p "sinh"
p "cosh"
p "tanh"
p "asinh"
p "acosh"
p "atanh"

cover "Complex"
p "angle"
p "complex?"
p "imag-part"
p "real-part"
p "magnitude"
p "make-polar"
p "make-rectangular"

cover "Append/Reverse"
t "append" "(lambda (lst . lsts) ...)"
t "reverse" "(lambda (lst) ...)"

cover "Pairs"
p "car"
p "cdr"
p "cons"
p "pair?"
p "set-car!"
p "set-cdr!"

cover "Comparison"
t "positive?" "(lambda ( . args) ...)"
t "negative?" "(lambda ( . args) ...)"
t "zero?" "(lambda ( . args) ...)"

cover "Integers"
p "integer?"

cover "Integer Operators"
p "lcm"
p "gcd"
p "modulo"
p "remainder"
p "quotient"
t "odd?" "(lambda (num) ...)"
t "even?" "(lambda (num) ...)"

cover "Characters"
p "char->integer"
p "char-alphabetic?"
p "char-ci<=?"
p "char-ci<?"
p "char-ci=?"
p "char-ci>=?"
p "char-ci>?"
p "char-downcase"
p "char-lower-case?"
p "char-numeric?"
p "char-upcase"
p "char-upper-case?"
p "char-whitespace?"
p "char<=?"
p "char<?"
p "char=?"
p "char>=?"
p "char>?"
p "char?"
p "integer->char"

cover "String Constructors"
t "list->string" "(lambda (lst) ...)"
p "make-string"
p "string"

cover "String Comparison"
p "string-ci<?"
p "string-ci=?"
p "string-ci>=?"
p "string-ci>?"
p "string<=?"
p "string<?"
p "string=?"
p "string>=?"
p "string>?"

cover "Equality"
p "eq?"
p "equal?"
p "eqv?"

cover "List Mapping"
t "for-each" "(lambda (f lst) ...)"
t "map" "(lambda (f lst) ...)"

cover "Symbol Primitives"
p "string->symbol"
p "symbol->string"
p "symbol?"

cover "Appending Strings"
p "string-append"

cover "String Selection"
p "string-copy"
p "string-length"
p "string-ref"
p "substring"

cover "String Modification"
p "string-fill!"
p "string-set!"

cover "String Predicates"
p "string?"

cover "Loading"
t "(load \"`dirname \"$0\"`/../examples/fibonacci.scm\")" "(lambda (n) ...)"

cover "Numerical Tower"
p "number?"

cover "File Ports"
p "with-input-from-file"
p "with-output-to-file"
p "call-with-input-file"
p "call-with-output-file"
p "open-input-file"
p "open-output-file" 

cover "Ports"
p "output-port?"
p "input-port?"

cover "Reals and Rationals"
p "rational?"
p "real?"

cover "Closing"
p "close-input-port"
p "close-output-port"

cover "Reading"
p "char-ready?"
p "eof-object?"
p "peek-char?"
p "read-char?"

cover "Fly Evaluation"
p "interaction-environment"
p "apply"
p "eval"

cover "Writing"
p "display"
p "newline"
p "write-char"

cover "Delayed Evaluation"
p "force"

cover "Continuations"
p "call-with-current-continuation"

cover "Multiple Values"
p "call-with-values"
p "values"

cover "Default Ports"
p "current-input-port"
p "current-output-port"

cover "Dynamic Wind"
p "dynamic-wind"

cover "Exactness"
p "exact->inexact"
p "exact?"
p "inexact->exact"
p "inexact?"

cover "List Constructors"
t "list" "(lambda ( . objs) ...)"

cover "List Selection"
t "length" "(lambda (lst) ...)"
p "list-ref" 
p "list-tail" 

cover "List Predicates"
p "list?"
t "null?" "(lambda (x) ...)"

cover "Vector Creation"
p "list->vector"
p "make-vector"
p "vector"
p "vector->list"
p "vector?"

cover "List Searching"
t "member" "(lambda (x lst) ...)"
t "memq" "(lambda (x lst) ...)"
t "memv" "(lambda (x lst) ...)"
t "assoc" "(lambda (x lst) ...)"
t "assq" "(lambda (x lst) ...)"
t "assv" "(lambda (x lst) ...)"


cover "Conversion"
p "number->string"
p "string->number"
p "string->list"

cover "Procedure Properties"
p "procedure?" 

cover "Scheme Read"
p "read"

cover "Vector Accessors"
p "vector-fill!"
p "vector-length"
p "vector-ref"
p "vector-set!"

printf "\n\n$tests_run tests run\n$tests_passed PASSED\n$tests_failed FAILED\n"
#!/usr/bin/env bash

source "`dirname \"$0\"`/framework.sh"

function primitive {
    t $1 "<primitive>"
}

function cover {
    describe "R5RS $1 Coverage"
}

cover "Arithmetic"
primitive "*"
primitive "+"
primitive "-"
primitive "/"
primitive "abs"
primitive "ceiling"
primitive "floor"
primitive "round"
primitive "truncate"
t "max" "(lambda (first . num-list) ...)"
t "min" "(lambda (first . num-list) ...)"

cover "Boolean"
primitive "boolean?"
t "not" "(lambda (x) ...)"

cover "Scientific"
primitive "acos"
primitive "asin"
primitive "atan"
primitive "cos"
primitive "sin"
primitive "exp"
primitive "expt"
primitive "log"
primitive "sqrt"
primitive "tan"

cover "Complex"
primitive "angle"
primitive "complex?"
primitive "imag-part"
primitive "real-part"
primitive "magnitude"
primitive "make-polar"
primitive "make-rectangular"

cover "Append/Reverse"
t "append" "(lambda (lst . lsts) ...)"
t "reverse" "(lambda (lst) ...)"

cover "Pairs"
primitive "car"
primitive "cdr"
primitive "cons"
primitive "pair?"
primitive "set-car!"
primitive "set-cdr!"

cover "Comparison"
t "positive?" "(lambda ( . args) ...)"
t "negative?" "(lambda ( . args) ...)"
t "zero?" "(lambda ( . args) ...)"

cover "Integers"
primitive "integer?"

cover "Integer Operators"
primitive "lcm"
primitive "gcd"
primitive "modulo"
primitive "remainder"
primitive "quotient"
t "odd?" "(lambda (num) ...)"
t "even?" "(lambda (num) ...)"

cover "Characters"
primitive "char->integer"
primitive "char-alphabetic?"
primitive "char-ci<=?"
primitive "char-ci<?"
primitive "char-ci=?"
primitive "char-ci>=?"
primitive "char-ci>?"
primitive "char-downcase"
primitive "char-lower-case?"
primitive "char-numeric?"
primitive "char-upcase"
primitive "char-upper-case?"
primitive "char-whitespace?"
primitive "char<=?"
primitive "char<?"
primitive "char=?"
primitive "char>=?"
primitive "char>?"
primitive "char?"
primitive "integer->char"

cover "String Constructors"
primitive "list->string"
primitive "make-string"
primitive "string"

cover "String Comparison"
primitive "string-ci<?"
primitive "string-ci=?"
primitive "string-ci>=?"
primitive "string-ci>?"
primitive "string<=?"
primitive "string<?"
primitive "string=?"
primitive "string>=?"
primitive "string>?"

cover "Equality"
primitive "eq?"
primitive "equal?"
primitive "eqv?"

cover "List Mapping"
t "for-each" "(lambda (f lst) ...)"
t "map" "(lambda (f lst) ...)"

cover "Symbol Primitives"
primitive "string->symbol"
primitive "symbol->string"
primitive "symbol?"

cover "Appending Strings"
primitive "string-append"

cover "String Selection"
primitive "string-copy"
primitive "string-length"
primitive "string-ref"
primitive "substring"

cover "String Modification"
primitive "string-fill!"
primitive "string-set!"

cover "String Predicates"
primitive "string?"

cover "Loading"
t "(load \"`dirname \"$0\"`/../examples/fibonacci.scm\")" "(lambda (n) ...)"

cover "Numerical Tower"
primitive "number?"

cover "File Ports"
primitive "with-input-from-file"
primitive "with-output-to-file"
primitive "call-with-input-file"
primitive "call-with-output-file"
primitive "open-input-file"
primitive "open-output-file" 

cover "Ports"
primitive "output-port?"
primitive "input-port?"

cover "Reals and Rationals"
primitive "rational?"
primitive "real?"

cover "Closing"
primitive "close-input-port"
primitive "close-output-port"

cover "Reading"
primitive "char-ready?"
primitive "eof-object?"
primitive "peek-char?"
primitive "read-char?"

cover "Fly Evaluation"
primitive "interaction-environment"
primitive "apply"
primitive "eval"

cover "Writing"
primitive "display"
primitive "newline"
primitive "write-char"

cover "Delayed Evaluation"
primitive "force"

cover "Retrieving Alist Entries"
primitive "assoc"
primitive "assq"
primitive "assv"

cover "Continuations"
primitive "call-with-current-continuation"

cover "Multiple Values"
primitive "call-with-values"
primitive "values"

cover "Default Ports"
primitive "current-input-port"
primitive "current-output-port"

cover "Dynamic Wind"
primitive "dynamic-wind"

cover "Exactness"
primitive "exact->inexact"
primitive "exact?"
primitive "inexact->exact"
primitive "inexact?"

cover "List Constructors"
t "list" "(lambda ( . objs) ...)"

cover "List Selection"
t "length" "(lambda (lst) ...)"
primitive "list-ref" 
primitive "list-tail" 

cover "List Predicates"
primitive "list?"
primitive "null?"

cover "Vector Creation"
primitive "list->vector"
primitive "make-vector"
primitive "vector"
primitive "vector->list"
primitive "vector?"

cover "List Searching"
primitive "member"
primitive "memq"
primitive "memv"

cover "Conversion"
primitive "number->string"
primitive "string->number"
primitive "string->list"

cover "Procedure Properties"
primitive "procedure?" 

cover "Scheme Read"
primitive "read"

cover "Vector Accessors"
primitive "vector-fill!"
primitive "vector-length"
primitive "vector-ref"
primitive "vector-set!"

printf "\n\n$tests_run tests run\n$tests_passed PASSED\n$tests_failed FAILED\n"
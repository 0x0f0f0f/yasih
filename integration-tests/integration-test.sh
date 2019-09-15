#!/usr/bin/env bash

source "`dirname \"$0\"`/framework.sh"

# =========================================================================
# INTEGRATION TESTS
# =========================================================================

describe "Basic parsing correctness"
t "2" "2"
t "2.2" "2.2"
t "4+1i" "4.0+1.0i"
t "2.1+3.2i" "2.1+3.2i"
t "-4+1i" "-4.0+1.0i"
t "\"I am a good test\"" "\"I am a good test\""
#TODO fixme
#t "\"I am evil\!\"" "Parse error at \"lisp\" (line 1, column 12):\nunexpected \"!\""

describe "Basic arithmetic primitives"
t "(+ 2 2)" "4"
t "(- 2 2)" "0"
t "(* 2 2)" "4"
t "(/ 2 2)" "1"
t "(modulo 347521 17)" "7"
t "(remainder 347521 17)" "7"
t "(quotient 347521 17)" "20442"

describe "Basic arithmetic primitives on Complex Numbers"
t "(+ 1+1i 1+1i)" "2.0+2.0i"
t "(+ 3+4i 5+7i)" "8.0+11.0i"
t "(- 3+4i 5+7i)" "-2.0-3.0i"
t "(* 3+4i 5+7i)" "-13.0+41.0i"

describe "Basic arithmetic primitives on Floats"
#TODO implement test

describe "Basic arithmetic primitives on Mixed type numbers"
#TODO implement test

describe "Numerical boolean operators"
#TODO implement test

describe "Numerical boolean operators on Complex"
#TODO implement test

describe "Numerical boolean operators on float"
#TODO implement test

describe "Boolean operators"
#TODO implement test

describe "String boolean operators"
#TODO implement test

describe "Type testing functions"
#TODO implement test

describe "Symbol handling primitives"
#TODO implement test

describe "if-clause"
t "(if (> 3 2) 'correct 'wrong)" "correct"
t "(if (> 2 3) 'wrong 'correct)" "correct"
#TODO implement test

describe "case-clause"
#TODO implement test

describe "cond-clause"
#TODO implement test

describe "List primitives"
#TODO implement test

describe "Boolean Equivalence operators"
#TODO implement test

describe "String constructors"
#TODO implement test

describe "String primitives"
t "(string-length \"helloworld\")" "10"
t "(string-length \"\")" "0"

describe "String predicates"
t "(string-null? \"\")" "#t"
t "(string-null? \"ciao\")" "#f"

printf "\n\n$tests_run tests run\n$tests_passed PASSED\n$tests_failed FAILED\n"
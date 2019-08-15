#!/usr/bin/env bash

# From http://eradman.com/posts/ut-shell-scripts.html

# Number of tests run
declare -i tests_run=0
declare -i tests_passed=0
declare -i tests_failed=0

INTERPRETER_NAME="toy-scheme"

# Check if the interpreter is in path
command -v $INTERPRETER_NAME || { echo "Could not find $INTERPRETER_NAME in \$PATH"; exit 1; }

# describe function
# Sets the current test context
function describe { this="$1"; printf "\nTESTING $this\n"; } 

# Display what test failed on ERR
trap 'printf "$0: exit code $? on line $LINENO\nFAIL: $this\n"; exit 1' ERR

# Assertion function
function assert {
    let tests_run+=1
    [ "$1" == "$2" ] && { echo -n "."; let tests_passed+=1; return; }
    printf "\nFAIL: $this\n'$1' != '$2'\n"; let tests_failed+=1;
}

function t {
    assert "`$INTERPRETER_NAME \"$1\"`" "$2"
}

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
t "(mod 347521 17)" "7"
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
#TODO implement test

describe "case-clause"
#TODO implement test

describe "cond-clause"
#TODO implement test

describe "List primitives"
#TODO implement test

describe "Boolean Equivalence operators"
#TODO implement test

describe "String primitives"
#TODO implement test


printf "\n\n$tests_run tests run\n$tests_passed PASSED\n$tests_failed FAILED\n"
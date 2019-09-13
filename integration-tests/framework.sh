#!/usr/bin/env bash

# From http://eradman.com/posts/ut-shell-scripts.html

# Number of tests run
declare -i tests_run=0
declare -i tests_passed=0
declare -i tests_failed=0

INTERPRETER_NAME="yasih -e"

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
    [ -z "$SILENT" ] && printf "\nFAIL: $this\ngot '$1' but expected '$2'\n" || echo -n "f"
    let tests_failed+=1
}

function t {
    assert "`$INTERPRETER_NAME \"$1\"`" "$2"
}
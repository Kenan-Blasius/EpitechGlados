#!/bin/bash

# Run the tests and capture the output
output=$(./glados factorial.lisp)

echo "factorial.lisp Output: $output"
if [ "$output" == "IntAST 120" ]; then
    echo "factorial.lisp Passed"
else
    echo "factorial.lisp Failed"
    exit 1
fi

# Run the tests and capture the output
output=$(./glados if.lisp)

echo "if.lisp Output: $output"
if [ "$output" == "IntAST 11" ]; then
    echo "if.lisp Passed"
else
    echo "if.lisp Failed"
    exit 1
fi

# Run the tests and capture the output
output=$(./glados foo.lisp)

echo "foo.lisp Output: $output"
if [ "$output" == "IntAST 1" ]; then
    echo "foo.lisp Passed"
else
    echo "foo.lisp Failed"
    exit 1
fi

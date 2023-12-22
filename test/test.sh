#!/bin/bash

list_of_files="arithmetic_test.lisp variable_test.lisp max.lisp factorial.lisp if.lisp foo.lisp sum.lisp square.lisp"

expected_outputs=(
    "IntAST 3"
    "IntAST 13"
    "IntAST 6"
    "IntAST 120"
    "IntAST 11"
    "IntAST 1"
    "IntAST 15"
    "IntAST 16"
)

index=0
for file in $list_of_files; do
    echo "Running $file"
    output=$(./glados $file)
    echo "$file Output: $output"
    expected_output=${expected_outputs[$index]}

    if [ "$output" == "$expected_output" ]; then
        echo "$file Passed"
    else
        echo "$file Failed"
        exit 1
    fi

    ((index++))
done

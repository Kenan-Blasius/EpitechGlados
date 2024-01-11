#!/bin/bash

# Navigate to the 'tests' directory
cd tests_examples || exit

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Define the path to the 'glados' executable at the root
GLADOS_PATH="../glados"

all_tests_passed=true

# Iterate over each file in the directory
for file in *.cmm; do
    if [ -f "$file" ]; then

        binary_file="${file%.*}.bin"
        binary_file="file.bin"

        # Execute the './glados' command for each file
        "$GLADOS_PATH" "$file" > /dev/null 2>&1
        echo -e "Test ${BLUE}$file${NC}"

        # Read the first line of the file and extract the value after "//"
        expected_value=$( head -n 1 "$file" | awk -F'//' '{print $2}' | tr -d '[:space:]' )

        # Check if './glados' command was successful
        if [ $? -eq 0 ]; then
            # If successful, execute the './eval' command on the generated binary file
            eval_result=$(../eval "$binary_file" 2>/dev/null | tr -cd '[:digit:]')

            # Display the result of the './eval' command

            # Compare the expected and actual values
            if [ "$eval_result" = "$expected_value" ]; then
                echo -e "${GREEN}Test passed! (returned value $eval_result)${NC}"
            else
                echo -e "${RED}Test failed! for file: $file, Expected: $expected_value, Actual: $eval_result|${NC}"
                all_tests_passed=false
            fi
            echo ""
        else
            echo "Error executing '$GLADOS_PATH' for file: $file"
        fi
    fi
done

if [ "$all_tests_passed" = true ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi

#!/bin/bash

# Navigate to the 'tests' directory
cd tests_examples || exit

# Define the path to the 'glados' executable at the root
GLADOS_PATH="../glados"

# Iterate over each file in the directory
for file in *.cmm; do
    if [ -f "$file" ]; then

        binary_file="${file%.*}.bin"
        binary_file="file.bin"

        # Execute the './glados' command for each file
        "$GLADOS_PATH" "$file" > /dev/null 2>&1
        echo "Test $file"

        # Read the first line of the file and extract the value after "//"
        expected_value=$( head -n 1 "$file" | awk -F'//' '{print $2}' | tr -d '[:space:]' )

        # Check if './glados' command was successful
        if [ $? -eq 0 ]; then
            # If successful, execute the './eval' command on the generated binary file
            eval_result=$(../eval "$binary_file" 2>/dev/null | tr -cd '[:digit:]')

            # Display the result of the './eval' command

            # Compare the expected and actual values
            if [ "$eval_result" = "$expected_value" ]; then
                echo "- Test passed! (returned value $eval_result)"
            else
                echo "- Test failed! for file: $file, Expected: $expected_value, Actual: $eval_result|"
            fi
            echo ""
        else
            echo "Error executing '$GLADOS_PATH' for file: $file"
        fi
    fi
done

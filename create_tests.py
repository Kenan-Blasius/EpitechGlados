import os
import subprocess
import sys

def execute_command(file_path):
    # Construct the command
    command = f"./glados {file_path}"

    try:
        # Execute the command and capture the output
        result = subprocess.run(command, shell=True, capture_output=True, text=True, check=True)

        # Write the output to a file
        output_file_path = f"{file_path}_output.txt"
        with open(output_file_path, 'w') as output_file:
            output_file.write(result.stdout)

        print(f"Command executed successfully for {file_path}. Output written to {output_file_path}")

    except subprocess.CalledProcessError as e:
        print(f"Error executing command for {file_path}. Error: {e}")


def process_files(directory):
    # Iterate over files in the directory
    for filename in os.listdir(directory):
        file_path = os.path.join(directory, filename)

        # Check if it's a file (not a directory)
        if os.path.isfile(file_path):
            execute_command(file_path)


if __name__ == "__main__":
    # Check if the correct number of command-line arguments is provided
    if len(sys.argv) != 2:
        print("Usage: python script.py <directory>")
        sys.exit(1)

    # Get the directory from the command-line argument
    directory = sys.argv[1]

    # Check if the directory exists
    if not os.path.isdir(directory):
        print(f"Error: Directory '{directory}' not found.")
        sys.exit(1)

    # Process files in the directory
    process_files(directory)

import os
import subprocess
import sys

BEGIN_OF_SCRIPT = "test"
BEGIN_OF_SCRIPT2 = "AST :: Test\ntest"
BEGIN_OF_SCRIPT3 = "AST =\n\tTestList\n\t\t[\n\t\t\tlet ast = "
MIDDLE_OF_SCRIPT = "\n\t\t\tin let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)\n\t\t\tin TestCase (assertEqual \"compile, "
MIDDLE_OF_SCRIPT2 = "\" (bytecode) ("
END_OF_SCRIPT = "))\n\t\t]\n"

def execute_command(file_path):
    # Construct the command
    command = f"./glados {file_path}"

    try:
        # Execute the command and capture the output
        result = subprocess.run(command, shell=True, capture_output=True, text=True, check=True)

        # Parse the output and find lines following "AST:" and "Bytecode:"
        output_lines = result.stdout.splitlines()

        ast_index = output_lines.index("AST:") if "AST:" in output_lines else -1
        bytecode_index = output_lines.index("Bytecode:") if "Bytecode:" in output_lines else -1

        # Write the next line after "AST:" to a file
        if ast_index != -1 and ast_index + 1 < len(output_lines):
            ast_output = output_lines[ast_index + 1]

        # Write the next line after "Bytecode:" to a file
        if bytecode_index != -1 and bytecode_index + 1 < len(output_lines):
            bytecode_output = output_lines[bytecode_index + 1]

        output_file_path = f"output.txt"

        name_test = file_path.split("/")[-1].split(".")[0]

        with open(output_file_path, 'a') as output_file_path:
            output_file_path.write(BEGIN_OF_SCRIPT)
            output_file_path.write(name_test)
            output_file_path.write(BEGIN_OF_SCRIPT2)
            output_file_path.write(name_test)
            output_file_path.write(BEGIN_OF_SCRIPT3)
            output_file_path.write(ast_output)
            output_file_path.write(MIDDLE_OF_SCRIPT)
            output_file_path.write(name_test)
            output_file_path.write(MIDDLE_OF_SCRIPT2)
            output_file_path.write(bytecode_output)
            output_file_path.write(END_OF_SCRIPT)

        print(f"Command executed successfully for {file_path}")

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

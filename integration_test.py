import subprocess

def run_command(command):
    try:
        # Run the command and capture the output
        result = subprocess.run(command, text=True, capture_output=True, shell=True)
        
        # Get the stdout and stderr
        stdout = result.stdout
        stderr = result.stderr
        
        return stdout, stderr, result.returncode
    except Exception as e:
        return None, str(e), -1

def read_file(file_path):
    try:
        with open(file_path, 'r') as file:
            content = file.read()
        return content, None
    except Exception as e:
        return None, str(e)

def run_test(file_name):
    psx_file_name = file_name+".psx"
    expected_path = "./test/integration_test/expected_files/"+file_name+".c"
    actual_path ="./compile/c_output/output_"+file_name+".c"

    # Execute the the test
    stdout, stderr, status = run_command("make int_test " + psx_file_name)
    print(stdout)
    if stderr:
        print("Error while executing make command:", stderr)

    # Read the file
    actual_content, actual_error = read_file(actual_path)
    expected_content, expected_error = read_file(expected_path)
    
    if actual_error:
        print("Error while reading actual file:", actual_error)
    elif expected_error:
        print("Error while reading expected file:", expected_error)

    else:
        # Check if the specific string is part of the file content
        if expected_content in actual_content:
            print("Specific output block found in file content.")
            print("test passed")
        else:
            print("Specific output block not found in file content.")
    
    print("Exit status:", status)
    

def main():
    
    # Make main
    stdout, stderr, status = run_command("make all")
    print(stdout)
    
    if stderr:
        print("Error while executing make command:", stderr)

    run_test("test_sum")
    
    

if __name__ == "__main__":
    main()
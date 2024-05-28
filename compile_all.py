import subprocess
import argparse

def run_powershell_command(command):
    try:
        # Run the command and capture the output
        result = subprocess.run(["powershell", "-Command", command], capture_output=True, text=True)
        
        # Get the stdout and stderr
        stdout = result.stdout
        stderr = result.stderr
        
        return stdout, stderr, result.returncode
    except Exception as e:
        return None, str(e), -1

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

def main(filename):

    # Make c to psyq
    stdout, stderr, status = run_command(f"make main {filename}.psx")
    print(stdout)

    if stderr:
        print("Error while executing make command:", stderr)
    
    # Make c to psyq
    stdout, stderr, status = run_powershell_command(f"make -f psyqcompiler filename={filename}")
    print(stdout)
    
    if stderr:
        print("Error while executing make command:", stderr)

    
    

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run commands with given filename.')
    parser.add_argument('filename', type=str, help='The filename to use in the commands')
    args = parser.parse_args()
    
    main(args.filename)
import subprocess
import time
import sys

def measure(path_to_exe):
  start = time.time()
  subprocess.run(path_to_exe, shell=True)  # Run the executable
  stop = time.time()
  result = stop - start
  return result

if __name__ == "__main__":
  if len(sys.argv) != 2:
    sys.exit(1)

  path_to_exe = sys.argv[1]
  result = measure(path_to_exe)
  print(f"Execution time: {result:.4f} seconds")
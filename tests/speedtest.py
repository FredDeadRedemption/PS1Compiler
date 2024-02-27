import subprocess
import time
import sys

print("py ran")

def measure(path_to_exe, sys_arg):
  print("py " + path_to_exe + sys_arg)
  start = time.time()
  subprocess.run([path_to_exe, sys_arg])  # Run the executable
  stop = time.time()
  result = stop - start
  return result

if __name__ == "__main__":
  if len(sys.argv) != 3:
    sys.exit(1)

  path_to_exe = sys.argv[1]
  sys_arg = sys.argv[2]
  result = measure(path_to_exe, sys_arg)
  print(f"Execution time: {result:.4f} seconds")
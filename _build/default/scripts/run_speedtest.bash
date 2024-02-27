export PATH="$(pwd)/.venv/bin:$PATH"

echo "arg1= "$1
echo "arg2= "$2

python tests/speedtest.py "$1" "$2" # $1 = sys arg 1
export PATH="$(pwd)/.venv/bin:$PATH"

python tests/lextest.py "$1" # $1 = sys arg 1
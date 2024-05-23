.PHONY: all clean test lexspeedtest purge main
# $(filter-out ./psyq/% ./includes/%, $(wildcard **/*.ml **/*.mll **/*.mli))
# Update the 'all' target to compile all files except those from the "psyq" and "includes" folders
all:
	dune build
	@echo "Build completed!"

clean: # rm -rf /build
	@echo "Ryder op..."
	dune clean

test: clean
	dune runtest

lexspeedtest: 
	make speedtest EXE="_build/default/src/frontend/lexer/lexer.exe" PSX="code.psx"

speedtest: #EXE to test #PSC path to code for exe arg
	source scripts/run_speedtest.bash $(EXE) $(PSX)

purge: #convert bash script notation from dos to unix
	dos2unix scripts/run_speedtest.bash

# Define the default target
.PHONY: default
default: run_main

# Define a rule for running main.exe with a given .psx file
main: # run main.exe with a specified .psx file
	cd _build/default/src && ./main.exe ../../../$(filename)
	$(MAKE) snake_demo_output

# Extract the second word from the make command line arguments
second_word = $(word 2,$(MAKECMDGOALS))

# Check if the second word exists and set the filename
ifeq ($(filter main,$(MAKECMDGOALS)),main)
	ifneq ($(second_word),)
		filename := $(second_word)
	endif
endif


# Run the second Makefile with the generated filename
snake_demo_output:
	$(MAKE) -f psyqcompiler
	$(MAKE) -f psyqcompiler clean

# Prevent make from interpreting "filename.psx" as a file target
%:
	@:





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

filename := some_value

# Extract the second word from the make command line arguments
second_word = $(word 2,$(MAKECMDGOALS))

# Check if the second word exists and set the filename
ifeq ($(filter main,$(MAKECMDGOALS)),main)
	ifneq ($(second_word),)
		filename := $(second_word)
	endif
endif

# Check if the second word exists and set the filename
ifeq ($(filter int_test,$(MAKECMDGOALS)),int_test)
	ifneq ($(second_word),)
		filename := $(second_word)
	endif
endif


# Define a rule for running main.exe with a given .psx file
main: # run main.exe with a specified .psx file
	cd _build/default/src && ./main.exe ../../../compile/input/$(filename)


int_test: # run main.exe with a specified .psx file
	cd _build/default/src && ./main.exe ../../../test/integration_test/psx_files/$(filename)
# $(MAKE) -f psyqcompiler filename=$(filename)

	
# $(MAKE) -f psyqcompiler clean

# Prevent make from interpreting "filename.psx" as a file target
%:
	@:





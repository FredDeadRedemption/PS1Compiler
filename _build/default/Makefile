.PHONY: all clean lex relax lextest # this tells Make that these targets (all & clean) do not represent files

all: # compile all .ml -mll -mli
	dune build
	@echo "build samlet :D!"

clean: # rm -rf /build
	@echo "Ryder op..."
	dune clean

lex: # lexes code.psx
	cd _build/default/src/frontend/lexer && ./lexer.exe ../../../code.psx

relax: clean all lex # clean --> recompile --> lex
	@echo "Cleaning, building, and lexing completed."

SHELL := /bin/bash

lextest: 
	make speedtest EXE="_build/default/src/frontend/lexer/lexer.exe" PSX="code.psx"

speedtest: #EXE to test #PSC path to code for exe arg
	source scripts/run_speedtest.bash $(EXE) $(PSX)

purge: #convert bash script notation from dos to unix
	dos2unix scripts/run_speedtest.bash 
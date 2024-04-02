.PHONY: all clean main lexspeedtest purge main main2 main3# this tells Make that these targets (all & clean) do not represent files

all: # compile all .ml -mll -mli
	dune build
	@echo "build samlet :D!"

clean: # rm -rf /build
	@echo "Ryder op..."
	dune clean

main:
	cd _build/default/src && ./main.exe ../../../code.psx

main2:
	cd _build/default/src && ./main.exe ../../../code2.psx

main3:
	cd _build/default/src && ./main.exe ../../../code3.psx

test:
	cd _build/default/src && ./main.exe ../../../tests/expr.psx && ./main.exe ../../../tests/functions.psx




## BENCHMARKING SCRIPTS
SHELL := /bin/bash

lexspeedtest: 
	make speedtest EXE="_build/default/src/frontend/lexer/lexer.exe" PSX="code.psx"

speedtest: #EXE to test #PSC path to code for exe arg
	source scripts/run_speedtest.bash $(EXE) $(PSX)

purge: #convert bash script notation from dos to unix
	dos2unix scripts/run_speedtest.bash 
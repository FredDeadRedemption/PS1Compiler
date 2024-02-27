.PHONY: all clean lex relax # this tells Make that these targets (all & clean) do not represent files

all: # compile all .ml -mll -mli
	dune build
	@echo "build samlet :D!"

clean: # rm -rf /build
	@echo "Ryder op..."
	dune clean

lex: # lexes code.psx
	cd _build/default/src/frontend/lexer && ./lexer.exe

relax: clean all lex # clean --> recompile --> lex
	@echo "Cleaning, building, and lexing completed."
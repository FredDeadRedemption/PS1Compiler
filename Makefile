.PHONY: all clean lex # this tells Make that these targets (all & clean) do not represent files

all: # compile all .ml -mll -mli
	dune build
	@echo "build samlet :D!"

clean: # rm -rf /build
	@echo "Ryder op..."
	dune clean

lex: # lexes code.psx
	cd _build/default/src/frontend/parser && ./lexer.exe

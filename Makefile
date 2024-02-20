.PHONY: all clean # this tells Make that these targets do not represent files

all: # compile all .ml -mll -mli
	dune build

clean: # rm -rf all
	@echo "Cleaning up..."
	dune clean

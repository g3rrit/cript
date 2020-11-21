# create opam switch with:
# opam switch create . opam-base-compiler.4.10.0

all:
	dune build app/cript.exe
	./_build/default/app/cript.exe -c clang test/example.cr

# install necessary packages
init:
	opam install merlin ocp-indent dune utop core menhir ppx_deriving

clean: 
	rm -rf ./_build/*
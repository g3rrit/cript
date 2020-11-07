# create opam switch with:
# opam switch create . opam-base-compiler.4.10.0

all:
	dune build app/mlc.exe
	./_build/default/app/mlc.exe -c clang test/example.mlc

# install necessary packages
init:
	opam install merlin ocp-indent dune utop core menhir ppx_deriving

clean: 
	rm -rf ./_build/*
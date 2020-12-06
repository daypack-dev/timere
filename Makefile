SRCFILES = src/*.ml src/*.mli debug/*.ml tests/*.ml fuzz/*.ml

OCAMLFORMAT = ocamlformat \
	--inplace \
	$(SRCFILES)

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: lib
lib :
	dune build src

.PHONY: test
test :
	OCAMLRUNPARAM=b dune exec ./tests/main.exe

.PHONY: covtest
covtest :
	rm -rf _coverage
	rm -rf bisect*.coverage
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html

.PHONY: debug
debug :
	dune exec ./debug/main.exe

.PHONY: doc
doc :
	dune build @doc

.PHONY: format
format :
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY: cinaps
cinaps :
	cinaps -i $(SRCFILES)
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY: fuzz-resolver
fuzz-resolver :
	mkdir -p fuzz-resolver-input
	echo "abcd" > fuzz-resolver-input/dummy
	mkdir -p fuzz-resolver-output
	afl-fuzz -t 1000 -i fuzz-resolver-input -o fuzz-resolver-output ./_build/default/fuzz/resolver_is_same_as_simple_resolver.exe @@

.PHONY : clean
clean:
	dune clean

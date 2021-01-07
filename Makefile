SRCFILES = src/*.ml src/*.mli parse/*.ml parse/*.mli debug/*.ml tests/*.ml fuzz/*.ml gen/*.ml

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
test : lib
	OCAMLRUNPARAM=b dune exec ./tests/main.exe

.PHONY: covtest
covtest : lib
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html

.PHONY: debug
debug : lib
	dune exec ./debug/main.exe

.PHONY: debug-parse
debug-parse : lib
	dune exec ./debug-parse/main.exe

.PHONY: doc
doc :
	dune build @doc

.PHONY: format
format :
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY: gen
gen :
	cd gen/ && dune build gen.exe
	dune exec gen/gen.exe

.PHONY: cinaps
cinaps :
	cinaps -i $(SRCFILES)
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean

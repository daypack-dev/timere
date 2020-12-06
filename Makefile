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
	BISECT_ENABLE=yes OCAMLRUNPARAM=b dune exec ./tests/main.exe
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

.PHONY : clean
clean:
	dune clean

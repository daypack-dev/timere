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

.PHONY: fuzz-of-sexp-string
fuzz-of-sexp-string : lib
	mkdir -p fuzz-of-sexp-string-input
	echo "abcd" > fuzz-of-sexp-string-input/dummy
	mkdir -p fuzz-of-sexp-string-output
	afl-fuzz -t 1000 -i fuzz-of-sexp-string-input -o fuzz-of-sexp-string-output ./_build/default/fuzz/of_sexp_string_does_not_crash.exe @@

.PHONY: fuzz-to-of-sexp
fuzz-to-of-sexp : lib
	mkdir -p fuzz-to-of-sexp-input
	echo "abcd" > fuzz-to-of-sexp-input/dummy
	mkdir -p fuzz-to-of-sexp-output
	afl-fuzz -t 1000 -i fuzz-to-of-sexp-input -o fuzz-to-of-sexp-output ./_build/default/fuzz/to_of_sexp.exe @@

.PHONY: fuzz-resolver
fuzz-resolver : lib
	mkdir -p fuzz-resolver-input
	echo "abcd" > fuzz-resolver-input/dummy
	mkdir -p fuzz-resolver-output
	afl-fuzz -t 1000 -i fuzz-resolver-input -o fuzz-resolver-output ./_build/default/fuzz/resolver_is_same_as_simple_resolver.exe @@

.PHONY : clean
clean:
	dune clean

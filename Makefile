SRCFILES = src/*.ml src/*.mli parse/*.ml parse/*.mli debug/*.ml tests/*.ml fuzz/*.ml gen/*.ml gen-build/*.ml tzdb-*/*.ml tzdb-*/*.mli export-js-tzdb-full/*.ml

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

.PHONY: corpus-timeres
corpus-timeres:
	dune exec ./corpus/timeres.exe > corpus-outputs/timeres.txt

.PHONY: corpus-date-times
corpus-date-times:
	dune exec ./corpus/date_times.exe > corpus-outputs/date-times.txt

.PHONY: corpus-hmss
corpus-hmss:
	dune exec ./corpus/hmss.exe > corpus-outputs/hmss.txt

.PHONY: corpus-durations
corpus-durations:
	dune exec ./corpus/durations.exe > corpus-outputs/durations.txt

.PHONY: doc
doc :
	dune build @doc

.PHONY: format
format :
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY: gen
gen :
	cd gen/ && dune build gen_time_zone_data.exe
	dune exec gen/gen_time_zone_data.exe

.PHONY: export-js-tzdb-full
export-js-tzdb-full :
	dune build export-js-tzdb-full/export.bc.js
	dune build export-js-tzdb-full/export_all.bc.js

.PHONY: export-js-tzdb-none
export-js-tzdb-none :
	dune build export-js-tzdb-none/export.bc.js
	dune build export-js-tzdb-none/export_all.bc.js

.PHONY: cinaps
cinaps :
	cinaps -i $(SRCFILES)
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean

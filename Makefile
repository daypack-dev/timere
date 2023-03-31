SRCFILES = timedesc/*.ml timedesc/*.mli \
					 timere/*.ml timere/*.mli \
					 timere-parse/*.ml timere-parse/*.mli	\
					 corpus/*.ml debug/*.ml \
					 timedesc-tests/*.ml \
					 timere-tests/*.ml \
					 fuzz/*.ml \
					 gen/*.ml \
					 timedesc-tzdb/*/*.ml timedesc-tzdb/*.mli \
					 timedesc-tzlocal/*/*.ml timedesc-tzlocal/*.mli \
					 timedesc-json/*.ml timedesc-json/*.mli \
					 timedesc-sexp/*.ml timedesc-sexp/*.mli \
					 export-js-tzdb-full/*.ml

OPAMFILES = *.opam

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: timedesc
timedesc :
	dune build timedesc/

.PHONY: timedesc-json
timedesc-json :
	dune build timedesc-json

.PHONY: timere
timere :
	dune build timere

.PHONY: timedesc-tests
timedesc-tests : timedesc
	OCAMLRUNPARAM=b dune exec timedesc-tests/main.exe --no-buffer --force

.PHONY: timere-tests
timere-tests : timere
	OCAMLRUNPARAM=b dune exec timere-tests/main.exe --no-buffer --force

.PHONY: cov-timedesc-tests
cov-timedesc-tests : timedesc
	find . -name '*.coverage' | xargs rm -f
	dune exec timedesc-tests/main.exe --instrument-with bisect_ppx --no-buffer --force timedesc/
	bisect-ppx-report html

.PHONY: cov-timere-tests
cov-timere-tests : timere
	find . -name '*.coverage' | xargs rm -f
	OCAMLRUNPARAM=b dune exec timere-tests/main.exe --instrument-with bisect_ppx --no-buffer --force timere/
	bisect-ppx-report html

.PHONY: debug
debug : lib
	dune exec debug/main.exe

.PHONY: debug-parse
debug-parse : lib
	dune exec debug-parse/main.exe

.PHONY: corpus
corpus: corpus-timeres corpus-date-times corpus-hmss corpus-spans

.PHONY: corpus-timeres
corpus-timeres:
	dune exec corpus/timeres.exe > corpus-outputs/timeres.txt

.PHONY: corpus-date-times
corpus-date-times:
	dune exec corpus/date_times.exe > corpus-outputs/date-times.txt

.PHONY: corpus-hmss
corpus-hmss:
	dune exec corpus/hmss.exe > corpus-outputs/hmss.txt

.PHONY: corpus-spans
corpus-spans:
	dune exec corpus/spans.exe > corpus-outputs/spans.txt

.PHONY: doc
doc :
	dune build @doc

.PHONY: format
format :
	$(OCPINDENT)

.PHONY: gen
gen :
	dune exec gen/gen_time_zone_data.exe -- 1970 2040 full

.PHONY: export-js-tzdb-full
export-js-tzdb-full :
	dune build export-js-tzdb-full/export.bc.js
	dune build export-js-tzdb-full/export_all.bc.js

.PHONY: export-js-tzdb-none
export-js-tzdb-none :
	dune build export-js-tzdb-none/export.bc.js
	dune build export-js-tzdb-none/export_all.bc.js

.PHONY : clean
clean:
	dune clean

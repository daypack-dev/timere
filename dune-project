(lang dune 2.9)

(license "MIT")
(authors "Daypack developers")
(maintainers "Darren Ldl <darrenldldev@gmail.com>")
(source (github daypack-dev/timere))
(homepage "https://github.com/daypack-dev/timere")

(generate_opam_files true)

(name timere)

(package
  (name timedesc)
  (synopsis "OCaml date time handling library")
  (description "
Features:

- Timestamp and date time handling with platform independent time zone support

  - Subset of the IANA time zone database is built into this library

- Supports Gregorian calendar date, ISO week date, and ISO ordinal date

- Supports nanosecond precision

- ISO8601 parsing and RFC3339 printing
"
)
  (depends 
    (ocaml (>= "4.08"))
    (bisect_ppx (and :dev (>= "2.5.0")))
    dune
    (timedesc-tzdb (= :version))
    (timedesc-tzlocal (= :version))
    seq
    (angstrom (>= "0.15.0"))
    ptime
    (result (>= "1.5"))
    (crowbar :with-test)
    (alcotest :with-test)
    (qcheck-alcotest :with-test)
    (qcheck :with-test)
  )
)

(package
  (name timedesc-tzdb)
  (synopsis "Virtual library for Timedesc time zone database backends")
  (description "
Concrete implementations available in this package:

- `timedesc-tzdb.full`

- `timedesc-tzdb.none`
")
  (depends
    (ocaml (>= "4.08"))
    dune
  )
)

(package
  (name timedesc-tzlocal)
  (synopsis "Virtual library for Timedesc local time zone detection backends")
  (description "
Concrete implementations available in this package:

- `timedesc-tzlocal.unix`

- `timedesc-tzlocal.none`

- `timedesc-tzlocal.utc`

See timedesc-tzlocal-js for JavaScript backend
")
  (depends
    (ocaml (>= "4.08"))
    dune
  )
)

(package
  (name timedesc-tzlocal-js)
  (synopsis "JS implementation for timedesc-tzlocal")
  (description "
JavaScript implementation of timedesc-tzlocal
")
  (depends
    (ocaml (>= "4.08"))
    dune
    (timedesc-tzlocal (= :version))
    (js_of_ocaml (>= "4.0.0"))
    js_of_ocaml-ppx
  )
)

(package
  (name timedesc-json)
  (synopsis "Timedesc JSON backend")
  (description "
This package provides JSON serialization functions for various Timedesc values
")
  (depends
    (ocaml (>= "4.08"))
    dune
    (timedesc (= :version))
    (yojson (>= "1.6.0"))
  )
)

(package
  (name timedesc-sexp)
  (synopsis "Timedesc Sexp backend")
  (description "
This package provides sexp serialization functions for various Timedesc values
")
  (depends
    (ocaml (>= "4.08"))
    dune
    (timedesc (= :version))
    (sexplib (>= "v0.14.0"))
  )
)

(package
  (name timere)
  (synopsis "OCaml date time reasoning library")
  (description "
Features:

- Reasoning over time intervals via `timere` objects/expressions, examples:

  - Pattern matching time and intervals. These work across DST boundaries.
 
  - Intersection and union

  - Chunking at year or month boundary, or in fixed sizes

  - Evaluate (sub)expressions with a different time zone (e.g. intersection of 9am to 5pm of Sydney and 9am to 5pm of New York)
"
)
  (depends 
    (ocaml (>= "4.08"))
    (bisect_ppx (and :dev (>= "2.5.0")))
    dune
    seq
    (oseq (>= "0.3"))
    (containers (>= "3.6"))
    fmt
    (timedesc (>= "0.9.0"))
    (timedesc-sexp (>= "0.9.0"))
    (diet (>= "0.4"))
    (crowbar :with-test)
    (alcotest :with-test)
    (qcheck-alcotest :with-test)
    (qcheck :with-test)
  )
)

(package
  (name timere-parse)
  (synopsis "OCaml date time and duration natural language parsing library")
  (description "
(WIP) Facilities for parsing natural language expressions into

- Timere objects

- Date time

- Hour minute second

- Duration
"
)
  (depends 
    (ocaml (>= "4.08"))
    (bisect_ppx (and :dev (>= "2.5.0")))
    dune
    (oseq (>= "0.3"))
    (re (>= "1.9.0"))
    (containers (>= "3.6"))
    (timedesc (>= "0.8.0"))
    (timere (>= "0.8.0"))
    (mparser (>= "1.2.3"))
    (crowbar :with-test)
    (alcotest :with-test)
    (qcheck-alcotest :with-test)
    (qcheck :with-test)
  )
)

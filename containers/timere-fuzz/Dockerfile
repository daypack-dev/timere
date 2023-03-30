FROM docker.io/ocaml/opam:debian-ocaml-4.14-afl
USER root
RUN apt-get update
RUN apt-get install --yes pkg-config
RUN apt-get install --yes afl
RUN opam install dune containers fmt
RUN opam install re ptime oseq seq diet
RUN opam install yojson fileutils
RUN opam install alcotest crowbar
RUN opam install mparser angstrom
RUN opam install sexplib

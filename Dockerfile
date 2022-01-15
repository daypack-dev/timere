FROM docker.io/ocaml/opam
USER root
RUN opam install dune containers fmt
RUN opam install mparser re ptime oseq seq diet
RUN opam install yojson fileutils
RUN opam install utop ocamlformat

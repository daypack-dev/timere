#!/usr/bin/env bash
podman run -it \
  -v ~/timere:/home/opam/timere \
  -v $TZDIR:/usr/share/zoneinfo \
  --workdir /home/opam/timere \
  --rm \
  localhost/timere

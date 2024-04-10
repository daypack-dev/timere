#!/usr/bin/env bash
podman run -it \
  -v ~/timere:/home/opam/timere \
  -v $TZDIR:/usr/share/zoneinfo \
  --userns keep-id:uid=1000,gid=1000 \
  --workdir /home/opam/timere \
  --rm \
  localhost/timere

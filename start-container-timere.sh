#!/bin/bash
podman run -it \
  -v ~/timere:/home/opam/timere \
  -v /usr/share/zoneinfo:/usr/share/zoneinfo \
  --userns keep-id:uid=1000,gid=1000 \
  --workdir /home/opam/timere \
  --rm \
  localhost/timere

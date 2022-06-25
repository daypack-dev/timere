#!/bin/bash

tag=$(cat CHANGELOG.md \
  | grep '## Timedesc' \
  | head -n 1 \
  | sed -n 's/^## Timedesc\s*\(\S*\)$/\1/p')

echo "Detected tag for Timedesc:" $tag

read -p "Is the tag correct [y/n]? " ans

if [[ $ans != "y" ]]; then
  echo "Publishing cancelled"
fi

#!/bin/bash

opam_repo="$HOME/opam-repository"

echo "Checking if $opam_repo exists"

if [ ! -d "$opam_repo" ]; then
  echo "$opam_repo does not exist"
  exit 1
fi

ver=$(cat CHANGELOG.md \
  | grep '## Timedesc' \
  | head -n 1 \
  | sed -n 's/^## Timedesc\s*\(\S*\)$/\1/p')

echo "Detected version for Timedesc:" $ver

git_tag="timedesc-$ver"

echo "Computed git tag for Timedesc:" $git_tag

read -p "Are the version and git tag correct [y/n]? " ans

if [[ $ans != "y" ]]; then
  echo "Publishing cancelled"
  exit 0
fi

echo "Checking if $git_tag exists in repo already"

if [[ $(git tag -l "$git_tag") == "" ]]; then
  echo "Tagging commit"
  git tag "$git_tag"
fi

echo "Pushing all tags"

git push --tags

echo "Creating directory in $opam_repo"

mkdir -p "$opam_repo"/packages/timedesc/timedesc."$ver"

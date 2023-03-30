#!/bin/bash

script_dir=$(dirname $(readlink -f "$0"))

rm -rf "$script_dir"/../fuzz-*-input
rm -rf "$script_dir"/../fuzz-*-output
rm -f "$script_dir"/core.*
rm -f ./core.*

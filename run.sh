#!/bin/sh

set -e

(
  cd "$(dirname "$0")"
  dune build --build-dir ./build --release
)

exec ./build/default/main.exe "$@"

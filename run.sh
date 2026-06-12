#!/bin/sh


set -e # Exit early if any commands fail

(
  cd "$(dirname "$0")" # Ensure compile steps are run within the repository directory
  dune build --build-dir /tmp/codecrafters-build-interpreter-ocaml
)

exec /tmp/codecrafters-build-interpreter-ocaml/default/main.exe "$@"

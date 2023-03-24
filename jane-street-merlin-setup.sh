#!/bin/bash

cd "$(dirname "$0")"

ocamlmerlin="$(which ocamlmerlin 2>&-)"
ocamlmerlin_status=$?
if [[ $ocamlmerlin_status -ne 0 ]]; then
  echo >&2 'No ocamlmerlin on $PATH'
  exit $ocamlmerlin_status
fi

set -e

merlin_dir="$(dirname "$ocamlmerlin")"
echo "$merlin_dir" > .local-merlin-binaries
ocamlc -where > .local-ocaml-lib

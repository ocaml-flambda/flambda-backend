#!/bin/bash

target_dir=$PWD
cd "$(dirname "$0")"

ocamlmerlin="$(which ocamlmerlin 2>&-)"
ocamlmerlin_status=$?
if [[ $ocamlmerlin_status -ne 0 ]]; then
  exit $ocamlmerlin_status
fi

set -e

merlin_dir="$(dirname "$ocamlmerlin")"
echo "$merlin_dir" > "$target_dir"/.local-merlin-binaries
ocamlc -where > "$target_dir"/.local-ocaml-lib

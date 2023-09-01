#!/usr/bin/env bash

set -euo pipefail

# Build Jane Syntax files with the opam switch's OCaml. This verifies
# that they can be put into public release with minimal changes;
# see Note [Buildable with upstream] in ocaml/parsing/jane_syntax.mli

cd $(git rev-parse --show-toplevel)/ocaml

files_in_dependency_order=(
  utils/language_extension_kernel.{mli,ml}
  language_extension.ml
  parsing/jane_asttypes.mli
  parsing/{jane_syntax_parsing,jane_syntax}.{mli,ml}
)

function basenames_in_dependency_order() {
  printf "%s\n" "${files_in_dependency_order[@]}" | xargs -n 1 basename
}

# language_extension.ml is stubbed later, so we don't copy anything
# from ocaml/ for it.
function source_files_in_ocaml_dir() {
  printf "%s\n" "${files_in_dependency_order[@]}" | grep -v '^language_extension.ml$'
}

# Copy files to another directory before compiling so we're confident
# we're not using the build artifacts of an earlier build.
tmp=$(mktemp -d)
cp $(source_files_in_ocaml_dir) "$tmp"

cd "$tmp"

# Stub [Language_extension] in the same way we do internally.
cat > language_extension.ml <<EOF
include Language_extension_kernel
let is_enabled _ = true
let is_at_least _ _ = true
EOF

if ! ocamlc -stop-after typing -I +compiler-libs \
  $(basenames_in_dependency_order)
then
  echo "Jane Syntax files failed to build with upstream OCaml"
  echo "See Note [Buildable with upstream] in ocaml/parsing/jane_syntax.mli"
  exit 2
else
  # Print the typechecked interfaces to support the assertion that this check
  # is actually checking anything.
  echo "Jane Syntax files built with upstream OCaml:"
  ocamlc -i -I +compiler-libs $(basenames_in_dependency_order)
fi

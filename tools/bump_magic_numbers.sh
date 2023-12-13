#!/bin/bash

set -eu -o pipefail

# Needs GNU sed which is not the default on macOS
sed=gsed
gsed --version >/dev/null || sed=sed

if [ "$(git status --porcelain --untracked-files=no)" != "" ]; then
  echo "There must be a clean 'git status' before running this script"
  exit 1
fi

git clean -dfX
(cd ocaml && ./configure && make -j8 coldstart && make -j8 coreall)

# Unfortunately the "e" modifier appears not to work except for replacing
# a whole line

$sed -i \
  -e 's/^#define EXEC_MAGIC "Caml1999X\([0-9][0-9][0-9]\)"/echo "#define EXEC_MAGIC \\x22Caml1999X$((\1 + 1))\\x22"/ge' \
  ocaml/runtime/caml/exec.h

$sed -i \
  -e 's/^\(let\|and\) \([a-z][a-z_]*\) = "Caml\([0-9][0-9][0-9][0-9]\)\([a-zA-Z]\)\(5[0-9][0-9]\)"/echo "\1 \2 = \\x22Caml\3\4$((\5 + 1))\\x22"/ge' \
  ocaml/utils/config.mlp

$sed -i \
  -e 's/^    "Caml\([0-9][0-9][0-9][0-9]\)\([a-zA-Z]\)\(5[0-9][0-9]\)"/echo "    \\x22Caml\1\2$((\3 + 1))\\x22"/ge' \
  ocaml/utils/config.mlp

(cd ocaml && make -j8 coreall && make -j8 bootstrap)

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

# When `.local-merlin-binaries` is fully supported (rather than just supported
# in dev Emacs), we can drop the extra directory `.for-jane-street-merlin` as
# well as the stub `jenga.conf` file.

mkdir -p .for-jane-street-merlin
ln -sfn "$merlin_dir" .for-jane-street-merlin/dev
ln -sfn "$merlin_dir" .for-jane-street-merlin/prod

echo "$PWD/.for-jane-street-merlin" > .merlin-binaries
cp .local-ocaml-lib .ocaml-lib

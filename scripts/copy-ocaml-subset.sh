#!/bin/bash

# To be run from the root of the Flambda backend repo

set -eu -o pipefail

# Delete folders if they already exists:
rm -rf backend driver file_formats middle_end native_toplevel

# Import them from ocaml
cp -r ocaml/asmcomp backend
cp -r ocaml/driver driver
cp -r ocaml/file_formats file_formats
cp -r ocaml/middle_end middle_end
cp -r ocaml/toplevel native_toplevel

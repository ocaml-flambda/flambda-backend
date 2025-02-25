#!/bin/sh

set -eu

case ${1:-} in
  "--check")
    OUTPUT="test_native.corrected.ml"
    CHECKING="true"
  ;;
  "")
    OUTPUT="test_native.ml"
    CHECKING=""
  ;;
  *)
    echo "unrecognized option $1"
    exit 1
  ;;
esac

sed '
s/_byte/_native/g
s/ocamlc/ocamlopt/g
# Oops, we changed ocamlc_byte to ocamlopt_native, so fix that
s/ocamlopt_native/ocamlopt_byte/g
s/\.cmo/.cmx/g
s/\.bc/.exe/g
# Do this last to avoid replacing the _byte:
s/TEST/TEST (* DO NOT EDIT. Instead edit test_byte.ml and run gen-native.sh. *)/
' test_byte.ml > "$OUTPUT"

if [ -n "$CHECKING" ]; then
  diff -q test_native.ml "$OUTPUT" && rm "$OUTPUT"
fi

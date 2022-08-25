#!/bin/bash

# Format the input dune file in place

set -eu -o pipefail


if [ $# != 1 ]; then
  echo "syntax: $0 PATH_TO_DUNE_FILE"
  exit 1
fi

file=$1
file_formatted="$file.formatted"
echo "Formatting $file"
dune format-dune-file "$file" > "$file.formatted"
mv "$file_formatted" "$file"

#!/bin/sh

# To be run from the root of the Flambda backend repo

set -eu -o pipefail

if [ $# != 1 ]; then
  echo "syntax: $0 REVISION"
  exit 1
fi

rev=$1

subtree=ocaml/

temp=$(mktemp -d)
mkdir -p $temp

patchfile=$(git format-patch -p --src-prefix=a/$subtree --dst-prefix=b/$subtree -o $temp $rev -1)
git am $patchfile

rm -rf $temp

#!/bin/sh

# To be run from the root of the Flambda backend repo

set -e -u -o pipefail

dump_dir="`pwd`/_profile"
summary_path="`pwd`/summary.csv"

export OLD_OCAMLPARAM="${OCAMLPARAM:-}"
export OCAMLPARAM="_,profile=1,dump-into-csv=1,dump-dir=$dump_dir,regalloc=irc"
export BUILD_OCAMLPARAM="$OCAMLPARAM"

revert_env_variables() {
  export OCAMLPARAM="$OLD_OCAMLPARAM"
  unset OLD_OCAMLPARAM
}
trap revert_env_variables EXIT

build_compiler() {
  git clean -Xdf
  autoconf
  ./configure --enable-ocamltest --enable-warn-error --enable-dev --prefix=`pwd`/_install
  make install
}

build_compiler
python3 ./scripts/combine-profile-information.py "$dump_dir" -o "$summary_path"
rmdir "$dump_dir"

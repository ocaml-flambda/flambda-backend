#!/bin/sh

set -eu -o pipefail

prefix=/Users/mark/dev/mshinwell-ocaml4-install
dune=/Users/mark/dev/dune-dev/_build/default/bin/dune.exe

first_stage_install=$(pwd)/_build/install/default

./configure -C --prefix=$first_stage_install
$dune build --verbose @install

./configure -C --prefix=$prefix
export PATH=$first_stage_install/bin:$PATH
$dune build --build-dir=_build2 --verbose @install

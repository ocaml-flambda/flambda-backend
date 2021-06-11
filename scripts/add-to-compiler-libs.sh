#!/bin/sh

# For each input file name (without extension) output the lines
# that should be added to top-level dune file

set -eu -o pipefail

function add_to_distro () {
    file=$1
    echo "    ($file.mli as compiler-libs/$file.mli)"
    echo "    (.ocamloptcomp.objs/native/$file.cmx as compiler-libs/$file.cmx)"
    echo "    (.ocamloptcomp.objs/byte/$file.cmi as compiler-libs/$file.cmi)"
    echo "    (.ocamloptcomp.objs/byte/$file.cmo as compiler-libs/$file.cmo)"
    echo "    (.ocamloptcomp.objs/byte/$file.cmt as compiler-libs/$file.cmt)"
    echo "    (.ocamloptcomp.objs/byte/$file.cmti as compiler-libs/$file.cmti)"
}

for file in $@ ; do
    add_to_distro $file
done

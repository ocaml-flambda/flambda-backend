#!/bin/sh

# To be run from the root of the Flambda backend repo

git clean -Xdf
autoconf
./configure --enable-ocamltest --enable-warn-error --enable-dev --prefix=`pwd`/_install
make install

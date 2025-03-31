# Script to compare two versions of the compiler on the compiler itself

set -euxo pipefail

# SET THE VARIABLES BELOW
BASE=876bd8cffa50fad089e4fc7774051906d4baa1d9
REVISION=ab63795a8dcf4221e5ac39f1914da6e88b7d54a4
TARGETDIR=../compiler-comparison


# we first build the new compiler
git clean -dfX
git checkout $REVISION
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"

make install
cp -R -f _install/ "$TARGETDIR/revision-compiler/"

# we turn to the base line compiler and build the normal version
git checkout $BASE

git clean -dfX
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make install
cp -R -f _install/ "$TARGETDIR/base-compiler-original/"


# we build a version with the new compiler
git clean -dfX
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"

make hacking-no-watch

cp -R -f "$TARGETDIR/revision-compiler/" _build/_bootinstall/
make install
cp -R -f _install/ "$TARGETDIR/base-compiler-revised/"

# # finally, we diff the resulting compilers
ASM_DIFF_DIR=$(mktemp -d)
objdump -dr $TARGETDIR/base-compiler-original/bin/ocamlopt.opt > $ASM_DIFF_DIR/base-original.s
objdump -dr $TARGETDIR/base-compiler-revised/bin/ocamlopt.opt > $ASM_DIFF_DIR/base-revised.s

diff $ASM_DIFF_DIR/base-original.s $ASM_DIFF_DIR/base-revised.s


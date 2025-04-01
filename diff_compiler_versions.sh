# Script to compare two versions of the compiler on the compiler itself

set -euxo pipefail

# SET THE VARIABLES BELOW
BASE=876bd8cffa50fad089e4fc7774051906d4baa1d9
REVISION=ab63795a8dcf4221e5ac39f1914da6e88b7d54a4
TARGETDIR="$(pwd)/../compiler-comparison"

CURDIR=$(pwd)
BUILDDIR=$(mktemp -d)
BASE_ORIGINAL_DIR="$BUILDDIR/base-original/"
BASE_REVISION_DIR="$BUILDDIR/base-revision/"
REVISION_DIR="$BUILDDIR/revision/"

# we make sure the target directory exists
mkdir -p $TARGETDIR
rm -rf $TARGETDIR/base-compiler-original/
rm -rf $TARGETDIR/base-compiler-revised/
rm -rf $TARGETDIR/revision-compiler/

# we copy the commits into the build directory
git archive --format=tar --prefix=base-original/ $BASE | (cd $BUILDDIR && tar xf -)
cp -r $BASE_ORIGINAL_DIR $BASE_REVISION_DIR
git archive --format=tar --prefix=revision/ $REVISION | (cd $BUILDDIR && tar xf -)


# we first build the new compiler
cd $REVISION_DIR
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make install
cp -R -f _install/ "$TARGETDIR/revision-compiler/"

# we turn to the base line compiler and build the normal version
cd $BASE_ORIGINAL_DIR
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make install
cp -R -f _install/ "$TARGETDIR/base-compiler-original/"


# we build a version with the new compiler
cd $BASE_REVISION_DIR
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make boot-compiler
cp -R -f "$TARGETDIR/revision-compiler/" _build/_bootinstall/

make install
cp -R -f _install/ "$TARGETDIR/base-compiler-revised/"

cd $CURDIR

# finally, we diff the resulting compilers
ASM_DIFF_DIR=$(mktemp -d)
objdump -dr $TARGETDIR/base-compiler-original/bin/ocamlopt.opt > $ASM_DIFF_DIR/base-original.s
objdump -dr $TARGETDIR/base-compiler-revised/bin/ocamlopt.opt > $ASM_DIFF_DIR/base-revised.s

diff $ASM_DIFF_DIR/base-original.s $ASM_DIFF_DIR/base-revised.s

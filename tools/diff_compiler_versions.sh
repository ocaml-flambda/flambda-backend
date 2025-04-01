# Script to compare two versions of the compiler on the compiler itself

if [ $# -ne 3 ]; then
    echo "Usage: $0 output-directory base-commit revision-commit"
    exit 1
fi


set -euxo pipefail

# SET THE VARIABLES BELOW
BASE=$2
REVISION=$3
TARGETDIR_REL_ABS=$1

# we make sure the target directory exists
mkdir -p $TARGETDIR_REL_ABS
TARGETDIR=$(realpath $TARGETDIR_REL_ABS)


BUILDDIR=$(mktemp -d)
BASE_ORIGINAL_DIR="$BUILDDIR/base-original/"
BASE_REVISION_DIR="$BUILDDIR/base-revision/"
REVISION_DIR="$BUILDDIR/revision/"

rm -rf $TARGETDIR/base-compiler-original/
rm -rf $TARGETDIR/base-compiler-revision/
mkdir -p $TARGETDIR/{base-compiler-original,base-compiler-revision}/{_build,_install}


# we copy the commits into the build directory
git archive --format=tar --prefix=base-original/ $BASE | (cd $BUILDDIR && tar xf -)
cp -r $BASE_ORIGINAL_DIR $BASE_REVISION_DIR
git archive --format=tar --prefix=revision/ $REVISION | (cd $BUILDDIR && tar xf -)


# we first build the bootcompiler of the new compiler
cd $REVISION_DIR
sed -i.bak "s/echo '(:standard \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/echo '(:standard -S \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/g" Makefile.common-jst && rm Makefile.common-jst.bak
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make boot-compiler

# we turn to the base line compiler and build the normal version
cd $BASE_ORIGINAL_DIR
sed -i.bak "s/echo '(:standard \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/echo '(:standard -S \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/g" Makefile.common-jst && rm Makefile.common-jst.bak
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make install
cp -R -f _install/ "$TARGETDIR/base-compiler-original/_install/"
cp -R -f _build/ "$TARGETDIR/base-compiler-original/_build/"


# we build a version with the new compiler
cd $BASE_REVISION_DIR
sed -i.bak "s/echo '(:standard \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/echo '(:standard -S \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/g" Makefile.common-jst && rm Makefile.common-jst.bak
autoconf
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make boot-compiler
# hack: we copy over the boot compiler from the revision compiler
# note: this can sometimes lead to a flaky error where dune tries to rebuild the compiler; in those cases simply run the script again
cp -L -R -f "$REVISION_DIR/_build/_bootinstall/bin/ocamlopt.opt" _build/_bootinstall/bin/
cp -L -R -f "$REVISION_DIR/_build/_bootinstall/bin/ocamlopt" _build/_bootinstall/bin/
make install
cp -R -f _install/ "$TARGETDIR/base-compiler-revision/_install/"
cp -R -f _build/ "$TARGETDIR/base-compiler-revision/_build/"

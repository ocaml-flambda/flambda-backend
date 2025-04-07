# Script to compare two versions of the compiler on the compiler itself

if [ $# -ne 3 ]; then
    echo "Usage: $0 <output-directory> <base-commit> <revision-commit>"
    echo ""
    echo "Additional configuration is possible via environment variables:"
    echo "  TMPDIR=<path>             # defaults to /tmp"
    echo "  AUTOCONF=<autoconf-path>  # defaults to autoconf"
    exit 1
fi

set -euxo pipefail


if [ ! -v AUTOCONF ] || [ -z $AUTOCONF ]; then
  AUTOCONF=autoconf
fi

if [ ! -v TMPDIR ] || [ -z $TMPDIR ]; then
  TMPDIR=/tmp
fi


# check that the git commits exist
if ! git rev-parse --verify "$2" &> /dev/null; then
  echo "Error: base commit $2 does not exist"
  exit 1
fi
if ! git rev-parse --verify "$3" &> /dev/null; then
  echo "Error: revision commit $3 does not exist"
  exit 1
fi


# we set the base commit, the revision commit, and the target directory
BASE=$2
REVISION=$3
TARGETDIR_REL_ABS=$1

# we make sure the target directory exists
mkdir -p $TARGETDIR_REL_ABS
TARGETDIR=$(realpath $TARGETDIR_REL_ABS)

if [ $TARGETDIR = "/" ]; then
  echo "Error: target directory cannot be root"
  exit 1
fi

if [ -d $TARGETDIR/base-compiler-original ] || [ -d $TARGETDIR/base-compiler-revision ]; then
    echo "The target directory already contains a compiler in base-compiler-original or base-compiler-revision."
    read -p "Are you sure you want to continue? This will erase the existing compilers. (y/n) " erase_existing_compilers
    if [[ $erase_existing_compilers == [yY] || $erase_existing_compilers == [yY][eE][sS] ]]; then
        echo "Erasing the existing compilers..."
        rm -rf $TARGETDIR/base-compiler-original/
        rm -rf $TARGETDIR/base-compiler-revision/
    else
        echo "Operation cancelled."
        exit 0
    fi
fi



BUILDDIR=$(mktemp -d)
if [ ! -d "$BUILDDIR" ]; then
    echo "Failed to create temporary directory $BUILDDIR"
    exit 1
fi
# only attach the trap once we know the directory exists
trap 'rm -rf "$BUILDDIR"' EXIT
BASE_ORIGINAL_DIR="$BUILDDIR/base-original/"
BASE_REVISION_DIR="$BUILDDIR/base-revision/"
REVISION_DIR="$BUILDDIR/revision/"


mkdir -p $TARGETDIR/{base-compiler-original,base-compiler-revision}/{_build,_install}


# we copy the commits into the build directory
git archive --format=tar --prefix=base-original/ $BASE | (cd $BUILDDIR && tar xf -)
cp -r $BASE_ORIGINAL_DIR $BASE_REVISION_DIR
git archive --format=tar --prefix=revision/ $REVISION | (cd $BUILDDIR && tar xf -)


# we first build the bootcompiler of the new compiler
cd $REVISION_DIR
sed -i.bak "s/echo '(:standard \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/echo '(:standard -S \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/g" Makefile.common-jst && rm Makefile.common-jst.bak
$AUTOCONF
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make boot-compiler

# we turn to the base line compiler and build the normal version
cd $BASE_ORIGINAL_DIR
sed -i.bak "s/echo '(:standard \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/echo '(:standard -S \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/g" Makefile.common-jst && rm Makefile.common-jst.bak
$AUTOCONF
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make install
cp -R -f _install/ "$TARGETDIR/base-compiler-original/_install/"
cp -R -f _build/ "$TARGETDIR/base-compiler-original/_build/"


# we build a version with the new compiler
cd $BASE_REVISION_DIR
sed -i.bak "s/echo '(:standard \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/echo '(:standard -S \$(if \$(filter true,\$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp/g" Makefile.common-jst && rm Makefile.common-jst.bak
$AUTOCONF
./configure --enable-ocamltest --enable-warn-error --prefix="$(pwd)/_install"
make boot-compiler
# hack: we copy over the boot compiler from the revision compiler
cp -L -R -f "$REVISION_DIR/_build/_bootinstall/bin/ocamlopt.opt" _build/_bootinstall/bin/
cp -L -R -f "$REVISION_DIR/_build/_bootinstall/bin/ocamlopt" _build/_bootinstall/bin/
# we update the make file to avoid rebuilding the boot compiler
sed -i.bak "s/runtime-stdlib: boot-compiler/runtime-stdlib: _build\/_bootinstall/g" Makefile.common-jst && rm Makefile.common-jst.bak
make install
cp -R -f _install/ "$TARGETDIR/base-compiler-revision/_install/"
cp -R -f _build/ "$TARGETDIR/base-compiler-revision/_build/"


# recommended versions to compare the output
#   compare the binaries:
#       diff <(objdump -dr "$TARGETDIR_REL_ABS/base-compiler-original/_install/bin/ocamlopt.opt") <(objdump -dr "$TARGETDIR_REL_ABS/base-compiler-revision/_install/bin/ocamlopt.opt")
#   compare the assembly files:
#       (cd $TARGETDIR_REL_ABS/base-compiler-original && (for i in $(find _build/ -name "*.s"); do diff -u -p  "$i" "../base-compiler-revision/$i"; done))

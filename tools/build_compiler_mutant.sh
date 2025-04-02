# Script to build two versions of the compiler

set -euxo pipefail


if [ $# -ne 3 ]; then
    echo "Usage: $0 output-directory base-commit revision-commit"
    exit 1
fi

# SET THE VARIABLES BELOW
BASE=$2
REVISION=$3
TARGETDIR_REL_ABS=$1

# we make sure the target directory exists
mkdir -p $TARGETDIR_REL_ABS
TARGETDIR=$(realpath $TARGETDIR_REL_ABS)


BUILDDIR=$(mktemp -d)
ORIGINAL_DIR="$BUILDDIR/original/"
REVISION_DIR="$BUILDDIR/revision/"


git archive --format=tar --prefix=original/ $BASE     | (cd $BUILDDIR && tar xf -)
git archive --format=tar --prefix=revision/ $REVISION | (cd $BUILDDIR && tar xf -)


# check whether the base compiler is already built
if [ ! -f "$TARGETDIR/original/commit.txt" ] || [ ! $(cat "$TARGETDIR/original/commit.txt") = $BASE ]
then
    echo Making the base compiler
    rm -rf $TARGETDIR/original/{_bootinstall,_install,commit.txt}
    mkdir -p $TARGETDIR/original/{_bootinstall,_install}
    cd $ORIGINAL_DIR
    autoconf
    ./configure --enable-ocamltest --enable-warn-error --prefix="$TARGETDIR/original/_install"
    make boot-compiler
    cp -R -L -f _build/_bootinstall/ "$TARGETDIR/original/_bootinstall/"
    make install
    echo $BASE > "$TARGETDIR/original/commit.txt"
fi


if [ ! -f "$TARGETDIR/revision/commit.txt" ] || [ ! $(cat "$TARGETDIR/revision/commit.txt") = $REVISION ]
then
    echo Making the revision compiler
    rm -rf $TARGETDIR/revision/{_install,commit.txt}
    mkdir -p $TARGETDIR/revision/_install
    cd $REVISION_DIR
    autoconf
    ./configure --enable-ocamltest --enable-warn-error --prefix="$TARGETDIR/revision/_install"
    make boot-compiler
    # ensure the boot compiler is not rebuilt to avoid dune caching issues
    sed -i.bak "s/runtime-stdlib: boot-compiler/runtime-stdlib: _build\/_bootinstall/g" Makefile.common-jst && rm Makefile.common-jst.bak
    cp -R -L -f "$TARGETDIR/original/_bootinstall/" _build/_bootinstall/
    make install
    echo $BASE > "$TARGETDIR/revision/commit.txt"
fi



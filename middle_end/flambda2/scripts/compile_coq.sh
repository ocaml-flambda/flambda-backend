#!/bin/bash -e

help() {
  echo "./compile-coq <compiler_path> <path_to_directory> [--opam opam] [-c|--compile-opam] [-r|--remove]"
  echo "[compiler_path]     is the path to the repository to test. It uses the files
                            as in and does not the head of the git branch."
  echo "[path_to_directory] is the path to the drectory where eveything will be build."
  echo "                    This script considers that it is free to do anything from"
  echo "                    this directory, including deleting it."
  echo "                    Omitting this entry will result in using a temp dir."
  echo "[-c|--compile-opam] Compile and use the latest version of opam."
  echo "[--opam opam]       [opam] will be used as the opam command to execute."
  echo "                    Useful to specify a specific opam binary."
  echo "[-r|--remove]       Remove the destination directory after coq is compiled."
}

#
# Argument parsing
#

POSITIONAL=()
while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  -c|--compile-opam)
    COMPILE_OPAM=true
    shift # past argument
    ;;
  --opam)
    COMPILE_OPAM=false
    OPAM="$2"
    shift # past argument
    shift # past value
    ;;
  -h|--help)
    help
    exit 1
    ;;
  -r|--remove)
    REMOVE=true
    shift # past argument
    ;;
  *)    # unknown option
    POSITIONAL+=("$1") # save it in an array for later
    shift # past argument
    ;;
esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

if [ "$#" -le 0 ]
then
  help
  exit 0
elif [ "$#" -le 1 ]
then
  COMPILER_TO_TEST=$(realpath $1)
  CWD=$(mktemp -d)
else
  COMPILER_TO_TEST=$(realpath $1)
  CWD=$(realpath $2)
fi

# Setup the working environment
mkdir -p $CWD
OCAML_PATH=$CWD/ocaml
# Export a custom OPAMROOT not to interfer with the user's existing opam
# environment. All subsequent opam invocations will be done from an environment
# setup in $CWD/.opam.
export OPAMROOT=$CWD/.opam
rm -rdf $OPAMROOT

# If requested remove the working dir on the exit
remove_dir_trap() {
  if [ "$REMOVE" = true ] ; then
    rm -rfd $CWD
  fi
}

trap "remove_dir_trap" EXIT

# Compile opam if needed
if [ "$COMPILE_OPAM" = true ] ; then
  OPAM_PATH=$CWD/opam
  OPAM_INSTALL=$OPAM_PATH/_install
  OPAM=$OPAM_INSTALL/bin/opam

  git clone https://github.com/ocaml/opam $OPAM_PATH
  pushd $OPAM_PATH

  ./configure --prefix=$OPAM_INSTALL
  make lib-ext
  make
  make install
  popd
elif [ -z "${OPAM}" ] ; then
  OPAM=opam
else
  true
fi

echo "Using opam $OPAM"

# Copy the compiler to the working directory
rm -rdf $OCAML_PATH
cp -r $COMPILER_TO_TEST $OCAML_PATH

$OPAM init -n --disable-sandboxing --bare

pushd $OCAML_PATH
# Remove pre-existing compilation artifacts
make clean

$OPAM switch create . --empty --repositories=default,beta=git+https://github.com/ocaml/ocaml-beta-repository.git

# Once the switch is set try to compile coq. On failure remove the switch.
trap "$OPAM switch remove $CWD/ocaml -y; remove_dir_trap" EXIT
$OPAM install  . --inplace-build -y \
&& $OPAM pin add  coq git+https://github.com/ocaml-flambda/coq.git\#flambda2-patches -y 

#!/bin/sh -e

# ============================================================================
# ======= VARIABLES ==========================================================
# ============================================================================

# ROOT of the git repo
GITROOT=$(git rev-parse --show-toplevel)

# TEMPORARY file to store the list of tests to run
trap 'rm -f "${LIST_FILE}"' EXIT
LIST_FILE=$(mktemp) || exit 1

# Variable to decide what we are testing, values ares:
# - false (default), test all compilers
# - true           , only test ocamlc.byte and ocamlopt.byte
#                    (this is done using by patching ocamltest's sources)
ONLY_BYTE=false


# ============================================================================
# ======= SCRIPTS ARGUMENTS ==================================================
# ============================================================================

function usage {
    echo "usage: ./run_testsuite.sh {command}"
    echo "where {command} can be one of:"
    echo "  all      : run the testuite using all compilers"
    echo "  byte     : run the testuite using only ocamlc.byte and ocamlopt.byte"
    echo "  help     : display this message"
}

case $1 in
  all)
    ONLY_BYTE=false
    ;;
  byte)
    ONLY_BYTE=true
    ;;
  help)
    usage
    exit 0
    ;;
  *)
    usage
    exit 1
    ;;
esac


# ============================================================================
# ======= THE SCRIPT ITSELF ==================================================
# ============================================================================

# If running the testsuite for bytecode compilers
# (i.e. ocamlc.byte, ocamlopt.byte), then apply the relevant patch
# and rebuild ocamltest
if [ "${ONLY_BYTE}" = true ]; then

  # Go to the root (simpler for applying patches,
  # though probably not strictly necessary).
  cd ${GITROOT};

  # Apply the patch to ocamltest
  git apply ${GITROOT}/.github/workflows/ocamltest_makefile.patch ||
    echo "To only test ocamlc.byte and ocamlopt.byte, this script needs\
          to apply a git patch, please commit or stash any current changes\
          to ensure this can happen cleanly";

  # Unapply the patch when the script exits (even under abnormal cirumstances)
  trap 'cd ${GITROOT} && \
        git apply -R ${GITROOT}/.github/workflows/ocamltest_makefile.patch' EXIT

  # Rebuild ocamltest
  make ocamltest;

fi

# Get into the testsuite
cd ${GITROOT}/testsuite

# Generate the lists of tests
grep -v '#' ../.github/workflows/test-list > ${LIST_FILE}

# Run the testsuite
make list-parallel FILE=${LIST_FILE}


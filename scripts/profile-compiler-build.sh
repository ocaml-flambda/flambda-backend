#!/bin/sh

# To be run from the root of the Flambda backend repo

set -e -u -o pipefail

dump_dir="`pwd`/_profile"
summary_path="`pwd`/summary.csv"

if [ -d "$dump_dir" ] && [ "$(ls -A "$dump_dir")" ]; then
  echo "$dump_dir is not empty."
  while true; do
    read -p "Do you want to clear the directory $dump_dir? (y/n): " choice
    case "$choice" in
      y|Y )
        rm -rf "$dump_dir"
        echo "$dump_dir cleared."
        break
        ;;
      n|N )
        exit 1
        ;;
      * )
        echo "Invalid choice. Please enter 'y' or 'n'."
        ;;
    esac
  done
fi

export OLD_OCAMLPARAM="${OCAMLPARAM:-}"
export OCAMLPARAM="_,profile=1,dump-into-csv=1,dump-dir=$dump_dir,regalloc=irc"
export BUILD_OCAMLPARAM="$OCAMLPARAM"

revert_env_variables() {
  export OCAMLPARAM="$OLD_OCAMLPARAM"
  unset OLD_OCAMLPARAM
}
trap revert_env_variables EXIT

build_compiler() {
  git clean -Xdf
  autoconf
  ./configure --enable-ocamltest --enable-warn-error --enable-dev --prefix=`pwd`/_install
  make install
}

build_compiler
python3 ./scripts/combine-profile-information.py "$dump_dir" -o "$summary_path"

rm "$dump_dir/"*.csv
rmdir "$dump_dir"

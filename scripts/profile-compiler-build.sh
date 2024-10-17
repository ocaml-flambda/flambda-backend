#!/bin/sh

set -e -u -o pipefail

# Works regardless of where script run from
scripts_dir=$( cd "$( dirname "$0" )" &> /dev/null && pwd )

root="$scripts_dir"/..
dump_dir="$root"/_profile
summary_path="$root"/summary.csv

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

export OCAMLPARAM="_,profile=1,dump-into-csv=1,dump-dir=$dump_dir,regalloc=irc"
export BUILD_OCAMLPARAM="$OCAMLPARAM"

build_compiler() {
  git clean -Xdf
  autoconf
  ./configure --enable-ocamltest --enable-warn-error --enable-dev --prefix="$(pwd)"/_install
  make install
}

if [ -v TMPDIR ] && [ ! -z "$TMPDIR" ]; then
  temp_dir=$(mktemp -d)
else
  temp_dir=$(mktemp -d -p "$root"/.. -t tmp.XXXXXXX)
fi
trap "rm -rf "$temp_dir"" EXIT
cp -r --reflink=auto "$root" "$temp_dir"

cd "$temp_dir"
build_compiler

cd "$root"
python3 "$scripts_dir"/combine-profile-information.py "$dump_dir" -o "$summary_path"

rm "$dump_dir/"*.csv
rmdir "$dump_dir"

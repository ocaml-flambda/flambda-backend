#!/bin/sh
# Merge static library (.a) files into one.

set -eu

if [ "$#" -lt 2 ]; then
  echo "syntax: $0 TARGET-DOT-A-FILE INPUT-DOT-A-FILE [...]"
  exit 1
fi

absolute_path () {
  path=$1
  dir=$(dirname $path)
  dir_abs=$(cd $dir && pwd)
  file=$(basename $path)
  echo $dir_abs/$file
}

target=$1
shift
archives=$*

target=$(absolute_path $target)

if [ "$(uname)" = "Darwin" ]; then
  exec libtool -static -o $target $archives
fi

files=$(mktemp)
tempdir=$(mktemp -d)

trap "rm -rf $tempdir $files" EXIT

for archive in $archives; do
  archive=$(absolute_path $archive)
  ar t $archive | grep '\.o$' >> $files
  (cd $tempdir && ar x $archive)
done

cd $tempdir
rm -f $target

ar rD $target $(cat $files)

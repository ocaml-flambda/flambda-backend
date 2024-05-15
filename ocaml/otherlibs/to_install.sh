#!/bin/bash

name=$1

echo '('

for path in ./*.{cmxa,a,cmxs,cma,mli}\
    .$name.objs/byte/*.{cmi,cmt,cmti}\
    .$name.objs/native/*.cmx; do
    file=$(basename $path)
    echo " ($path as $name/$file)"
done

echo ')'

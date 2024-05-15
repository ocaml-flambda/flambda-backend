#!/bin/bash

libname=$1

echo '('
echo " ($libname.cmxa as $libname/$libname.cmxa)"
echo " ($libname.a as $libname/$libname.a)"
echo " ($libname.cmxs as $libname/$libname.cmxs)"
echo " ($libname.cma as $libname/$libname.cma)"

for file in ./*.mli ; do
    module="${file%.*}"
    echo " ($module.mli as $libname/$module.mli)"
    echo " (.$libname.objs/byte/$module.cmi as $libname/$module.cmi)"
    echo " (.$libname.objs/byte/$module.cmt as $libname/$module.cmt)"
    echo " (.$libname.objs/byte/$module.cmti as $libname/$module.cmti)"
    echo " (.$libname.objs/native/$module.cmx as $libname/$module.cmx)"
done

echo ')'

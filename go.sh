#!/bin/bash

function try () {
  export SEQUENCE=$1
  echo "Trying: $SEQUENCE"
  sed -i 's/fooab/fooabb/' backend/cmm_helpers.ml
  make install >>/tmp/log 2>&1
  nroberts-out/bin/ocamlopt >>/tmp/log 2>&1
}

function recur () {
  echo "So far: $1"
  if try "$11"; then
    if try "$10"; then
      echo "FAILED! $1"
    else
      recur "$10"
    fi
  else
    recur "$11"
  fi
}

rm /tmp/log
recur ""

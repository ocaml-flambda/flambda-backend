#!/usr/bin/env bash

libnames=(${LIBNAMES-})

function construct_command {
    local bin="$1"
    shift
    printf "%q" "${bin}"
    standard_library="${STDLIB:-$("${bin}" -config-var standard_library)}" || return 1
    for libname in "${libnames[@]}"; do
        printf ' -I %q' "${standard_library}/${libname}"
        printf ' %q' "${standard_library}/${libname}/${libname}.cmxa"
    done
    for arg in "$@"; do
        printf ' %q' "${arg}"
    done
    echo
}

bin=ocamlopt.opt
dev="$(realpath -- "${DEV-_install/bin/${bin}}")" || exit 1
prod="${PROD-$(which "${bin}")}" || exit 1
prod="$(STDLIB="${PROD_STDLIB}" construct_command "${prod}" "$@" ${PROD_FLAGS- -o prod})" || exit 1
dev="$(STDLIB="${DEV_STDLIB}" construct_command "${dev}" "$@" ${DEV_FLAGS- -o dev})" || exit 1

exit_code=0

printf '%s\n' "${prod}" >&2
eval "${prod}" || exit_code=1

printf '%s\n' "${dev}" >&2
eval "${dev}" || exit_code=1

exit ${exit_code}

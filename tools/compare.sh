#!/usr/bin/env bash
set -e -u -o pipefail

# Use this script to compare the output of an existing compiler
# with one installed to _install
#
# E.g.,
# make boot-_install
# export OLD='opam exec --switch=MY_OLD_SWITCH ocamlopt.opt --'
# tools/compare.sh testsuite/tests/lib-int/test.ml -c -dcmm -dump-into-file
# diff {old,new}.cmx.dump

# Set these to add paths under the stdlib to the compiler arguments
read -a LIB <<<"${LIB-}"
read -a INCLUDE <<<"${INCLUDE-}"

bin=ocamlopt.opt

# override OLD or NEW to set what is executed as the compiler
if [[ -z ${OLD+x} ]]; then
    OLD="$(type -P "${bin}")" || exit 1
    printf -v OLD '%q' "${OLD}"
fi

if [[ -z ${NEW+x} ]]; then
    NEW="$(realpath -- "_install/bin/${bin}")" || exit 1
    printf -v NEW '%q' "${NEW}"
fi

print_quoted_command () {
    printf '%q' "$1"
    shift

    while [[ $# -gt 0 ]]; do
        printf ' %q' "$1"
        shift
    done
}

# Usage:
# construct_command "OCAML BINARY" [ARGS...]
construct_command() {
    local cmd

    # Set the binary as the first part of the command
    read -a cmd <<<"$1"
    shift

    if [[ ${#cmd[@]} -eq 0 ]]; then
        echo 'no compiler specified' >&2
        return 1
    fi

    local stdlib="$1"
    shift

    if [[ ${#LIB[@]} -gt 0 || ${#INCLUDE[@]} -gt 0 ]]; then
        if [[ -z ${stdlib-} ]]; then
            stdlib="$("${cmd[@]}" -config-var standard_library)" || return 1
        fi

        local include
        for include in "${INCLUDE[@]}"; do
            cmd+=( "${stdlib}/${include}/${include}.cmxa" )
        done

        local lib
        for lib in "${LIB[@]}"; do
            cmd+=( '-I' "${stdlib}/${lib}" )
        done
    fi

    cmd+=( "$@" )
    print_quoted_command "${cmd[@]}"
}

old="$(construct_command "${OLD}" "${OLD_STDLIB-}" -o old "${@}")" || exit 1
new="$(construct_command "${NEW}" "${NEW_STDLIB-}" -o new "${@}")" || exit 1

exit_code=0

printf '%s\n' "${old}" >&2
eval "${old}" || exit_code=1

printf '%s\n' "${new}" >&2
eval "${new}" || exit_code=1

exit "${exit_code}"

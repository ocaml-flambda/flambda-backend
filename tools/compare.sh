#!/usr/bin/env bash

# we want `read` to respect backslashes
# shellcheck disable=SC2162
set -u -o pipefail

# Use this script to compare the output of an existing compiler
# with one installed to _install
#
# E.g.,
# autoconf
# ./configure --prefix="$(pwd)/_install"
# make boot-_install
# export OLD='opam exec --switch=MY_OLD_SWITCH ocamlopt.opt --'
# tools/compare.sh testsuite/tests/lib-int/test.ml -c -dcmm
# diff {old,new}.stderr

# Set these to add paths under the stdlib to the compiler arguments
read -a LIB <<< "${LIB-}"
read -a INCLUDE <<< "${INCLUDE-}"

fail() {
    [[ $# -eq 0 ]] || printf '%s: %s\n' "$0" "$*" >&2
    exit 1
}

quote_array() {
    while [[ $# -gt 1 ]]; do
        printf '%q ' "$1"
        shift
    done

    printf '%q' "$1"
}


[[ -n ${BIN-} ]] || BIN=ocamlopt.opt

if [[ -z ${LIB_SUFFIX-} ]]; then
    case "${BIN}" in
    ocamlc*) LIB_SUFFIX='.cma' ;;
    ocamlopt*) LIB_SUFFIX='.cmxa' ;;
    *) fail "Unrecognized command ${BIN}, expected ocamlc or ocamlopt" ;;
    esac
fi

# override OLD or NEW to set what is executed as the compiler

#usage: parse_compiler VAR_NAME DEFAULT_TO_EVAL
parse_compiler() {
    local parsed
    if [[ -n ${!1-} ]]; then
        # break the command up by spaces
        read -a parsed <<< "${!1}"
        if [[ ${#parsed[@]} -eq 0 ]]; then
            fail "no $1 compiler specified"
        elif [[ ${#parsed[@]} -eq 1 && -d ${parsed[0]} ]]; then
             # automagically add BIN if we've specified a directory
            parsed=( "${parsed[0]}/${BIN}" )
        fi
    else
        parsed=("$(type -P "${BIN}")") || fail
    fi

    # overwrite VAR_NAME with the parsed version
    declare -a -g -n dst="$1"
    dst=( "${parsed[@]}" )
}


if [[ -n ${OLD-} ]]; then
    # break the command up by spaces
    read -a OLD <<< "${OLD}"
    [[ ${#OLD[@]} -gt 0 ]] || fail "no OLD compiler specified"

    # automagically add BIN if we've specified a directory
    if [[ ${#OLD[@]} -eq 1 && -d ${OLD[0]} ]]; then
        OLD=( "${OLD[0]}/${BIN}" )
    fi
else
    OLD=("$(type -P "${BIN}")") || fail
fi

if [[ -n ${NEW-} ]]; then
    # break the command up by spaces
    read -a NEW <<< "${NEW}"
    [[ ${#NEW[@]} -gt 0 ]] || fail "no NEW compiler specified"

    if [[ ${#NEW[@]} -eq 1 && -d ${NEW[0]} ]]; then
        NEW=( "${NEW[0]}/${BIN}" )
    fi
else
    NEW=("$(realpath -- "${BASH_SOURCE[0]%/*}/../_install/bin/${BIN}")") || fail
fi

# Usage:
# COMPILER=( ... ) STDLIB=... construct_command [ARGS...]
construct_command() {
    declare -a args

    if [[ ${#LIB[@]} -gt 0 || ${#INCLUDE[@]} -gt 0 ]]; then
        if [[ -z ${STDLIB-} ]]; then
            STDLIB="$("${COMPILER[@]}" -config-var standard_library)" || fail
        fi

        local include
        for include in "${INCLUDE[@]}"; do
            args+=("${STDLIB}/${include}/${include}${LIB_SUFFIX}")
        done

        local lib
        for lib in "${LIB[@]}"; do
            args+=('-I' "${STDLIB}/${lib}")
        done
    fi

    quote_array "${COMPILER[@]}" "${args[@]}" "$@"
}

old="$(
    COMPILER=("${OLD[@]}")
    STDLIB="${OLD_STDLIB-}"
    construct_command "${@}"
)" || fail
new="$(
    COMPILER=("${NEW[@]}")
    STDLIB="${NEW_STDLIB-}"
    construct_command "${@}"
)" || fail

verbose_eval_var() {
    local exit_code
    cmd="${!1} >${1}.stdout 2>${1}.stderr"
    printf '%s\n' "${cmd}" >&2
    eval "${cmd}"
    exit_code=$?
    if [[ ${exit_code} -ne 0 ]]; then
        cat "${1}.stderr" >&2
        printf 'Exited with code %d: %s\n' "${exit_code}" "${cmd}" >&2
    fi
    return "${exit_code}"
}

verbose_eval_var old
old_exit="$?"

verbose_eval_var new
new_exit="$?"

if [[ ${old_exit} -ne ${new_exit} ]]; then
    exit 1
else
    exit "${old_exit}"
fi
